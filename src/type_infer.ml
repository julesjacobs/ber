open Ast
open Location
open Type_solver

module SMap = Map.Make (String)

let dummy_loc = Location.span Lexing.dummy_pos Lexing.dummy_pos

let ( let* ) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e

let ( >>= ) r f =
  match r with
  | Ok v -> f v
  | Error msg -> Error msg

type value_env = scheme SMap.t

type type_info =
  { params : string list
  ; ctors : (string * scheme) list
  }

type type_env = type_info SMap.t

type env =
  { vars : value_env
  ; types : type_env
  ; gen_level : int
  }

type type_error =
  { loc : Location.t
  ; kind : type_error_kind
  }

and type_error_kind =
  | Type_mismatch of ty * ty
  | Occurs_check of tvar * ty
  | Message of string

let error_msg loc message = Error { loc; kind = Message message }

let rec map_result f = function
  | [] -> Ok []
  | x :: xs ->
    let* y = f x in
    let* ys = map_result f xs in
    Ok (y :: ys)

let rec map_result2 f xs ys =
  match xs, ys with
  | [], [] -> Ok []
  | x :: xs', y :: ys' ->
    let* z = f x y in
    let* zs = map_result2 f xs' ys' in
    Ok (z :: zs)
  | _ -> error_msg dummy_loc "length mismatch"

type binding_info =
  { name : string
  ; scheme : scheme
  }

type toplevel_info =
  | InfoLet of binding_info list * Location.t
  | InfoExpr of scheme * Location.t
  | InfoType of Location.t

let generalize env ty = Type_solver.generalize ~level:env.gen_level ty
let instantiate ?loc env sch = Type_solver.instantiate ?loc ~level:env.gen_level sch

let push_level env = { env with gen_level = env.gen_level + 1 }

let initial_env =
  { vars = SMap.empty
  ; types = SMap.empty
  ; gen_level = 1
  }

let unify_types loc ~got ~expected =
  match Type_solver.unify got expected with
  | Ok () -> Ok ()
  | Error (Type_solver.Type_mismatch (a, b)) -> Error { loc; kind = Type_mismatch (a, b) }
  | Error (Type_solver.Occurs (tv, ty)) -> Error { loc; kind = Occurs_check (tv, ty) }

let string_of_ty = Type_solver.string_of_ty
let string_of_scheme = Type_solver.string_of_scheme

let lookup_type env name loc =
  match SMap.find_opt name env.types with
  | Some info -> Ok info
  | None ->
    (match name with
     | "int" | "bool" | "string" -> Ok { params = []; ctors = [] }
     | _ -> error_msg loc ("Unknown type " ^ name))

let rec ty_of_type_expr env (tyvars : (string * tvar) list ref) (te : type_expr) =
  let lookup name = List.assoc_opt name !tyvars in
  match te.node with
  | TyVar id ->
    (match lookup id.node with
     | Some tv -> Ok (TVar tv)
     | None ->
       let tv = fresh_tvar env.gen_level in
       tyvars := (id.node, tv) :: !tyvars;
       Ok (TVar tv))
  | TyConstr (name, args) ->
    let* type_info = lookup_type env name.node te.loc in
    if List.length args <> List.length type_info.params then
      error_msg te.loc ("type " ^ name.node ^ " expects " ^ string_of_int (List.length type_info.params) ^ " arguments")
    else
      let* args' = map_result (ty_of_type_expr env tyvars) args in
      Ok (mk_con ~loc:te.loc name.node args')
  | TyArrow (a, b) ->
    let* ta = ty_of_type_expr env tyvars a in
    let* tb = ty_of_type_expr env tyvars b in
    Ok (t_arrow ~loc:te.loc ta tb)
  | TyTuple elems ->
    let* elems' = map_result (ty_of_type_expr env tyvars) elems in
    Ok (t_tuple ~loc:te.loc elems')

let rec infer_pattern env pat expected =
  match pat.node with
  | PWildcard -> Ok []
  | PVar id ->
    Ok [ id.node, expected ]
  | PAs (p, id) ->
    let* binds = infer_pattern env p expected in
    Ok ((id.node, expected) :: binds)
  | PInt _ ->
    let* () = unify_types pat.loc ~got:(t_int ~loc:pat.loc ()) ~expected in
    Ok []
  | PBool _ ->
    let* () = unify_types pat.loc ~got:(t_bool ~loc:pat.loc ()) ~expected in
    Ok []
  | PString _ ->
    let* () = unify_types pat.loc ~got:(t_string ~loc:pat.loc ()) ~expected in
    Ok []
  | PTuple elems ->
    let ts = List.map (fun _ -> fresh_ty env.gen_level) elems in
    let tuple_ty = t_tuple ~loc:pat.loc ts in
    let* () = unify_types pat.loc ~got:tuple_ty ~expected in
    let rec loop acc pats tys =
      match pats, tys with
      | [], [] -> Ok (List.concat (List.rev acc))
      | p :: ps, t :: ts' ->
        let* b = infer_pattern env p t in
        loop (b :: acc) ps ts'
      | _ -> error_msg pat.loc "Tuple arity mismatch in pattern"
    in
    loop [] elems ts
  | PConstr (ctor, args) ->
    (match SMap.find_opt ctor.node env.vars with
     | Some scheme ->
       let ctor_ty = instantiate ~loc:ctor.loc env scheme in
       let rec collect ty acc =
         match prune ty with
         | TCon ("->", [ a; b ], _) -> collect b (a :: acc)
         | result -> List.rev acc, result
       in
       let arg_tys, res_ty = collect ctor_ty [] in
       if List.length arg_tys <> List.length args then
         error_msg pat.loc "Constructor arity mismatch"
       else (
         let* () = unify_types pat.loc ~got:res_ty ~expected in
         let rec loop acc pats tys =
           match pats, tys with
           | [], [] -> Ok (List.concat (List.rev acc))
           | p :: ps, t :: ts' ->
             let* b = infer_pattern env p t in
             loop (b :: acc) ps ts'
           | _ -> error_msg pat.loc "Constructor arity mismatch"
         in
         loop [] args arg_tys)
     | None -> error_msg pat.loc ("Unknown constructor " ^ ctor.node))
  | PAnnot (p, texpr) ->
    let tyvars = ref [] in
    let* t = ty_of_type_expr env tyvars texpr in
    let* () = unify_types pat.loc ~got:t ~expected in
    infer_pattern env p t

let rec infer_expr env expr =
  let expected = fresh_ty env.gen_level in
  let* () = check_expr env expected expr in
  Ok expected
and check_expr env expected expr =
  match expr.node with
  | EInt _ ->
    let* () = unify_types expr.loc ~got:(t_int ~loc:expr.loc ()) ~expected in
    Ok ()
  | EBool _ ->
    let* () = unify_types expr.loc ~got:(t_bool ~loc:expr.loc ()) ~expected in
    Ok ()
  | EString _ ->
    let* () = unify_types expr.loc ~got:(t_string ~loc:expr.loc ()) ~expected in
    Ok ()
  | EVar id ->
    (match SMap.find_opt id.node env.vars with
     | None -> error_msg expr.loc ("Unbound variable " ^ id.node)
     | Some s ->
       let t = instantiate ~loc:id.loc env s in
       let* () = unify_types expr.loc ~got:t ~expected in
       Ok ())
  | EConstr (ctor, args) ->
    (match SMap.find_opt ctor.node env.vars with
     | None -> error_msg expr.loc ("Unknown constructor " ^ ctor.node)
     | Some scheme ->
       let ctor_ty = instantiate ~loc:ctor.loc env scheme in
       let rec collect ty acc =
         match prune ty with
         | TCon ("->", [ a; b ], _) -> collect b (a :: acc)
         | result -> List.rev acc, result
       in
       let arg_tys, res_ty = collect ctor_ty [] in
        if List.length arg_tys <> List.length args then
          error_msg expr.loc "Constructor arity mismatch"
        else
            let* () = unify_types expr.loc ~got:res_ty ~expected in
            let* _ =
              map_result2
                (fun e t ->
                    let* () = check_expr env t e in
                    Ok t)
                args
                arg_tys
            in
            Ok ())
  | ETuple elems ->
    let elem_tys = List.map (fun _ -> fresh_ty env.gen_level) elems in
    let tuple_ty = t_tuple ~loc:expr.loc elem_tys in
    let* () = unify_types expr.loc ~got:tuple_ty ~expected in
    let* _ =
      map_result2
        (fun e t ->
           let* () = check_expr env t e in
           Ok t)
        elems
        elem_tys
    in
    Ok ()
  | ELambda { params; fn_body } ->
    let env' = push_level env in
    let param_tys = List.map (fun _ -> fresh_ty env'.gen_level) params in
    let* binds =
      let rec loop acc pats tys =
        match pats, tys with
        | [], [] -> Ok (List.concat (List.rev acc))
        | p :: ps, t :: ts' ->
          let* b = infer_pattern env' p t in
          loop (b :: acc) ps ts'
        | _ -> error_msg expr.loc "arity mismatch in lambda parameters"
      in
      loop [] params param_tys
    in
    let env'' =
      List.fold_left
        (fun e (name, ty) ->
           { e with vars = SMap.add name (Forall ([], ty)) e.vars })
        env'
        binds
    in
    let result_ty = fresh_ty env'.gen_level in
    let fn_ty =
      List.fold_right (fun arg acc -> t_arrow ~loc:expr.loc arg acc) param_tys result_ty
    in
    let* () = check_expr env'' result_ty fn_body in
    let* () = unify_types expr.loc ~got:fn_ty ~expected in
    Ok ()
  | EApp (fn, args) ->
    let result_ty = expected in
    let arg_tys = List.map (fun _ -> fresh_ty env.gen_level) args in
    let app_ty =
      List.fold_right (fun arg acc -> t_arrow ~loc:expr.loc arg acc) arg_tys result_ty
    in
    let* () = check_expr env app_ty fn in
    let* _ =
      map_result2
        (fun e t ->
           let* () = check_expr env t e in
           Ok t)
        args
        arg_tys
    in
    Ok ()
  | ELet { rec_flag; bindings; in_expr } ->
    let* env_after, _infos = infer_let_bindings env rec_flag bindings in
    let* () = check_expr env_after expected in_expr in
    Ok ()
  | EMatch (scrut, cases) ->
    let scrut_ty = fresh_ty env.gen_level in
    let* () = check_expr env scrut_ty scrut in
    let env_case_level = push_level env in
    let* () =
      let rec loop = function
        | [] -> Ok ()
        | c :: rest ->
          let* binds = infer_pattern env_case_level c.node.pattern scrut_ty in
          let env_with_binds =
            List.fold_left
              (fun e (name, ty) -> { e with vars = SMap.add name (Forall ([], ty)) e.vars })
              env_case_level
              binds
          in
          (match c.node.guard with
           | None -> Ok ()
           | Some g ->
             let* () = check_expr env_with_binds (t_bool ~loc:g.loc ()) g in
             Ok ()) >>= fun () ->
          let* () = check_expr env_with_binds expected c.node.body in
          loop rest
      in
      loop cases
    in
    Ok ()
  | EAnnot (e, texpr) ->
    let tyvars = ref [] in
    let* t_expected = ty_of_type_expr env tyvars texpr in
    let* () = check_expr env t_expected e in
    let* () = unify_types texpr.loc ~got:t_expected ~expected in
    Ok ()

and infer_let_bindings env rec_flag bindings =
  let env_level = push_level env in
  match rec_flag with
  | Nonrecursive ->
    let rec loop env_acc infos_rev = function
      | [] -> Ok (env_acc, List.rev infos_rev)
      | b :: rest ->
        let rhs_ty = fresh_ty env_level.gen_level in
        let* () = check_expr env_level rhs_ty b.node.rhs in
        let* binds = infer_pattern env_level b.node.lhs rhs_ty in
        let generalized =
          List.map
            (fun (name, ty) -> { name; scheme = generalize env ty })
            binds
        in
        let env_acc =
          List.fold_left
            (fun e info -> { e with vars = SMap.add info.name info.scheme e.vars })
            env_acc
            generalized
        in
        loop env_acc (List.rev_append generalized infos_rev) rest
    in
    loop env [] bindings
  | Recursive ->
    let* names =
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | b :: rest ->
          (match b.node.lhs.node with
           | PVar id -> collect (id.node :: acc) rest
           | _ -> error_msg b.loc "let rec requires variable patterns")
      in
      collect [] bindings
    in
    let provisional =
      List.map (fun name -> name, fresh_ty env_level.gen_level) names
    in
    let env_with_prov =
      List.fold_left
        (fun e (name, ty) -> { e with vars = SMap.add name (Forall ([], ty)) e.vars })
        env_level
        provisional
    in
    let* () =
      let rec loop bs provs =
        match bs, provs with
        | [], [] -> Ok ()
        | b :: bs', (_, ty) :: provs' ->
          let* () = check_expr env_with_prov ty b.node.rhs in
          loop bs' provs'
        | _ -> error_msg dummy_loc "Arity mismatch in let rec bindings"
      in
      loop bindings provisional
    in
    let infos =
      List.map (fun (name, ty) -> { name; scheme = generalize env ty }) provisional
    in
    let env_after =
      List.fold_left
        (fun e info -> { e with vars = SMap.add info.name info.scheme e.vars })
        env
        infos
    in
    Ok (env_after, infos)
let register_type_decl env (decl : type_decl) =
  let params = List.map (fun id -> id.node) decl.node.params in
  let param_vars = List.map (fun _ -> fresh_tvar (env.gen_level + 1)) params in
  let param_env = ref [] in
  List.iter2 (fun name tv -> param_env := (name, tv) :: !param_env) params param_vars;
  let type_body = mk_con ~loc:decl.loc decl.node.tname.node (List.map (fun tv -> TVar tv) param_vars) in
  let pre_type_info = { params; ctors = [] } in
  let env_with_type = { env with types = SMap.add decl.node.tname.node pre_type_info env.types } in
  let* ctor_schemes =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | ctor :: rest ->
        let* args =
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | a :: tail ->
              let* t = ty_of_type_expr env_with_type param_env a in
              collect (t :: acc) tail
          in
          collect [] ctor.node.args
        in
        let ty =
          List.fold_right
            (fun arg acc -> t_arrow ~loc:ctor.loc arg acc)
            args
            type_body
        in
        let sch = generalize env ty in
        loop ((ctor.node.cname.node, sch) :: acc) rest
    in
    loop [] decl.node.constructors
  in
  let type_info = { params; ctors = ctor_schemes } in
  let types = SMap.add decl.node.tname.node type_info env_with_type.types in
  let vars =
    List.fold_left
      (fun v (name, sch) -> SMap.add name sch v)
      env_with_type.vars ctor_schemes
  in
  Ok { env_with_type with types; vars }

let infer_toplevel env (tl : toplevel) =
  match tl.node with
  | TType decl ->
    (match register_type_decl env decl with
     | Ok env' -> Ok (env', InfoType tl.loc)
     | Error err -> Error err)
  | TExpr e ->
    (match infer_expr env e with
     | Ok ty ->
       let sch = generalize env ty in
       Ok (env, InfoExpr (sch, tl.loc))
     | Error err -> Error err)
  | TLet (rec_flag, bindings) ->
    (match infer_let_bindings env rec_flag bindings with
     | Ok (env', infos) -> Ok (env', InfoLet (infos, tl.loc))
     | Error err -> Error err)

let infer_program env (prog : program) =
  let rec loop env infos errors = function
    | [] -> env, List.rev infos, List.rev errors
    | tl :: rest ->
      (match infer_toplevel env tl with
       | Ok (env', info) -> loop env' (info :: infos) errors rest
       | Error err -> loop env infos (err :: errors) rest)
  in
  loop env [] [] prog
