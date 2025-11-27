open Ast
open Location

module SMap = Map.Make (String)
module IMap = Map.Make (Int)

let ( let* ) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e

let ( >>= ) r f =
  match r with
  | Ok v -> f v
  | Error msg -> Error msg

type tvar =
  { id : int
  ; mutable instance : ty option
  ; mutable level : int
  }

and ty =
  | TVar of tvar
  | TCon of string * ty list

and scheme = Forall of tvar list * ty

and value_env = scheme SMap.t

and type_info =
  { params : string list
  ; ctors : (string * scheme) list
  }

and type_env = type_info SMap.t

and env =
  { vars : value_env
  ; types : type_env
  ; gen_level : int
  }

type type_error =
  { loc : Location.t
  ; message : string
  }

type binding_info =
  { name : string
  ; scheme : scheme
  }

type toplevel_info =
  | InfoLet of binding_info list * Location.t
  | InfoExpr of scheme * Location.t
  | InfoType of Location.t

let counter = ref 0

let fresh_id () =
  let v = !counter in
  incr counter;
  v

let fresh_tvar level = { id = fresh_id (); instance = None; level }
let fresh_ty level = TVar (fresh_tvar level)

let t_int = TCon ("int", [])
let t_bool = TCon ("bool", [])
let t_string = TCon ("string", [])

let t_arrow a b = TCon ("->", [ a; b ])
let t_tuple ts = TCon ("*", ts)

let rec prune = function
  | TVar ({ instance = Some ty; _ } as tv) ->
    let ty' = prune ty in
    tv.instance <- Some ty';
    ty'
  | ty -> ty

let rec ftv_ty acc ty =
  match prune ty with
  | TVar tv -> IMap.add tv.id tv acc
  | TCon (_, args) ->
    List.fold_left ftv_ty acc args

let ftv_scheme (Forall (tvs, ty)) =
  let acc = ftv_ty IMap.empty ty in
  List.fold_left (fun a tv -> IMap.remove tv.id a) acc tvs

let ftv_env env =
  SMap.fold (fun _ sch acc -> IMap.union (fun _ a _ -> Some a) acc (ftv_scheme sch)) env.vars IMap.empty

exception TypeError of string

let rec occurs_check_adjust_levels tv ty =
  match prune ty with
  | TVar tv' ->
    if tv.id = tv'.id then raise (TypeError "recursive types");
    if tv'.level > tv.level then tv'.level <- tv.level
  | TCon (_, args) -> List.iter (occurs_check_adjust_levels tv) args

let rec unify a b =
  let a = prune a in
  let b = prune b in
  match a, b with
  | TVar va, TVar vb when va.id = vb.id -> ()
  | TVar va, ty | ty, TVar va ->
    occurs_check_adjust_levels va ty;
    va.instance <- Some ty
  | TCon (na, args_a), TCon (nb, args_b) ->
    if na <> nb || List.length args_a <> List.length args_b then
      raise (TypeError "type mismatch")
    else
      List.iter2 unify args_a args_b

let generalize env ty =
  let ty_ftv = ftv_ty IMap.empty ty in
  let vars =
    IMap.fold
      (fun _ tv acc -> if tv.level > env.gen_level then tv :: acc else acc)
      ty_ftv
      []
  in
  Forall (vars, ty)

let instantiate env (Forall (vars, ty)) =
  let subst =
    List.fold_left
      (fun m tv -> IMap.add tv.id (fresh_ty env.gen_level) m)
      IMap.empty vars
  in
  let rec aux ty =
    match prune ty with
    | TVar tv ->
      (match IMap.find_opt tv.id subst with
       | Some t -> t
       | None -> ty)
    | TCon (n, args) -> TCon (n, List.map aux args)
  in
  aux ty

let push_level env = { env with gen_level = env.gen_level + 1 }

let initial_env =
  { vars = SMap.empty
  ; types = SMap.empty
  ; gen_level = 1
  }

let unify_res a b = try unify a b; Ok () with TypeError msg -> Error msg

let string_of_ty ty =
  let names = Hashtbl.create 16 in
  let name_counter = ref 0 in
  let fresh_name () =
    let i = !name_counter in
    incr name_counter;
    let base =
      let n = (i mod 26) in
      String.make 1 (Char.chr (97 + n))
    in
    if i < 26 then "'" ^ base else "'" ^ base ^ string_of_int (i / 26)
  in
  let rec aux prec ty =
    match prune ty with
    | TVar tv ->
      let key = string_of_int tv.id in
      let name =
        match Hashtbl.find_opt names key with
        | Some n -> n
        | None ->
          let n = fresh_name () in
          Hashtbl.add names key n;
          n
      in
      name
    | TCon ("->", [ a; b ]) ->
      let s =
        Printf.sprintf "%s -> %s" (aux 1 a) (aux 0 b)
      in
      if prec > 0 then "(" ^ s ^ ")" else s
    | TCon ("*", elems) ->
      let s = String.concat " * " (List.map (aux 0) elems) in
      if prec > 1 then "(" ^ s ^ ")" else s
    | TCon (name, []) -> name
    | TCon (name, args) ->
      let s = Printf.sprintf "%s %s" name (String.concat " " (List.map (aux 2) args)) in
      if prec > 1 then "(" ^ s ^ ")" else s
  in
  aux 0 ty

let string_of_scheme s =
  match s with
  | Forall ([], ty) -> string_of_ty ty
  | Forall (_, ty) -> string_of_ty ty

let lookup_type env name =
  match SMap.find_opt name env.types with
  | Some info -> Ok info
  | None ->
    (match name with
     | "int" | "bool" | "string" -> Ok { params = []; ctors = [] }
      | _ -> Error ("unknown type " ^ name))

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
    let* type_info =
      match lookup_type env name.node with
      | Ok info -> Ok info
      | Error msg -> Error msg
    in
    if List.length args <> List.length type_info.params then
      Error ("type " ^ name.node ^ " expects " ^ string_of_int (List.length type_info.params) ^ " arguments")
    else
      let* args' =
        let rec fold acc = function
          | [] -> Ok (List.rev acc)
          | a :: rest ->
            let* ta = ty_of_type_expr env tyvars a in
            fold (ta :: acc) rest
        in
        fold [] args
      in
      Ok (TCon (name.node, args'))
  | TyArrow (a, b) ->
    let* ta = ty_of_type_expr env tyvars a in
    let* tb = ty_of_type_expr env tyvars b in
    Ok (t_arrow ta tb)
  | TyTuple elems ->
    let* elems' =
      let rec fold acc = function
        | [] -> Ok (List.rev acc)
        | e :: rest ->
          let* te = ty_of_type_expr env tyvars e in
          fold (te :: acc) rest
      in
      fold [] elems
    in
    Ok (t_tuple elems')

let rec infer_pattern env pat expected =
  match pat.node with
  | PWildcard -> Ok ([], expected)
  | PVar id ->
    Ok ([ id.node, expected ], expected)
  | PAs (p, id) ->
    let* binds, t = infer_pattern env p expected in
    Ok ((id.node, t) :: binds, t)
  | PInt _ ->
    let* () = unify_res expected t_int in
    Ok ([], expected)
  | PBool _ ->
    let* () = unify_res expected t_bool in
    Ok ([], expected)
  | PString _ ->
    let* () = unify_res expected t_string in
    Ok ([], expected)
  | PTuple elems ->
    let ts = List.map (fun _ -> fresh_ty env.gen_level) elems in
    let tuple_ty = t_tuple ts in
    let* () = unify_res expected tuple_ty in
    let rec loop acc pats tys =
      match pats, tys with
      | [], [] -> Ok (List.concat (List.rev acc), tuple_ty)
      | p :: ps, t :: ts' ->
        let* b, _ = infer_pattern env p t in
        loop (b :: acc) ps ts'
      | _ -> Error "tuple arity mismatch in pattern"
    in
    loop [] elems ts
  | PConstr (ctor, args) ->
    (match SMap.find_opt ctor.node env.vars with
     | Some scheme ->
       let ctor_ty = instantiate env scheme in
       let rec collect ty acc =
         match prune ty with
         | TCon ("->", [ a; b ]) -> collect b (a :: acc)
         | result -> List.rev acc, result
       in
       let arg_tys, res_ty = collect ctor_ty [] in
       if List.length arg_tys <> List.length args then
         Error "constructor arity mismatch"
       else (
         let* () = unify_res expected res_ty in
         let rec loop acc pats tys =
           match pats, tys with
           | [], [] -> Ok (List.concat (List.rev acc), res_ty)
           | p :: ps, t :: ts' ->
             let* b, _ = infer_pattern env p t in
             loop (b :: acc) ps ts'
           | _ -> Error "constructor arity mismatch"
         in
         loop [] args arg_tys)
     | None -> Error ("unknown constructor " ^ ctor.node))
  | PAnnot (p, texpr) ->
    let tyvars = ref [] in
    let* t = ty_of_type_expr env tyvars texpr in
    let* () = unify_res expected t in
    infer_pattern env p t

let rec infer_expr env expr =
  match expr.node with
  | EInt _ -> Ok (t_int, env)
  | EBool _ -> Ok (t_bool, env)
  | EString _ -> Ok (t_string, env)
  | EVar id ->
    (match SMap.find_opt id.node env.vars with
     | None -> Error ("unbound variable " ^ id.node)
     | Some s ->
       let t = instantiate env s in
       Ok (t, env))
  | EConstr (ctor, args) ->
    (match SMap.find_opt ctor.node env.vars with
     | None -> Error ("unknown constructor " ^ ctor.node)
     | Some scheme ->
       let ctor_ty = instantiate env scheme in
       let rec collect ty acc =
         match prune ty with
         | TCon ("->", [ a; b ]) -> collect b (a :: acc)
         | result -> List.rev acc, result
       in
       let arg_tys, res_ty = collect ctor_ty [] in
       if List.length arg_tys <> List.length args then
         Error "constructor arity mismatch"
       else
         let* args_tys =
           let rec loop acc es tys =
             match es, tys with
             | [], [] -> Ok (List.rev acc)
             | e :: es', t :: ts' ->
               let* te, _ = infer_expr env e in
               let* () = unify_res te t in
               loop (te :: acc) es' ts'
             | _ -> Error "constructor arity mismatch"
           in
           loop [] args arg_tys
         in
         let* () =
           let rec loop ts es =
             match ts, es with
             | [], [] -> Ok ()
             | t :: ts', e :: es' ->
               let* () = unify_res t e in
               loop ts' es'
             | _ -> Ok ()
           in
           loop args_tys arg_tys
         in
         Ok (res_ty, env))
  | ETuple elems ->
    let* ts =
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | e :: rest ->
          let* t, _ = infer_expr env e in
          loop (t :: acc) rest
      in
      loop [] elems
    in
    Ok (t_tuple ts, env)
  | ELambda { params; fn_body } ->
    let env' = push_level env in
    let param_tys = List.map (fun _ -> fresh_ty env'.gen_level) params in
    let* binds =
      let rec loop acc pats tys =
        match pats, tys with
        | [], [] -> Ok (List.concat (List.rev acc))
        | p :: ps, t :: ts' ->
          let* b, _ = infer_pattern env' p t in
          loop (b :: acc) ps ts'
        | _ -> Error "arity mismatch in lambda parameters"
      in
      loop [] params param_tys
    in
    let env'' =
      List.fold_left
        (fun e (name, ty) -> { e with vars = SMap.add name (Forall ([], ty)) e.vars })
        env'
        binds
    in
    let* body_ty, _ = infer_expr env'' fn_body in
    let fn_ty =
      List.fold_right (fun arg acc -> t_arrow arg acc) param_tys body_ty
    in
    Ok (fn_ty, env)
  | EApp (fn, args) ->
    let* fn_ty, _ = infer_expr env fn in
    let* arg_tys =
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | e :: rest ->
          let* t, _ = infer_expr env e in
          loop (t :: acc) rest
      in
      loop [] args
    in
    let result_ty = fresh_ty env.gen_level in
    let app_ty =
      List.fold_right (fun arg acc -> t_arrow arg acc) arg_tys result_ty
    in
    let* () = unify_res fn_ty app_ty in
    Ok (result_ty, env)
  | ELet { rec_flag; bindings; in_expr } ->
    let env_rec_level = push_level env in
    (match rec_flag with
     | Nonrecursive ->
       let env_after =
         List.fold_left
           (fun e b ->
              match infer_expr env_rec_level b.node.rhs with
              | Error msg -> raise (TypeError msg)
              | Ok (t_rhs, _) ->
                (match infer_pattern env_rec_level b.node.lhs t_rhs with
                 | Error msg -> raise (TypeError msg)
                 | Ok (binds, _) ->
                   List.fold_left
                     (fun acc (name, ty) ->
                        let sch = generalize env ty in
                        { acc with vars = SMap.add name sch acc.vars })
                     e
                     binds))
           env
           bindings
       in
       let* body_ty, _ = infer_expr env_after in_expr in
       Ok (body_ty, env_after)
     | Recursive ->
       let names =
         List.map
           (fun b ->
              match b.node.lhs.node with
              | PVar id -> id.node
              | _ -> raise (TypeError "let rec requires variable patterns"))
           bindings
       in
       let provisional =
         List.map (fun name -> name, fresh_ty env_rec_level.gen_level) names
       in
       let env_with_prov =
         List.fold_left
           (fun e (name, ty) -> { e with vars = SMap.add name (Forall ([], ty)) e.vars })
           env_rec_level
           provisional
       in
       let env_vars = ref env_with_prov.vars in
       List.iter2
         (fun b (_, ty) ->
            match infer_expr env_with_prov b.node.rhs with
            | Error msg -> raise (TypeError msg)
            | Ok (rhs_ty, _) ->
              (try unify ty rhs_ty with TypeError msg -> raise (TypeError msg));
              let sch = generalize env rhs_ty in
              let name = match b.node.lhs.node with PVar id -> id.node | _ -> assert false in
              env_vars := SMap.add name sch !env_vars)
         bindings provisional;
       let env_after = { env with vars = !env_vars } in
       let* body_ty, _ = infer_expr env_after in_expr in
       Ok (body_ty, env_after))
  | EMatch (scrut, cases) ->
    let* scrut_ty, _ = infer_expr env scrut in
    let result_ty = fresh_ty env.gen_level in
    let env_case_level = push_level env in
    let* () =
      let rec loop = function
        | [] -> Ok ()
        | c :: rest ->
          let* binds, _ = infer_pattern env_case_level c.node.pattern scrut_ty in
          let env_with_binds =
            List.fold_left
              (fun e (name, ty) -> { e with vars = SMap.add name (Forall ([], ty)) e.vars })
              env_case_level
              binds
          in
          (match c.node.guard with
           | None -> Ok ()
           | Some g ->
             let* g_ty, _ = infer_expr env_with_binds g in
             let* () = unify_res g_ty t_bool in
             Ok ()) >>= fun () ->
          let* rhs_ty, _ = infer_expr env_with_binds c.node.body in
          let* () = unify_res rhs_ty result_ty in
          loop rest
      in
      loop cases
    in
    Ok (result_ty, env)
  | EAnnot (e, texpr) ->
    let tyvars = ref [] in
    let* t_expected = ty_of_type_expr env tyvars texpr in
    let* t_e, _ = infer_expr env e in
    let* () = unify_res t_e t_expected in
    Ok (t_expected, env)
let register_type_decl env (decl : type_decl) =
  let params = List.map (fun id -> id.node) decl.node.params in
  let param_vars = List.map (fun _ -> fresh_tvar (env.gen_level + 1)) params in
  let param_env = ref [] in
  List.iter2 (fun name tv -> param_env := (name, tv) :: !param_env) params param_vars;
  let type_body = TCon (decl.node.tname.node, List.map (fun tv -> TVar tv) param_vars) in
  let pre_type_info = { params; ctors = [] } in
  let env_with_type = { env with types = SMap.add decl.node.tname.node pre_type_info env.types } in
  let ctor_schemes =
    List.map
      (fun ctor ->
         let args =
           List.map
             (fun a ->
               match ty_of_type_expr env_with_type param_env a with
               | Ok t -> t
               | Error msg -> raise (TypeError msg))
             ctor.node.args
         in
         let ty =
           List.fold_right
             (fun arg acc -> t_arrow arg acc)
             args
             type_body
         in
         let sch = generalize env ty in
         ctor.node.cname.node, sch)
      decl.node.constructors
  in
  let type_info = { params; ctors = ctor_schemes } in
  let types = SMap.add decl.node.tname.node type_info env_with_type.types in
  let vars =
    List.fold_left
      (fun v (name, sch) -> SMap.add name sch v)
      env_with_type.vars ctor_schemes
  in
  { env_with_type with types; vars }

let infer_toplevel env (tl : toplevel) =
  match tl.node with
  | TType decl ->
    let env' = register_type_decl env decl in
    Ok (env', InfoType tl.loc)
  | TExpr e ->
    (match infer_expr env e with
     | Ok (ty, _) ->
       let sch = generalize env ty in
       Ok (env, InfoExpr (sch, tl.loc))
     | Error msg ->
       Error { loc = tl.loc; message = msg })
  | TLet (rec_flag, bindings) ->
    (try
       let env', info =
         match rec_flag with
         | Nonrecursive ->
           let env_rhs = push_level env in
           let env_acc = ref env in
           let infos = ref [] in
           List.iter
             (fun b ->
                match infer_expr env_rhs b.node.rhs with
                | Error msg -> raise (TypeError msg)
                | Ok (rhs_ty, _) ->
                  (match infer_pattern env_rhs b.node.lhs rhs_ty with
                   | Error msg -> raise (TypeError msg)
                   | Ok (binds, _) ->
                     let generalized =
                       List.map
                         (fun (name, ty) -> { name; scheme = generalize env ty })
                         binds
                     in
                     infos := List.rev_append generalized !infos;
                     env_acc :=
                       List.fold_left
                        (fun e info -> { e with vars = SMap.add info.name info.scheme e.vars })
                        !env_acc
                        generalized))
           bindings;
           !env_acc, InfoLet (List.rev !infos, tl.loc)
         | Recursive ->
           let names =
             List.map
               (fun b ->
                  match b.node.lhs.node with
                  | PVar id -> id.node
                  | _ -> raise (TypeError "let rec requires variable patterns"))
               bindings
           in
           let provisional =
             List.map (fun name -> name, fresh_ty env.gen_level) names
           in
           let env_with_prov =
             List.fold_left
               (fun e (name, ty) -> { e with vars = SMap.add name (Forall ([], ty)) e.vars })
               env
               provisional
           in
           let infos = ref [] in
           List.iter2
             (fun b (_, ty) ->
                match infer_expr env_with_prov b.node.rhs with
                | Error msg -> raise (TypeError msg)
                | Ok (rhs_ty, _) ->
                  (try unify rhs_ty ty with TypeError msg -> raise (TypeError msg));
                  let sch = generalize env rhs_ty in
                  infos := { name = (match b.node.lhs.node with PVar id -> id.node | _ -> assert false); scheme = sch } :: !infos)
             bindings provisional;
           let env' =
             List.fold_left
               (fun e info -> { e with vars = SMap.add info.name info.scheme e.vars })
               env
               !infos
           in
           env', InfoLet (List.rev !infos, tl.loc)
       in
       Ok (env', info)
     with
     | TypeError msg -> Error { loc = tl.loc; message = msg })

let infer_program env (prog : program) =
  let rec loop env infos errors = function
    | [] -> env, List.rev infos, List.rev errors
    | tl :: rest ->
      (match infer_toplevel env tl with
       | Ok (env', info) -> loop env' (info :: infos) errors rest
       | Error err -> loop env (infos) (err :: errors) rest)
  in
  loop env [] [] prog
