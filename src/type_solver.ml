module IMap = Map.Make (Int)

open Location

let default_loc = Location.span Lexing.dummy_pos Lexing.dummy_pos

type tvar =
  { id : int
  ; mutable instance : ty option
  ; mutable level : int
  }

and ty =
  | TVar of tvar
  | TCon of string * ty list * Location.t option

and scheme = Forall of tvar list * ty

let counter = ref 0

let fresh_id () =
  let v = !counter in
  incr counter;
  v

let fresh_tvar level = { id = fresh_id (); instance = None; level }
let fresh_ty level = TVar (fresh_tvar level)

module Loc_table = struct
  module H = Hashtbl.Make (struct
      type t = Location.t
      let hash (loc : t) =
        Hashtbl.hash
          ( loc.file
          , loc.start.Lexing.pos_lnum
          , loc.start.Lexing.pos_cnum
          , loc.start.Lexing.pos_bol
          , loc.stop.Lexing.pos_lnum
          , loc.stop.Lexing.pos_cnum
          , loc.stop.Lexing.pos_bol )
      let equal a b =
        a.file = b.file
        && a.start.Lexing.pos_lnum = b.start.Lexing.pos_lnum
        && a.start.Lexing.pos_cnum = b.start.Lexing.pos_cnum
        && a.start.Lexing.pos_bol = b.start.Lexing.pos_bol
        && a.stop.Lexing.pos_lnum = b.stop.Lexing.pos_lnum
        && a.stop.Lexing.pos_cnum = b.stop.Lexing.pos_cnum
        && a.stop.Lexing.pos_bol = b.stop.Lexing.pos_bol
    end)

  let table = H.create 256

  let reset () = H.reset table

  let track (loc : Location.t) (ty : ty) =
    if loc.file <> "" then H.replace table loc ty

  let find (loc : Location.t) = H.find_opt table loc
end

let reset_tracked_locs = Loc_table.reset
let track_loc_type = Loc_table.track
let type_of_loc = Loc_table.find

let mk_con ?loc name args =
  let loc = match loc with None -> Some default_loc | Some _ as l -> l in
  let ty = TCon (name, args, loc) in
  (match loc with
   | None -> ()
   | Some loc -> track_loc_type loc ty);
  ty

let t_int ?loc () = mk_con ?loc "int" []
let t_bool ?loc () = mk_con ?loc "bool" []
let t_string ?loc () = mk_con ?loc "string" []

let t_arrow ?loc a b = mk_con ?loc "->" [ a; b ]
let t_tuple ?loc ts = mk_con ?loc "*" ts

let rec prune = function
  | TVar ({ instance = Some ty; _ } as tv) ->
    let ty' = prune ty in
    tv.instance <- Some ty';
    ty'
  | ty -> ty

let rec ftv_ty acc ty =
  match prune ty with
  | TVar tv -> IMap.add tv.id tv acc
  | TCon (_, args, _) ->
    List.fold_left ftv_ty acc args

let ftv_scheme (Forall (tvs, ty)) =
  let acc = ftv_ty IMap.empty ty in
  List.fold_left (fun a tv -> IMap.remove tv.id a) acc tvs

exception Occurs_check_failed
exception Occurs of tvar * ty
exception TypeMismatch
exception Compiler_bug of string

let rec occurs_check_adjust_levels tv ty =
  match prune ty with
  | TVar tv' ->
    if tv.id = tv'.id then raise Occurs_check_failed
    else if tv'.level > tv.level then tv'.level <- tv.level
  | TCon (_, args, _) ->
    List.iter (occurs_check_adjust_levels tv) args

and unify_list xs ys =
  match xs, ys with
  | [], [] -> Ok ()
  | x :: xs', y :: ys' ->
    unify x y;
    unify_list xs' ys'
  | _ -> raise (Compiler_bug "arity mismatch in unify_list")

and unify a b =
  let a = prune a in
  let b = prune b in
  match a, b with
  | TVar va, TVar vb when va.id = vb.id -> ()
  | TVar va, ty | ty, TVar va ->
    (try occurs_check_adjust_levels va ty with Occurs_check_failed -> raise (Occurs (va, ty)));
    va.instance <- Some ty
  | TCon (na, args_a, _), TCon (nb, args_b, _) ->
    if na <> nb || List.length args_a <> List.length args_b then
      raise TypeMismatch
    else
      match unify_list args_a args_b with
      | Ok () -> ()
      | Error e -> raise e

let unify_res = unify

let generalize ~level ty =
  let ty_ftv = ftv_ty IMap.empty ty in
  let vars =
    IMap.fold
      (fun _ tv acc -> if tv.level > level then tv :: acc else acc)
      ty_ftv
      []
  in
  Forall (vars, ty)

let instantiate ?loc ~level (Forall (vars, ty)) =
  let subst =
    List.fold_left
      (fun m tv -> IMap.add tv.id (fresh_ty level) m)
      IMap.empty vars
  in
  let rec aux ty =
    match prune ty with
    | TVar tv ->
      (match IMap.find_opt tv.id subst with
       | Some t -> t
       | None -> ty)
    | TCon (n, args, loc') ->
      let use_loc =
        match loc with
        | None -> loc'
        | some -> some
      in
      mk_con ?loc:use_loc n (List.map aux args)
  in
  aux ty

let string_of_ty ?generalized ty =
  let names = Hashtbl.create 16 in
  let gen_counter = ref 0 in
  let weak_counter = ref 0 in
  let fresh_gen_name () =
    let i = !gen_counter in
    incr gen_counter;
    let base =
      let n = i mod 26 in
      String.make 1 (Char.chr (97 + n))
    in
    if i < 26 then "'" ^ base else "'" ^ base ^ string_of_int (i / 26)
  in
  let fresh_weak_name () =
    let i = !weak_counter in
    incr weak_counter;
    "'_weak" ^ string_of_int (i + 1)
  in
  let rec aux prec ty =
    match prune ty with
    | TVar tv ->
      let name =
        match Hashtbl.find_opt names tv.id with
        | Some n -> n
        | None ->
          let n =
            let is_generalized =
              match generalized with
              | None -> true
              | Some gen -> IMap.mem tv.id gen
            in
            if is_generalized then fresh_gen_name () else fresh_weak_name ()
          in
          Hashtbl.add names tv.id n;
          n
      in
      name
    | TCon ("->", [ a; b ], _) ->
      let s =
        Printf.sprintf "%s -> %s" (aux 1 a) (aux 0 b)
      in
      if prec > 0 then "(" ^ s ^ ")" else s
    | TCon ("*", [], _) -> "unit"
    | TCon ("*", elems, _) ->
      let s = String.concat " * " (List.map (aux 0) elems) in
      if prec > 1 then "(" ^ s ^ ")" else s
    | TCon (name, [], _) -> name
    | TCon (name, [arg], _) ->
      let s = Printf.sprintf "%s %s" (aux 2 arg) name in
      if prec > 1 then "(" ^ s ^ ")" else s
    | TCon (name, args, _) ->
      let s = Printf.sprintf "(%s) %s" (String.concat ", " (List.map (aux 0) args)) name in
      if prec > 1 then "(" ^ s ^ ")" else s
  in
  aux 0 ty

let string_of_scheme s =
  match s with
  | Forall (tvs, ty) ->
    let gen =
      List.fold_left (fun acc tv -> IMap.add tv.id () acc) IMap.empty tvs
    in
    string_of_ty ~generalized:gen ty
let ( let* ) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e
