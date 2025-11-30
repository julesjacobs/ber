open Location
open Type_solver

type span =
  { loc : Location.t
  ; label : string option
  }

type check_result =
  { ok : bool
  ; output_lines : string list
  ; spans : span list
  }

let format_location (loc : Location.t) =
  let start_line = loc.start.Lexing.pos_lnum in
  let start_col = loc.start.Lexing.pos_cnum - loc.start.Lexing.pos_bol in
  let stop_line = loc.stop.Lexing.pos_lnum in
  let stop_col = loc.stop.Lexing.pos_cnum - loc.stop.Lexing.pos_bol in
  Format.asprintf "%s:%d:%d-%d:%d" loc.file start_line start_col stop_line stop_col

let format_parse_error (err : Parse.parse_error) =
  Format.asprintf "Parse error at %s: %s" (format_location err.loc) err.message

let format_type_error (err : Type_infer.type_error) =
  match err.kind with
  | Type_infer.Type_mismatch (got, expected) ->
    Format.asprintf "Type mismatch at %s: got %s, expected %s"
      (format_location err.loc)
      (Type_infer.string_of_ty got)
      (Type_infer.string_of_ty expected)
  | Type_infer.Occurs_check (_tv, ty) ->
    Format.asprintf "Occurs check at %s: cannot construct infinite type %s"
      (format_location err.loc)
      (Type_infer.string_of_ty ty)
  | Type_infer.Message msg ->
    Format.asprintf "Error at %s: %s" (format_location err.loc) msg

let toplevel_outputs (info : Type_infer.toplevel_info) =
  match info with
  | Type_infer.InfoType _ -> []
  | Type_infer.InfoExpr (sch, _) -> [ "val it : " ^ Type_infer.string_of_scheme sch ]
  | Type_infer.InfoLet (binds, _) ->
    List.map
      (fun (b : Type_infer.binding_info) ->
         "val " ^ b.name ^ " : " ^ Type_infer.string_of_scheme b.scheme)
      binds

let is_dummy_loc (loc : Location.t) =
  loc.file = ""
  && loc.start.Lexing.pos_cnum = 0
  && loc.stop.Lexing.pos_cnum = 0

let mismatch_locs expected got =
  let head_loc ty =
    match prune ty with
    | TCon (_, _, Some loc) when not (is_dummy_loc loc) -> [ loc ]
    | _ -> []
  in
  let rec go a b =
    match prune a, prune b with
    | TCon (na, args_a, _), TCon (nb, args_b, _) when na = nb && List.length args_a = List.length args_b ->
      (match na, args_a, args_b with
       | "->", [ a1; a2 ], [ b1; b2 ] ->
         let le1, lg1 = go a1 b1 in
         let le2, lg2 = go a2 b2 in
         le1 @ le2, lg1 @ lg2
       | _, _, _ ->
         let parts = List.map2 go args_a args_b in
         List.fold_left
           (fun (le_acc, lg_acc) (le, lg) -> le_acc @ le, lg_acc @ lg)
           ([], [])
           parts)
    | TVar va, TVar vb when va.id = vb.id -> [], []
    | _ ->
      let le = head_loc a in
      let lg = head_loc b in
      le, lg
  in
  go expected got

let dedup_locs locs =
  let tbl = Hashtbl.create 16 in
  List.filter
    (fun (loc : Location.t) ->
       let key =
         ( loc.file
         , loc.start.Lexing.pos_lnum
         , loc.start.Lexing.pos_cnum
         , loc.stop.Lexing.pos_lnum
         , loc.stop.Lexing.pos_cnum )
       in
       if Hashtbl.mem tbl key then false else (Hashtbl.add tbl key (); true))
    locs

let spans_for_error (err : Type_infer.type_error) =
  match err.kind with
  | Type_infer.Type_mismatch (got, expected) ->
    let locs_e, locs_g = mismatch_locs expected got in
    let expected_spans =
      dedup_locs locs_e |> List.map (fun loc -> { loc; label = Some "expected" })
    in
    let got_spans =
      dedup_locs locs_g |> List.map (fun loc -> { loc; label = Some "got" })
    in
    let spans = got_spans @ expected_spans in
    if spans = [] then [ { loc = err.loc; label = Some "error" } ] else spans
  | _ -> [ { loc = err.loc; label = Some "error" } ]

let typecheck_string ?(filename = "repl") source =
  let lexbuf = Lexing.from_string source in
  Parse.set_initial_pos ~filename lexbuf;
  match Parse.parse_program_lexbuf lexbuf with
  | Error err ->
    { ok = false; output_lines = [ format_parse_error err ]; spans = [] }
  | Ok prog ->
    let rec loop env acc = function
      | [] -> { ok = true; output_lines = List.rev acc; spans = [] }
      | tl :: rest ->
        (match Type_infer.infer_toplevel env tl with
         | Ok (env', info) ->
           let acc = List.rev_append (toplevel_outputs info) acc in
           loop env' acc rest
         | Error err ->
           let spans = spans_for_error err in
           { ok = false; output_lines = [ format_type_error err ]; spans })
    in
    loop Type_infer.initial_env [] prog
