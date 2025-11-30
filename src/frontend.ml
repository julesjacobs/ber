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
  ; detail : detail option
  }

and mismatch_detail =
  { heading : string
  ; got : string
  ; expected : string
  ; marks_got : (int * int) list
  ; marks_expected : (int * int) list
  }

and detail =
  | Mismatch of mismatch_detail
  | Occurs of
      { heading : string
      ; ty : string
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
  | Type_infer.Occurs_check (tv, ty) ->
    let inf = TCon ("∞", [], None) in
    let ty_infinite =
      let save = tv.instance in
      tv.instance <- Some inf;
      let s = Type_infer.string_of_ty ty in
      tv.instance <- save;
      s
    in
    "Would require self-referential type ∞ = " ^ ty_infinite
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

let mismatch_detail heading expected_ty got_ty =
  let render_ty =
    let names = Hashtbl.create 16 in
    let counter = ref 0 in
    let fresh_name () =
      let i = !counter in
      incr counter;
      let base =
        let n = i mod 26 in
        String.make 1 (Char.chr (97 + n))
      in
      if i < 26 then "'" ^ base else "'" ^ base ^ string_of_int (i / 26)
    in
    let rec aux prec ty =
      match prune ty with
      | TVar tv ->
        (match Hashtbl.find_opt names tv.id with
         | Some n -> n
         | None ->
           let n = fresh_name () in
           Hashtbl.add names tv.id n;
           n)
      | TCon ("*", [], _) -> "unit"
      | TCon ("->", [ a; b ], _) ->
        let s = Printf.sprintf "%s -> %s" (aux 1 a) (aux 0 b) in
        if prec > 0 then "(" ^ s ^ ")" else s
      | TCon ("*", elems, _) ->
        let s = String.concat " * " (List.map (aux 0) elems) in
        if prec > 1 then "(" ^ s ^ ")" else s
      | TCon (name, [], _) -> name
      | TCon (name, args, _) ->
        let s = Printf.sprintf "%s %s" name (String.concat " " (List.map (aux 2) args)) in
        if prec > 1 then "(" ^ s ^ ")" else s
    in
    aux 0
  in
  let diff_mismatch expected got =
    let render_with_head prec ty =
      match prune ty with
      | TVar _ ->
        let s = render_ty ty in
        s, [ 0, String.length s ]
      | TCon ("->", [ a; b ], _) ->
        let sa = render_ty a in
        let sb = render_ty b in
        let op = " -> " in
        let s = sa ^ op ^ sb in
        let pos = String.length sa + 1 in
        let marks = [ pos, 2 ] in
        let need_paren = prec > 0 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks else s, marks
      | TCon ("*", elems, _) when elems = [] ->
        let s = "unit" in
        s, [ 0, String.length s ]
      | TCon ("*", elems, _) ->
        let rendered = List.map render_ty elems in
        let rec join = function
          | [] -> "", [], 0
          | [ s ] -> s, [], String.length s
          | s :: rest ->
            let tail_s, marks, _ = join rest in
            let sep = " * " in
            let s' = s ^ sep ^ tail_s in
            let op_pos = String.length s + 1 in
            let marks' = (op_pos, 1) :: List.map (fun (st, l) -> (st + String.length s + String.length sep, l)) marks in
            s', marks', String.length s'
        in
        let s, marks, _ = join rendered in
        let need_paren = prec > 1 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks else s, marks
      | TCon (name, args, _) ->
        let rendered = List.map render_ty args in
        let rec join = function
          | [] -> "", [], 0
          | [ s ] -> s, [], String.length s
          | s :: rest ->
            let tail_s, marks, _ = join rest in
            let sep = " " in
            let s' = s ^ sep ^ tail_s in
            let marks' = List.map (fun (st, l) -> (st + String.length s + String.length sep, l)) marks in
            s', marks', String.length s'
        in
        let args_s, marks, _ = join rendered in
        let base = if args = [] then name else name ^ " " in
        let s = base ^ args_s in
        let marks = (0, String.length name) :: List.map (fun (st, l) -> (st + String.length base, l)) marks in
        let need_paren = prec > 1 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks else s, marks
    in
    let rec go prec a b =
      match prune a, prune b with
      | TCon (na, args_a, _), TCon (nb, args_b, _) when na = nb && List.length args_a = List.length args_b ->
        (match na, args_a, args_b with
         | "->", [ a1; a2 ], [ b1; b2 ] ->
           let se1, me1, sg1, mg1 = go 1 a1 b1 in
           let se2, me2, sg2, mg2 = go 0 a2 b2 in
           let op = " -> " in
           let se = se1 ^ op ^ se2 in
           let sg = sg1 ^ op ^ sg2 in
           let shift m off = List.map (fun (st, l) -> (st + off, l)) m in
           let me = me1 @ shift me2 (String.length se1 + String.length op) in
           let mg = mg1 @ shift mg2 (String.length sg1 + String.length op) in
           let need_paren = prec > 0 in
           let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
           let se, me = wrap se me in
           let sg, mg = wrap sg mg in
           se, me, sg, mg
         | "*", _, _ ->
           let parts = List.map2 (go 0) args_a args_b in
           let rec join = function
             | [] -> "", [], "", []
             | [ (se, me, sg, mg) ] -> se, me, sg, mg
             | (se, me, sg, mg) :: ps ->
               let rest_se, rest_me, rest_sg, rest_mg = join ps in
               let sep = " * " in
               let se' = se ^ sep ^ rest_se in
               let sg' = sg ^ sep ^ rest_sg in
               let off = String.length se + String.length sep in
               let shift m = List.map (fun (st, l) -> (st + off, l)) m in
               let me' = me @ shift rest_me in
               let mg' = mg @ shift rest_mg in
               se', me', sg', mg'
           in
           let se, me, sg, mg = join parts in
           let need_paren = prec > 1 in
           let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
           let se, me = wrap se me in
           let sg, mg = wrap sg mg in
           se, me, sg, mg
         | _, _, _ ->
           let parts = List.map2 (go 2) args_a args_b in
           let rec join = function
             | [] -> "", [], "", []
             | [ (se, me, sg, mg) ] -> se, me, sg, mg
             | (se, me, sg, mg) :: ps ->
               let rest_se, rest_me, rest_sg, rest_mg = join ps in
               let sep = " " in
               let se' = se ^ sep ^ rest_se in
               let sg' = sg ^ sep ^ rest_sg in
               let off = String.length se + String.length sep in
               let shift m = List.map (fun (st, l) -> (st + off, l)) m in
               let me' = me @ shift rest_me in
               let mg' = mg @ shift rest_mg in
               se', me', sg', mg'
           in
           let args_se, args_me, args_sg, args_mg = join parts in
           let base = na ^ " " in
           let se = base ^ args_se in
           let sg = base ^ args_sg in
           let shift m = List.map (fun (st, l) -> (st + String.length base, l)) m in
           let me = shift args_me in
           let mg = shift args_mg in
           let need_paren = prec > 1 in
           let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
           let se, me = wrap se me in
           let sg, mg = wrap sg mg in
           se, me, sg, mg)
      | TVar va, TVar vb when va.id = vb.id ->
        let s = render_ty a in
        s, [], s, []
      | _ ->
        let se, me = render_with_head prec a in
        let sg, mg = render_with_head prec b in
        se, me, sg, mg
    in
    go 0 expected got
  in
  let expected_s, marks_expected, got_s, marks_got = diff_mismatch expected_ty got_ty in
  { heading; got = got_s; expected = expected_s; marks_got; marks_expected }

let typecheck_string ?(filename = "repl") source =
  let lexbuf = Lexing.from_string source in
  Parse.set_initial_pos ~filename lexbuf;
  match Parse.parse_program_lexbuf lexbuf with
  | Error err ->
    let spans = [ { loc = err.loc; label = Some "error" } ] in
    { ok = false; output_lines = [ format_parse_error err ]; spans; detail = None }
  | Ok prog ->
    let rec loop env acc = function
      | [] -> { ok = true; output_lines = List.rev acc; spans = []; detail = None }
      | tl :: rest ->
        (match Type_infer.infer_toplevel env tl with
         | Ok (env', info) ->
           let acc = List.rev_append (toplevel_outputs info) acc in
           loop env' acc rest
         | Error err ->
           let spans = spans_for_error err in
           let detail =
             match err.kind with
             | Type_infer.Type_mismatch (got, expected) ->
               Some (Mismatch (mismatch_detail (format_location err.loc) expected got))
             | Type_infer.Occurs_check (tv, ty) ->
               let inf = TCon ("∞", [], None) in
               let ty_infinite =
                 let save = tv.instance in
                 tv.instance <- Some inf;
                 let s = Type_infer.string_of_ty ty in
                 tv.instance <- save;
                 s
               in
               Some (Occurs { heading = format_location err.loc; ty = ty_infinite })
             | _ -> None
           in
           { ok = false; output_lines = [ format_type_error err ]; spans; detail })
    in
    loop Type_infer.initial_env [] prog
