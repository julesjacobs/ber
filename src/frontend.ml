open Location
open Type_solver

type span =
  { loc : Location.t
  ; label : string option
  ; ty : string option
  }

type type_tree =
  | TVarNode of
      { name : string
      ; loc : Location.t option
      }
  | TConNode of
      { name : string
      ; loc : Location.t option
      ; id : int
      ; args : type_tree list
      }

type type_mark =
  { loc : Location.t
  ; id : int
  }

type type_view =
  { text : string
  ; marks : type_mark list
  ; tree : type_tree
  }

type expr_info =
  { expr : string
  ; ty : type_view option
  }

type check_result =
  { ok : bool
  ; output_lines : string list
  ; spans : span list
  ; detail : detail option
  }

and mismatch_detail =
  { heading : string
  ; got : type_view
  ; expected : type_view
  ; marks_got : (int * int) list
  ; marks_expected : (int * int) list
  ; expr_left : expr_info option
  ; expr_right : expr_info option
  ; mismatch_got_ids : int list
  ; mismatch_expected_ids : int list
  }

and detail =
  | Mismatch of mismatch_detail
  | Occurs of
      { heading : string
      ; ty : string
      }

let ty_for_loc (_loc : Location.t) = None

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
  | Type_infer.Type_mismatch (got, expected, reason) ->
    Format.asprintf "Type mismatch at %s: got %s, expected %s (%s)"
      (format_location err.loc)
      (Type_infer.string_of_ty got)
      (Type_infer.string_of_ty expected)
      reason
  | Type_infer.Occurs_check (tv, ty) ->
    let inf = mk_con default_loc "∞" [] in
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

let make_type_renderer () =
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
  let name_of_tv (tv : tvar) =
    match Hashtbl.find_opt names tv.id with
    | Some n -> n
    | None ->
      let n = fresh_name () in
      Hashtbl.add names tv.id n;
      n
  in
  let add_mark (loc : typed_loc) id acc =
    if is_dummy_loc loc.loc then acc else { loc = loc.loc; id } :: acc
  in
  let rec shift (_delta : int) (parts : type_mark list) = parts
  and render (prec : int) (ty : ty) : string * type_mark list * type_tree =
    match prune ty with
    | TVar tv ->
      let name = name_of_tv tv in
      let tree = TVarNode { name; loc = None } in
      name, [], tree
    | TCon ("->", [ a; b ], meta) ->
      let sa, ma, ta = render 1 a in
      let sb, mb, tb = render 0 b in
      let op = " -> " in
      let s0 = sa ^ op ^ sb in
      let marks = ma @ shift (String.length sa + String.length op) mb in
      let need_paren = prec > 0 in
      let s, marks =
        if need_paren then
          let s = "(" ^ s0 ^ ")" in
          s, shift 1 marks
        else
          s0, marks
      in
      let marks = add_mark meta.loc meta.id marks in
      let tree = TConNode { name = "->"; loc = Some meta.loc.loc; id = meta.id; args = [ ta; tb ] } in
      s, marks, tree
    | TCon ("*", elems, meta) ->
      (match elems with
       | [] ->
        let s = "unit" in
        let tree = TConNode { name = "*"; loc = Some meta.loc.loc; id = meta.id; args = [] } in
        let marks = add_mark meta.loc meta.id [] in
        s, marks, tree
       | _ ->
         let rendered = List.map (render 0) elems in
         let rec join = function
           | [] -> "", [], [], 0
           | [ (s, marks, tree) ] -> s, marks, [ tree ], String.length s
           | (s, marks, tree) :: rest ->
             let tail_s, tail_marks, tail_trees, _ = join rest in
             let sep = " * " in
             let s' = s ^ sep ^ tail_s in
             let marks' = marks @ shift (String.length s + String.length sep) tail_marks in
             s', marks', tree :: tail_trees, String.length s'
         in
         let s0, marks, trees, _ = join rendered in
         let need_paren = prec > 1 in
         let s, marks =
           if need_paren then
             let s = "(" ^ s0 ^ ")" in
             s, shift 1 marks
           else
             s0, marks
         in
         let marks = add_mark meta.loc meta.id marks in
         let tree = TConNode { name = "*"; loc = Some meta.loc.loc; id = meta.id; args = trees } in
         s, marks, tree)
    | TCon (name, [], meta) ->
      let s = name in
      let marks = add_mark meta.loc meta.id [] in
      let tree = TConNode { name; loc = Some meta.loc.loc; id = meta.id; args = [] } in
      s, marks, tree
    | TCon (name, [ arg ], meta) ->
      let sa, ma, ta = render 2 arg in
      let sep = " " in
      let s0 = sa ^ sep ^ name in
      let marks = ma in
      let need_paren = prec > 1 in
      let s, marks =
        if need_paren then
          let s = "(" ^ s0 ^ ")" in
          s, shift 1 marks
        else
          s0, marks
      in
      let marks = add_mark meta.loc meta.id marks in
      let tree = TConNode { name; loc = Some meta.loc.loc; id = meta.id; args = [ ta ] } in
      s, marks, tree
    | TCon (name, args, meta) ->
      let rendered = List.map (render 0) args in
      let rec join = function
        | [] -> "", [], [], 0
        | [ (s, marks, tree) ] -> s, marks, [ tree ], String.length s
        | (s, marks, tree) :: rest ->
          let tail_s, tail_marks, tail_trees, _ = join rest in
          let sep = ", " in
          let s' = s ^ sep ^ tail_s in
          let marks' = marks @ shift (String.length s + String.length sep) tail_marks in
          s', marks', tree :: tail_trees, String.length s'
      in
      let args_s, marks, trees, _ = join rendered in
      let base = "(" ^ args_s ^ ")" in
      let s0 = base ^ " " ^ name in
      let need_paren = prec > 1 in
      let s, marks =
        if need_paren then
          let s = "(" ^ s0 ^ ")" in
          s, shift 1 marks
        else
          s0, marks
      in
      let marks = add_mark meta.loc meta.id marks in
      let tree = TConNode { name; loc = Some meta.loc.loc; id = meta.id; args = trees } in
      s, marks, tree
  in
  let render_view ty =
    let text, marks, tree = render 0 ty in
    { text; marks; tree }
  in
  let render_text ty =
    let text, _, _ = render 0 ty in
    text
  in
  render_view, render_text

let mismatch_locs expected got =
  let head_loc ty =
    match prune ty with
    | TCon (_, _, meta) ->
      if is_dummy_loc meta.loc.loc then [] else [ meta.loc.loc ]
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
  | Type_infer.Type_mismatch (got, expected, _) ->
    let locs_e, locs_g = mismatch_locs expected got in
    let expected_spans =
      dedup_locs locs_e |> List.map (fun loc -> { loc; label = Some "expected"; ty = ty_for_loc loc })
    in
    let got_spans =
      dedup_locs locs_g |> List.map (fun loc -> { loc; label = Some "got"; ty = ty_for_loc loc })
    in
    let spans = got_spans @ expected_spans in
    if spans = [] then [ { loc = err.loc; label = Some "error"; ty = ty_for_loc err.loc } ] else spans
  | _ -> [ { loc = err.loc; label = Some "error"; ty = ty_for_loc err.loc } ]

let mismatch_detail heading expected_ty got_ty ~source ~expected_locs ~got_locs =
  let render_view, _ = make_type_renderer () in
  let head_id ty =
    match prune ty with
    | TCon (_, _, meta) -> Some meta.id
    | _ -> None
  in
  let expected_view_fallback = render_view expected_ty in
  let got_view_fallback = render_view got_ty in
  let snippet loc =
    let len = String.length source in
    let start = max 0 (min len loc.start.Lexing.pos_cnum) in
    let stop = max start (min len loc.stop.Lexing.pos_cnum) in
    String.sub source start (stop - start)
  in
  let type_view_for_loc loc =
    let _ = loc in
    None
  in
  let expr_left =
    match got_locs with
    | loc :: _ -> Some { expr = snippet loc; ty = type_view_for_loc loc }
    | _ -> None
  in
  let expr_right =
    match expected_locs with
    | loc :: _ -> Some { expr = snippet loc; ty = type_view_for_loc loc }
    | _ -> None
  in
  let expected_view =
    match expected_locs with
    | loc :: _ ->
      (match type_view_for_loc loc with
       | Some v -> v
       | None -> expected_view_fallback)
    | _ -> expected_view_fallback
  in
  let got_view =
    match got_locs with
    | loc :: _ ->
      (match type_view_for_loc loc with
       | Some v -> v
       | None -> got_view_fallback)
    | _ -> got_view_fallback
  in
  let marks_expected = [ 0, String.length expected_view.text ] in
  let marks_got = [ 0, String.length got_view.text ] in
  let mismatch_expected_ids =
    match head_id expected_ty with
    | Some id -> [ id ]
    | None -> []
  in
  let mismatch_got_ids =
    match head_id got_ty with
    | Some id -> [ id ]
    | None -> []
  in
  { heading
  ; got = got_view
  ; expected = expected_view
  ; marks_got
  ; marks_expected
  ; expr_left
  ; expr_right
  ; mismatch_got_ids
  ; mismatch_expected_ids
  }

let typecheck_string ?(filename = "repl") source =
  reset_tracked_locs ();
  let lexbuf = Lexing.from_string source in
  Parse.set_initial_pos ~filename lexbuf;
  match Parse.parse_program_lexbuf lexbuf with
  | Error err ->
    let spans = [ { loc = err.loc; label = Some "error"; ty = ty_for_loc err.loc } ] in
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
             | Type_infer.Type_mismatch (got, expected, _) ->
               let locs_e, locs_g = mismatch_locs expected got in
               Some (Mismatch (mismatch_detail (format_location err.loc) expected got ~source ~expected_locs:locs_e ~got_locs:locs_g))
             | Type_infer.Occurs_check (tv, ty) ->
               let inf = mk_con default_loc "∞" [] in
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
