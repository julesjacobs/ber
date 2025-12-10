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

let ty_for_loc loc =
  match type_of_loc loc with
  | None -> None
  | Some ty -> Some (string_of_ty ty)

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
    let inf = mk_con "∞" [] in
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
  let add_mark loc id acc =
    match loc with
    | Some loc when not (is_dummy_loc loc) -> { loc; id } :: acc
    | _ -> acc
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
      let tree = TConNode { name = "->"; loc = meta.loc; id = meta.id; args = [ ta; tb ] } in
      s, marks, tree
    | TCon ("*", elems, meta) ->
      (match elems with
       | [] ->
         let s = "unit" in
         let tree = TConNode { name = "*"; loc = meta.loc; id = meta.id; args = [] } in
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
         let tree = TConNode { name = "*"; loc = meta.loc; id = meta.id; args = trees } in
         s, marks, tree)
    | TCon (name, [], meta) ->
      let s = name in
      let marks = add_mark meta.loc meta.id [] in
      let tree = TConNode { name; loc = meta.loc; id = meta.id; args = [] } in
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
      let tree = TConNode { name; loc = meta.loc; id = meta.id; args = [ ta ] } in
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
      let tree = TConNode { name; loc = meta.loc; id = meta.id; args = trees } in
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
      (match meta.loc with
       | Some loc when not (is_dummy_loc loc) -> [ loc ]
       | _ -> [])
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
      dedup_locs locs_e |> List.map (fun loc -> { loc; label = Some "expected"; ty = ty_for_loc loc })
    in
    let got_spans =
      dedup_locs locs_g |> List.map (fun loc -> { loc; label = Some "got"; ty = ty_for_loc loc })
    in
    let spans = got_spans @ expected_spans in
    if spans = [] then [ { loc = err.loc; label = Some "error"; ty = ty_for_loc err.loc } ] else spans
  | _ -> [ { loc = err.loc; label = Some "error"; ty = ty_for_loc err.loc } ]

let mismatch_detail heading expected_ty got_ty ~source ~expected_locs ~got_locs =
  let render_view, render_text = make_type_renderer () in
  let rec mismatch_ids a b =
    match prune a, prune b with
    | TCon (na, args_a, meta_a), TCon (nb, args_b, meta_b) ->
      if na = nb && List.length args_a = List.length args_b then
        let es, gs =
          List.fold_left2
            (fun (ea, ga) x y ->
               let ea', ga' = mismatch_ids x y in
               ea @ ea', ga @ ga')
            ([], [])
            args_a
            args_b
        in
        es, gs
      else [ meta_a.id ], [ meta_b.id ]
    | TCon (_, _, meta_a), _ -> [ meta_a.id ], []
    | _, TCon (_, _, meta_b) -> [], [ meta_b.id ]
    | _ -> [], []
  in
  let diff_mismatch expected got =
    let render_with_head prec ty =
      match prune ty with
      | TVar _ ->
        let s = render_text ty in
        s, [ 0, String.length s ]
      | TCon ("->", [ a; b ], _) ->
        let sa = render_text a in
        let sb = render_text b in
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
        let rendered = List.map render_text elems in
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
      | TCon (name, [], _) ->
        let s = name in
        let marks = [ 0, String.length name ] in
        s, marks
      | TCon (name, [arg], _) ->
        let arg_s = render_text arg in
        let sep = " " in
        let s = arg_s ^ sep ^ name in
        let name_start = String.length arg_s + String.length sep in
        let marks = [ name_start, String.length name ] in
        let need_paren = prec > 1 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks else s, marks
      | TCon (name, args, _) ->
        let rendered = List.map render_text args in
        let rec join = function
          | [] -> "", [], 0
          | [ s ] -> s, [], String.length s
          | s :: rest ->
            let tail_s, marks, _ = join rest in
            let sep = ", " in
            let s' = s ^ sep ^ tail_s in
            let marks' = List.map (fun (st, l) -> (st + String.length s + String.length sep, l)) marks in
            s', marks', String.length s'
        in
        let args_s, marks, _ = join rendered in
        let base = "(" ^ args_s ^ ")" in
        let s = base ^ " " ^ name in
        let name_start = String.length base + 1 in
        let marks = (name_start, String.length name) :: List.map (fun (st, l) -> (st + 1, l)) marks in
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
               let off_e = String.length se + String.length sep in
               let off_g = String.length sg + String.length sep in
               let shift_e m = List.map (fun (st, l) -> (st + off_e, l)) m in
               let shift_g m = List.map (fun (st, l) -> (st + off_g, l)) m in
               let me' = me @ shift_e rest_me in
               let mg' = mg @ shift_g rest_mg in
               se', me', sg', mg'
           in
           let se, me, sg, mg = join parts in
           let need_paren = prec > 1 in
           let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
           let se, me = wrap se me in
           let sg, mg = wrap sg mg in
           se, me, sg, mg
         | _, _, _ ->
           let parts = List.map2 (go 0) args_a args_b in
           let rec join = function
             | [] -> "", [], "", []
             | [ (se, me, sg, mg) ] -> se, me, sg, mg
             | (se, me, sg, mg) :: ps ->
               let rest_se, rest_me, rest_sg, rest_mg = join ps in
               let sep = ", " in
               let se' = se ^ sep ^ rest_se in
               let sg' = sg ^ sep ^ rest_sg in
               let off_e = String.length se + String.length sep in
               let off_g = String.length sg + String.length sep in
               let shift_e m = List.map (fun (st, l) -> (st + off_e, l)) m in
               let shift_g m = List.map (fun (st, l) -> (st + off_g, l)) m in
               let me' = me @ shift_e rest_me in
               let mg' = mg @ shift_g rest_mg in
               se', me', sg', mg'
           in
           let args_se, args_me, args_sg, args_mg = join parts in
           let need_paren = prec > 1 in
           (match args_a with
            | [] ->
              let se = na in
              let sg = na in
              let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
              let se, me = wrap se [] in
              let sg, mg = wrap sg [] in
              se, me, sg, mg
            | [ _ ] ->
              let sep = " " in
              let se = args_se ^ sep ^ na in
              let sg = args_sg ^ sep ^ na in
              let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
              let se, me = wrap se args_me in
              let sg, mg = wrap sg args_mg in
              se, me, sg, mg
            | _ ->
              let base_e = "(" ^ args_se ^ ")" in
              let base_g = "(" ^ args_sg ^ ")" in
              let se = base_e ^ " " ^ na in
              let sg = base_g ^ " " ^ na in
              let shift m = List.map (fun (st, l) -> (st + 1, l)) m in
              let me = shift args_me in
              let mg = shift args_mg in
              let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
              let se, me = wrap se me in
              let sg, mg = wrap sg mg in
              se, me, sg, mg))
      | TVar va, TVar vb when va.id = vb.id ->
        let s = render_text a in
        s, [], s, []
      | _ ->
        let se, me = render_with_head prec a in
        let sg, mg = render_with_head prec b in
        se, me, sg, mg
    in
    go 0 expected got
  in
  let _, marks_expected, _, marks_got = diff_mismatch expected_ty got_ty in
  let expected_view_fallback = render_view expected_ty in
  let got_view_fallback = render_view got_ty in
  let snippet loc =
    let len = String.length source in
    let start = max 0 (min len loc.start.Lexing.pos_cnum) in
    let stop = max start (min len loc.stop.Lexing.pos_cnum) in
    String.sub source start (stop - start)
  in
  let type_view_for_loc loc =
    match type_of_loc loc with
    | None -> None
    | Some ty ->
      let view, _ = make_type_renderer () in
      Some (view ty)
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
  let mismatch_expected_ids, mismatch_got_ids = mismatch_ids expected_ty got_ty in
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
             | Type_infer.Type_mismatch (got, expected) ->
               let locs_e, locs_g = mismatch_locs expected got in
               Some (Mismatch (mismatch_detail (format_location err.loc) expected got ~source ~expected_locs:locs_e ~got_locs:locs_g))
             | Type_infer.Occurs_check (tv, ty) ->
               let inf = mk_con "∞" [] in
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
