open Ber
open Location
open Type_solver

let render_output_lines content =
  let lines = String.split_on_char '\n' content in
  let lines =
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines
  in
  match lines with
  | [] -> [">"]
  | _ -> List.map (fun line -> "> " ^ line) lines

let is_blank_line line = String.trim line = ""

let trim_surrounding_blank_lines lines =
  let rec drop_leading = function
    | l :: rest when is_blank_line l -> drop_leading rest
    | rest -> rest
  in
  let rec drop_trailing = function
    | [] -> []
    | l :: rest ->
      let rest' = drop_trailing rest in
      if rest' = [] && is_blank_line l then [] else l :: rest'
  in
  let rec collapse acc prev_blank = function
    | [] -> List.rev acc
    | l :: rest ->
      let blank = is_blank_line l in
      if blank && prev_blank then collapse acc true rest
      else collapse (l :: acc) blank rest
  in
  drop_trailing (collapse [] false (drop_leading lines))

let drop_trailing_blank_lines lines =
  let rec aux = function
    | [] -> []
    | [ l ] when is_blank_line l -> []
    | l :: rest -> l :: aux rest
  in
  aux (List.rev lines) |> List.rev

let format_location (loc : Location.t) =
  let rel_file =
    let cwd = Sys.getcwd () in
    let prefix = cwd ^ Filename.dir_sep in
    let file = loc.file in
    if String.length file >= String.length prefix && String.sub file 0 (String.length prefix) = prefix then
      String.sub file (String.length prefix) (String.length file - String.length prefix)
    else
      file
  in
  let start_line = loc.start.Lexing.pos_lnum in
  let start_col = loc.start.Lexing.pos_cnum - loc.start.Lexing.pos_bol in
  let stop_line = loc.stop.Lexing.pos_lnum in
  let stop_col = loc.stop.Lexing.pos_cnum - loc.stop.Lexing.pos_bol in
  Format.asprintf "%s:%d:%d-%d:%d" rel_file start_line start_col stop_line stop_col

let format_error (err : Parse.parse_error) =
  Format.asprintf "Error at %s: %s" (format_location err.loc) err.message

let mismatch_locs expected got =
  let head_loc ty =
    match prune ty with
    | TCon (_, _, Some loc) when loc.file <> "" -> [ loc ]
    | _ -> []
  in
  let rec go a b =
    match prune a, prune b with
    | TCon (na, args_a, _), TCon (nb, args_b, _)
      when na = nb && List.length args_a = List.length args_b ->
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

type highlight =
  { loc : Location.t
  ; ch : string
  ; label : string option
  }

let print_error_span = ref false

let got_ch = "▲"
let expected_ch = "△"

let dedup_highlights hs =
  let tbl = Hashtbl.create 32 in
  let key h =
    let loc = h.loc in
    let s = loc.start in
    let e = loc.stop in
    Printf.sprintf "%s:%d:%d-%d:%d:%s"
      loc.file
      s.Lexing.pos_lnum (s.Lexing.pos_cnum - s.Lexing.pos_bol)
      e.Lexing.pos_lnum (e.Lexing.pos_cnum - e.Lexing.pos_bol)
      h.ch
  in
  List.filter
    (fun h ->
       let k = key h in
       if Hashtbl.mem tbl k then false
       else (Hashtbl.add tbl k (); true))
    hs

 let read_file_lines file =
  try
    let ic = open_in file in
    let rec loop acc =
      match input_line ic with
      | line -> loop (line :: acc)
      | exception End_of_file ->
        close_in ic;
        List.rev acc
    in
    Some (loop [])
  with _ -> None

let pack_highlights ranges =
  let ranges = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) ranges in
  let rec place packed = function
    | [] -> List.rev packed
    | r :: rs ->
      let rec try_place acc = function
        | [] -> List.rev acc, None
        | line :: rest ->
          let last_end =
            match List.rev line with
            | [] -> -1
            | (_, e, _) :: _ -> e
          in
          let s, _, _ = r in
          if s > last_end then
            (List.rev acc) @ ((line @ [ r ]) :: rest), Some ()
          else
            try_place (line :: acc) rest
      in
      (match packed with
       | [] -> place [ [ r ] ] rs
       | _ ->
         let packed', placed = try_place [] packed in
         let packed'' = match placed with None -> packed' @ [ [ r ] ] | Some () -> packed' in
         place packed'' rs)
  in
  place [] ranges

let format_highlights (locs : highlight list) =
  match locs with
  | [] -> ""
  | locs ->
    let locs = dedup_highlights locs in
    let file = (List.hd locs).loc.file in
    let all_same_file = List.for_all (fun l -> l.loc.file = file) locs in
    if not all_same_file then
      String.concat "\n" (List.map (fun h -> format_location h.loc) locs)
    else
      match read_file_lines file with
      | None -> String.concat "\n" (List.map (fun h -> format_location h.loc) locs)
      | Some file_lines ->
        let min_line =
          List.fold_left (fun acc h -> min acc h.loc.start.Lexing.pos_lnum) max_int locs
        in
        let max_line =
          List.fold_left (fun acc h -> max acc h.loc.stop.Lexing.pos_lnum) min_int locs
        in
        let buf = Buffer.create 256 in
        for line_no = min_line to max_line do
          let line_content =
            if line_no - 1 < List.length file_lines then List.nth file_lines (line_no - 1) else ""
          in
          let prefix = Printf.sprintf "%d | " line_no in
          Buffer.add_string buf prefix;
          Buffer.add_string buf line_content;
          Buffer.add_char buf '\n';
          let line_ranges = List.filter (fun h -> line_no >= h.loc.start.Lexing.pos_lnum && line_no <= h.loc.stop.Lexing.pos_lnum) locs in
          let underline_lines =
            let ranges_for_line =
              List.map
                (fun h ->
                   let start_col = if line_no = h.loc.start.Lexing.pos_lnum then h.loc.start.Lexing.pos_cnum - h.loc.start.Lexing.pos_bol else 0 in
                   let stop_col =
                     if line_no = h.loc.stop.Lexing.pos_lnum then max start_col (h.loc.stop.Lexing.pos_cnum - h.loc.stop.Lexing.pos_bol - 1)
                     else (String.length line_content) - 1
                   in
                   let start_col = max 0 (min start_col (String.length line_content)) in
                   let stop_col = max start_col (min stop_col (String.length line_content)) in
                   (start_col, stop_col, h))
                line_ranges
            in
            ranges_for_line |> pack_highlights
          in
          let prefix_spaces = String.make (String.length prefix) ' ' in
          List.iter
            (fun line ->
               let arr = Array.make (String.length line_content) " " in
              List.iter
                (fun (s, e, h) ->
                   for i = s to e do
                     arr.(i) <- h.ch
                   done)
                line;
               if Array.exists (fun c -> c <> " ") arr then (
                 Buffer.add_string buf prefix_spaces;
                 Array.iter (Buffer.add_string buf) arr;
                 Buffer.add_char buf '\n'))
            underline_lines;
          let messages =
            line_ranges
            |> List.filter_map (fun h ->
                match h.label with
                | None -> None
                | Some msg ->
                  let start_col = if line_no = h.loc.start.Lexing.pos_lnum then h.loc.start.Lexing.pos_cnum - h.loc.start.Lexing.pos_bol else 0 in
                  Some (start_col, msg))
          in
          List.iter
            (fun (col, msg) ->
               Buffer.add_string buf prefix_spaces;
               Buffer.add_string buf (String.make col ' ');
               Buffer.add_string buf "|\n";
               Buffer.add_string buf prefix_spaces;
               Buffer.add_string buf (String.make col ' ');
               Buffer.add_string buf msg;
               Buffer.add_char buf '\n')
            messages
        done;
        Buffer.contents buf

let format_type_error (err : Type_infer.type_error) =
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
      | TCon ("->", [ a; b ], _) ->
        let s = Printf.sprintf "%s -> %s" (aux 1 a) (aux 0 b) in
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
    aux 0
  in
  let diff_mismatch expected got =
    let render_with_head prec ty =
      match prune ty with
      | TVar _ ->
        let s = render_ty ty in
        s, [ 0, String.length s ], []
      | TCon ("->", [ a; b ], loc) ->
        let sa = render_ty a in
        let sb = render_ty b in
        let op = " -> " in
        let s = sa ^ op ^ sb in
        let pos = String.length sa + 1 in
        let marks = [ pos, 2 ] in
        let locs = match loc with None -> [] | Some l -> [ l ] in
        let need_paren = prec > 0 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks, locs else s, marks, locs
      | TCon ("*", elems, loc) ->
        (match elems with
         | [] ->
           let s = "unit" in
           let marks = [ 0, String.length s ] in
           let locs = match loc with None -> [] | Some l -> [ l ] in
           s, marks, locs
         | _ ->
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
        let locs = match loc with None -> [] | Some l -> [ l ] in
        let need_paren = prec > 1 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks, locs else s, marks, locs)
      | TCon (name, [], loc) ->
        let s = name in
        let marks = [ 0, String.length name ] in
        let locs = match loc with None -> [] | Some l -> [ l ] in
        s, marks, locs
      | TCon (name, [arg], loc) ->
        let arg_s = render_ty arg in
        let sep = " " in
        let s = arg_s ^ sep ^ name in
        let name_start = String.length arg_s + String.length sep in
        let marks = [ name_start, String.length name ] in
        let locs = match loc with None -> [] | Some l -> [ l ] in
        let need_paren = prec > 1 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks, locs else s, marks, locs
      | TCon (name, args, loc) ->
        let rendered = List.map render_ty args in
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
        let locs = match loc with None -> [] | Some l -> [ l ] in
        let need_paren = prec > 1 in
        if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) marks, locs else s, marks, locs
    in
    let rec go prec a b =
      match prune a, prune b with
      | TCon (na, args_a, _), TCon (nb, args_b, _) when na = nb && List.length args_a = List.length args_b ->
        (match na, args_a, args_b with
         | "->", [ a1; a2 ], [ b1; b2 ] ->
           let se1, sg1, me1, mg1, le1, lg1 = go 1 a1 b1 in
           let se2, sg2, me2, mg2, le2, lg2 = go 0 a2 b2 in
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
           se, sg, me, mg, le1 @ le2, lg1 @ lg2
         | "*", _, _ ->
           let parts = List.map2 (go 0) args_a args_b in
           let rec join = function
           | [] -> "", [], "", [], [], []
           | [ (se, sg, me, mg, le, lg) ] -> se, me, sg, mg, le, lg
           | (se, sg, me, mg, le, lg) :: ps ->
             let rest_se, rest_me, rest_sg, rest_mg, rest_le, rest_lg = join ps in
             let sep = " * " in
             let se' = se ^ sep ^ rest_se in
             let sg' = sg ^ sep ^ rest_sg in
             let off_e = String.length se + String.length sep in
             let off_g = String.length sg + String.length sep in
             let shift_e m = List.map (fun (st, l) -> (st + off_e, l)) m in
             let shift_g m = List.map (fun (st, l) -> (st + off_g, l)) m in
             let me' = me @ shift_e rest_me in
             let mg' = mg @ shift_g rest_mg in
             se', me', sg', mg', le @ rest_le, lg @ rest_lg
           in
           let se, me, sg, mg, le, lg = join parts in
           let need_paren = prec > 1 in
           let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
           let se, me = wrap se me in
           let sg, mg = wrap sg mg in
           se, sg, me, mg, le, lg
         | _, _, _ ->
           let parts = List.map2 (go 0) args_a args_b in
           let rec join = function
             | [] -> "", [], "", [], [], []
             | [ (se, sg, me, mg, le, lg) ] -> se, me, sg, mg, le, lg
             | (se, sg, me, mg, le, lg) :: ps ->
             let rest_se, rest_me, rest_sg, rest_mg, rest_le, rest_lg = join ps in
             let sep = ", " in
             let se' = se ^ sep ^ rest_se in
             let sg' = sg ^ sep ^ rest_sg in
             let off_e = String.length se + String.length sep in
             let off_g = String.length sg + String.length sep in
             let shift_e m = List.map (fun (st, l) -> (st + off_e, l)) m in
             let shift_g m = List.map (fun (st, l) -> (st + off_g, l)) m in
             let me' = me @ shift_e rest_me in
             let mg' = mg @ shift_g rest_mg in
             se', me', sg', mg', le @ rest_le, lg @ rest_lg
           in
           let args_se, args_me, args_sg, args_mg, le_args, lg_args = join parts in
           let need_paren = prec > 1 in
           (match args_a with
            | [] ->
              let se = na in
              let sg = na in
              let me = [] in
              let mg = [] in
              let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
              let se, me = wrap se me in
              let sg, mg = wrap sg mg in
              se, sg, me, mg, le_args, lg_args
            | [ _ ] ->
              let sep = " " in
              let se = args_se ^ sep ^ na in
              let sg = args_sg ^ sep ^ na in
              let wrap s m = if need_paren then "(" ^ s ^ ")", List.map (fun (st, l) -> (st + 1, l)) m else s, m in
              let se, me = wrap se args_me in
              let sg, mg = wrap sg args_mg in
              se, sg, me, mg, le_args, lg_args
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
              se, sg, me, mg, le_args, lg_args))
      | TVar va, TVar vb when va.id = vb.id ->
        let s = render_ty a in
        s, s, [], [], [], []
      | _ ->
        let se, me, le = render_with_head prec a in
        let sg, mg, lg = render_with_head prec b in
        se, sg, me, mg, le, lg
    in
    let se, sg, me, mg, _le, _lg = go 0 expected got in
    se, sg, me, mg
  in
  let kind_msg =
    match err.kind with
    | Type_infer.Type_mismatch (got_ty, expected_ty) ->
      let se, sg, me, mg = diff_mismatch expected_ty got_ty in
      let locs_e, locs_g = mismatch_locs expected_ty got_ty in
      let mk_line prefix s marks ch =
        let line = prefix ^ s in
        let underline =
          if marks = [] then None
          else
            let arr = Array.make (String.length s) " " in
            List.iter (fun (start, len) -> for i = start to start + len - 1 do if i >= 0 && i < Array.length arr then arr.(i) <- ch done) marks;
            let buf = Buffer.create 16 in
            Buffer.add_string buf (String.make (String.length prefix) ' ');
            Array.iter (Buffer.add_string buf) arr;
            Some (Buffer.contents buf)
        in
        match underline with
        | None -> [ line ]
        | Some u -> [ line; u ]
      in
      let loc_block =
        let highlights =
          List.map (fun loc -> { loc; ch = got_ch; label = None }) locs_g
          @ List.map (fun loc -> { loc; ch = expected_ch; label = None }) locs_e
        in
        let highlights =
          if !print_error_span then { loc = err.loc; ch = "-"; label = None } :: highlights else highlights
        in
        format_highlights highlights
      in
      let lines =
        mk_line "Got:      " sg mg got_ch
        @ mk_line "Expected: " se me expected_ch
      in
      "Type mismatch:\n" ^ loc_block ^ "\n" ^ String.concat "\n" lines
    | Type_infer.Occurs_check (tv, ty) ->
      let inf = TCon ("∞", [], None) in
      let ty_infinite =
        let save = tv.instance in
        tv.instance <- Some inf;
        let s = Type_infer.string_of_ty ty in
        tv.instance <- save;
        s
      in
      let msg = "Would require self-referential type ∞ = " ^ ty_infinite in
      let loc_block = format_highlights [ { loc = err.loc; ch = got_ch; label = None } ] in
      msg ^ "\n" ^ loc_block
    | Type_infer.Message msg ->
      let loc_block = format_highlights [ { loc = err.loc; ch = got_ch; label = None } ] in
      msg ^ "\n" ^ loc_block
  in
  kind_msg

let process_block_type filename (env : Type_infer.env) (block : File_format.block) =
  let has_content = List.exists (fun l -> String.trim l <> "") block.code_lines in
  if not has_content then env, trim_surrounding_blank_lines block.code_lines
  else
    let code_lines = drop_trailing_blank_lines block.code_lines in
    let src = String.concat "\n" code_lines in
    let lexbuf = Lexing.from_string src in
    Parse.set_initial_pos ~filename ~line:block.start_line lexbuf;
    let outputs =
      match Parse.parse_program_lexbuf lexbuf with
      | Error err ->
        [ render_output_lines (format_error err) ], env
      | Ok prog ->
        let rec loop env acc = function
          | [] -> List.rev acc, env
          | tl :: rest ->
            let env', outs =
              match Type_infer.infer_toplevel env tl with
              | Ok (env', info) ->
                let lines =
                  match info with
                  | Type_infer.InfoType _ -> [ "type declaration" ]
                  | Type_infer.InfoExpr (sch, _) -> [ "val it : " ^ Type_infer.string_of_scheme sch ]
                  | Type_infer.InfoLet (binds, _) ->
                    List.map (fun (b : Type_infer.binding_info) -> "val " ^ b.name ^ " : " ^ Type_infer.string_of_scheme b.scheme) binds
                in
                env', List.concat_map render_output_lines lines
              | Error err -> env, render_output_lines (format_type_error err)
            in
            loop env' (outs :: acc) rest
        in
        loop env [] prog
    in
    let outputs, env' = outputs in
    let outputs = List.concat outputs in
    let combined =
      match outputs with
      | [] -> code_lines
      | _ ->
        let code_lines = drop_trailing_blank_lines code_lines in
        let sep = if code_lines = [] then [] else [ "" ] in
        trim_surrounding_blank_lines (code_lines @ sep @ outputs)
    in
    env', combined

let process_block filename pragmas (block : File_format.block) =
  let _ = pragmas in
  let has_content = List.exists (fun l -> String.trim l <> "") block.code_lines in
  if not has_content then trim_surrounding_blank_lines block.code_lines
  else
    let code_lines = drop_trailing_blank_lines block.code_lines in
    let lexbuf = Lexing.from_string (String.concat "\n" code_lines) in
    Parse.set_initial_pos ~filename ~line:block.start_line lexbuf;
    let outputs =
      match Parse.parse_program_lexbuf lexbuf with
      | Ok prog ->
        List.concat_map
          (fun tl ->
             let rendered = Format.asprintf "%a" Pp.pp_toplevel tl in
             render_output_lines rendered)
          prog
      | Error err -> render_output_lines (format_error err)
    in
    let combined =
      match outputs with
      | [] -> code_lines
      | _ ->
        let sep = if code_lines = [] then [] else [ "" ] in
        trim_surrounding_blank_lines (code_lines @ sep @ outputs)
    in
    combined

let rewrite_file filename =
  let input =
    let ic = open_in_bin filename in
    let len = in_channel_length ic in
    let data = really_input_string ic len in
    close_in ic;
    data
  in
  let lines = String.split_on_char '\n' input in
  let doc = File_format.parse lines in
  let type_mode = File_format.has_pragma doc "type" in
  let processed_blocks =
    if type_mode then
      let _, blocks =
        List.fold_left
          (fun (env, acc) block ->
             let env', lines = process_block_type filename env block in
             env', lines :: acc)
          (Type_infer.initial_env, [])
          doc.blocks
      in
      List.rev blocks
    else
      List.map (process_block filename doc.pragmas) doc.blocks
  in
  let buf = Buffer.create (String.length input + 128) in
  List.iter (fun p -> Buffer.add_string buf ("#" ^ p ^ "\n")) doc.pragmas;
  let rec emit = function
    | [] -> ()
    | [ lines ] ->
      List.iter (fun line -> Buffer.add_string buf line; Buffer.add_char buf '\n') lines
    | lines :: rest ->
      List.iter (fun line -> Buffer.add_string buf line; Buffer.add_char buf '\n') lines;
      Buffer.add_char buf '\n';
      emit rest
  in
  emit processed_blocks;
  let oc = open_out_bin filename in
  output_string oc (Buffer.contents buf);
  close_out oc

let () =
  match Sys.argv with
  | [| _; filename |] -> rewrite_file filename
  | _ ->
    prerr_endline "Usage: ber-cli <file>";
    exit 1
