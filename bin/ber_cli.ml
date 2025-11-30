open Ber
open Location

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

let format_locations (locs : Location.t list) =
  match locs with
  | [] -> ""
  | locs ->
    let file = (List.hd locs).file in
    let all_same_file = List.for_all (fun l -> l.file = file) locs in
    if not all_same_file then
      String.concat "\n" (List.map format_location locs)
    else
      let lines =
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
      in
      match lines with
      | None -> String.concat "\n" (List.map format_location locs)
      | Some file_lines ->
        let min_line =
          List.fold_left (fun acc l -> min acc l.start.Lexing.pos_lnum) max_int locs
        in
        let max_line =
          List.fold_left (fun acc l -> max acc l.stop.Lexing.pos_lnum) min_int locs
        in
        let buf = Buffer.create 128 in
        for line_no = min_line to max_line do
          let line_content =
            if line_no - 1 < List.length file_lines then List.nth file_lines (line_no - 1) else ""
          in
          let prefix = Printf.sprintf "%d | " line_no in
          Buffer.add_string buf prefix;
          Buffer.add_string buf line_content;
          Buffer.add_char buf '\n';
          let underline =
            let len = String.length line_content in
            let arr = Array.make len ' ' in
            let apply_loc loc =
              let start_line = loc.start.Lexing.pos_lnum in
              let stop_line = loc.stop.Lexing.pos_lnum in
              let start_col = loc.start.Lexing.pos_cnum - loc.start.Lexing.pos_bol in
              let stop_col = loc.stop.Lexing.pos_cnum - loc.stop.Lexing.pos_bol in
              if line_no < start_line || line_no > stop_line then ()
              else
                let s =
                  if line_no = start_line then start_col else 0
                in
                let e =
                  if line_no = stop_line then max s (stop_col - 1) else len - 1
                in
                let s = max 0 (min s (len - 1)) in
                let e = max s (min e (len - 1)) in
                for i = s to e do
                  arr.(i) <- '^'
                done
            in
            List.iter apply_loc locs;
            let underline_str = String.init len (fun i -> arr.(i)) in
            underline_str
          in
          if underline |> String.trim |> String.length > 0 then (
            Buffer.add_string buf (String.make (String.length prefix) ' ');
            Buffer.add_string buf underline;
            Buffer.add_char buf '\n')
        done;
        Buffer.contents buf

let format_type_error (err : Type_infer.type_error) =
  let kind_msg =
    match err.kind with
    | Type_infer.Type_mismatch (a, b) ->
      Format.asprintf "type mismatch: %s vs %s" (Type_infer.string_of_ty a) (Type_infer.string_of_ty b)
    | Type_infer.Occurs_check (tv, ty) ->
      Format.asprintf "occurs check failed: %s occurs in %s" (Type_infer.string_of_ty (Type_solver.TVar tv)) (Type_infer.string_of_ty ty)
    | Type_infer.Message msg -> msg
  in
  let loc_block = format_locations [ err.loc ] in
  Format.asprintf "Type error: %s\n%s" kind_msg loc_block

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
