open Ber

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
  let start_line = loc.start.Lexing.pos_lnum in
  let start_col = loc.start.Lexing.pos_cnum - loc.start.Lexing.pos_bol in
  let stop_line = loc.stop.Lexing.pos_lnum in
  let stop_col = loc.stop.Lexing.pos_cnum - loc.stop.Lexing.pos_bol in
  Format.asprintf "%s:%d:%d-%d:%d" loc.file start_line start_col stop_line stop_col

let format_error (err : Parse.parse_error) =
  Format.asprintf "Error at %s: %s" (format_location err.loc) err.message

let format_type_error (err : Type_infer.type_error) =
  Format.asprintf "Type error at %s: %s" (format_location err.loc) err.message

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
