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

let format_location (loc : Location.t) =
  let start_line = loc.start.Lexing.pos_lnum in
  let start_col = loc.start.Lexing.pos_cnum - loc.start.Lexing.pos_bol in
  let stop_line = loc.stop.Lexing.pos_lnum in
  let stop_col = loc.stop.Lexing.pos_cnum - loc.stop.Lexing.pos_bol in
  Format.asprintf "%s:%d:%d-%d:%d" loc.file start_line start_col stop_line stop_col

let format_error (err : Parse.parse_error) =
  Format.asprintf "Error at %s: %s" (format_location err.loc) err.message

let process_block filename pragmas (block : File_format.block) =
  let _ = pragmas in
  let has_content = List.exists (fun l -> String.trim l <> "") block.code_lines in
  if not has_content then block.code_lines
  else
    let lines = Array.of_list block.code_lines in
    let len =
      let rec find_last i =
        if i < 0 then -1
        else if String.trim lines.(i) = "" then find_last (i - 1)
        else i
      in
      let last = find_last (Array.length lines - 1) in
      if last < 0 then 0 else last + 1
    in
    let starts =
      let starts_with prefix s =
        let lp = String.length prefix in
        String.length s >= lp && String.sub s 0 lp = prefix
      in
      let rec collect i acc =
        if i >= len then List.rev acc
        else
          let line = String.trim lines.(i) in
          if starts_with "let" line || starts_with "type" line then collect (i + 1) (i :: acc)
          else collect (i + 1) acc
      in
      match collect 0 [] with
      | [] -> [ 0 ]
      | xs -> xs
    in
    let spans =
      let rec aux acc = function
        | [] -> acc
        | [ s ] -> (s, len) :: acc
        | s1 :: ((s2 :: _) as rest) -> aux ((s1, s2) :: acc) rest
      in
      List.rev (aux [] starts)
    in
    let buf = Buffer.create (String.length (String.concat "\n" block.code_lines) + 64) in
    let append_line s = Buffer.add_string buf s; Buffer.add_char buf '\n' in
    let append_range a b =
      for i = a to b do
        append_line lines.(i)
      done
    in
    let rec loop cursor = function
      | [] ->
        if cursor < len then append_range cursor (len - 1)
      | (s, e) :: rest ->
        if cursor < s then append_range cursor (s - 1);
        let src =
          let b = Buffer.create 128 in
          for i = s to e - 1 do
            if i > s then Buffer.add_char b '\n';
            Buffer.add_string b lines.(i)
          done;
          Buffer.contents b
        in
        let lexbuf = Lexing.from_string src in
        Parse.set_initial_pos ~filename ~line:(block.start_line + s) lexbuf;
        let output_lines =
          match Parse.parse_toplevel_lexbuf lexbuf with
          | Ok item ->
            let rendered = Format.asprintf "%a" Pp.pp_toplevel item in
            render_output_lines rendered
          | Error err ->
            render_output_lines (format_error err)
        in
        append_range s (e - 1);
        if output_lines <> [] then append_line "" ;
        List.iter append_line output_lines;
        if output_lines <> [] then append_line "" ;
        loop e rest
    in
    loop 0 spans;
    let combined = Buffer.contents buf |> String.split_on_char '\n' in
    (match List.rev combined with
     | "" :: rest -> List.rev rest
     | _ -> combined)

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
  let processed_blocks = List.map (process_block filename doc.pragmas) doc.blocks in
  let buf = Buffer.create (String.length input + 128) in
  List.iter (fun p -> Buffer.add_string buf ("#" ^ p ^ "\n")) doc.pragmas;
  let rec emit = function
    | [] -> ()
    | lines :: rest ->
      List.iter (fun line -> Buffer.add_string buf line; Buffer.add_char buf '\n') lines;
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
