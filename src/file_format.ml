type pragma = string

type block =
  { code_lines : string list
  ; start_line : int
  }

type document =
  { pragmas : pragma list
  ; blocks : block list
  }

let drop_trailing_empty_line lines =
  match List.rev lines with
  | "" :: rest -> List.rev rest
  | _ -> lines

let is_pragma_line line =
  String.length line > 0 && line.[0] = '#'

let is_output_line line =
  String.length line > 0 && line.[0] = '>'

let trim_pragma line =
  String.sub line 1 (String.length line - 1) |> String.trim

let parse lines =
  let lines = drop_trailing_empty_line lines in
  let rec take_pragmas acc = function
    | line :: rest when is_pragma_line line -> take_pragmas (trim_pragma line :: acc) rest
    | rest -> List.rev acc, rest
  in
  let pragmas, remaining = take_pragmas [] lines in
  let rec loop line_no current start_line acc = function
    | [] ->
      let acc =
        match current, start_line with
        | [], _ -> acc
        | code, Some start -> { code_lines = List.rev code; start_line = start } :: acc
        | _ -> acc
      in
      List.rev acc
    | line :: rest ->
      if is_output_line line then
        let acc =
          match current, start_line with
          | [], _ -> acc
          | code, Some start -> { code_lines = List.rev code; start_line = start } :: acc
          | _ -> acc
        in
        loop (line_no + 1) [] None acc rest
      else
        let start_line = match start_line with None -> Some line_no | Some s -> Some s in
        loop (line_no + 1) (line :: current) start_line acc rest
  in
  let blocks = loop 1 [] None [] remaining in
  { pragmas; blocks }

let has_pragma doc name = List.exists (( = ) name) doc.pragmas
