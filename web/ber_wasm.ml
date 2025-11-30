open Js_of_ocaml

let js_span (s : Ber.Frontend.span) =
  let loc = s.loc in
  let start_col = loc.start.Lexing.pos_cnum - loc.start.Lexing.pos_bol in
  let end_col = loc.stop.Lexing.pos_cnum - loc.stop.Lexing.pos_bol in
  object%js
    val startLine = loc.start.Lexing.pos_lnum
    val startCol = start_col
    val endLine = loc.stop.Lexing.pos_lnum
    val endCol = end_col
    val label =
      match s.label with
      | None -> Js.null
      | Some l -> Js.some (Js.string l)
  end

let wrap_result (res : Ber.Frontend.check_result) =
  let output =
    match res.output_lines with
    | [] -> "ok"
    | _ -> String.concat "\n" res.output_lines
  in
  let spans = List.map js_span res.spans in
  let spans_arr = Js.array (Array.of_list spans) in
  object%js
    val ok = Js.bool res.ok
    val output = Js.string output
    val spans = spans_arr
  end

let typecheck code =
  let source = Js.to_string code in
  wrap_result (Ber.Frontend.typecheck_string source)

let () =
  Js.export "ber" (object%js
    method typecheck (code : Js.js_string Js.t) = typecheck code
  end)
