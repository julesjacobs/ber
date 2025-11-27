type parse_error =
  { loc : Location.t
  ; message : string
  }

let current_loc lexbuf =
  Location.span (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

let error_from_exn lexbuf = function
  | Lexer.LexError (loc, msg) -> { loc; message = msg }
  | Syntax_error.Syntax_error (loc, msg) -> { loc; message = msg }
  | Parser.Error -> { loc = current_loc lexbuf; message = "syntax error" }
  | exn ->
    let msg = Printexc.to_string exn in
    { loc = current_loc lexbuf; message = msg }

let with_file ?filename lexbuf =
  match filename with
  | None -> ()
  | Some file -> Lexing.set_filename lexbuf file

type buffered_lexer =
  { lexbuf : Lexing.lexbuf
  ; mutable stash : Parser.token option
  ; mutable last : Parser.token option
  }

let create_buffered lexbuf = { lexbuf; stash = None; last = None }

let next_token bl =
  match bl.stash with
  | Some t ->
    bl.stash <- None;
    t
  | None ->
    let t = Lexer.token bl.lexbuf in
    bl.last <- Some t;
    t

let peek_token bl =
  let t = next_token bl in
  bl.stash <- Some t;
  t

let rec skip_to_sync bl =
  match next_token bl with
  | Parser.LET | TYPE -> ()
  | EOF -> ()
  | _ -> skip_to_sync bl

let set_initial_pos ?filename ?(line = 1) lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let pos =
    { pos with
      Lexing.pos_fname = (match filename with None -> pos.Lexing.pos_fname | Some f -> f)
    ; pos_lnum = line
    }
  in
  lexbuf.lex_curr_p <- pos

let parse_program_lexbuf ?filename lexbuf =
  with_file ?filename lexbuf;
  try Ok (Parser.program Lexer.token lexbuf) with
  | exn -> Error (error_from_exn lexbuf exn)

let parse_expression_lexbuf ?filename lexbuf =
  with_file ?filename lexbuf;
  try Ok (Parser.expression Lexer.token lexbuf) with
  | exn -> Error (error_from_exn lexbuf exn)

let parse_type_expression_lexbuf ?filename lexbuf =
  with_file ?filename lexbuf;
  try Ok (Parser.type_expression Lexer.token lexbuf) with
  | exn -> Error (error_from_exn lexbuf exn)

let parse_toplevel_lexbuf ?filename lexbuf =
  with_file ?filename lexbuf;
  try Ok (Parser.toplevel_entry Lexer.token lexbuf) with
  | exn -> Error (error_from_exn lexbuf exn)

let parse_toplevels_lexbuf ?filename lexbuf =
  with_file ?filename lexbuf;
  let rec skip_to_sync_raw () =
    match Lexer.token lexbuf with
    | Parser.LET | TYPE -> false
    | EOF -> true
    | _ -> skip_to_sync_raw ()
  in
  let rec loop acc_items acc_errs =
    try
      let item = Parser.toplevel_naked Lexer.token lexbuf in
      loop (item :: acc_items) acc_errs
    with
    | exn ->
      let err = error_from_exn lexbuf exn in
      let reached_eof = skip_to_sync_raw () in
      let acc_errs = err :: acc_errs in
      if reached_eof then Ok (List.rev acc_items, List.rev acc_errs)
      else loop acc_items acc_errs
  in
  loop [] []

let from_string ?filename f s =
  let lexbuf = Lexing.from_string s in
  f ?filename lexbuf

let program_from_string ?filename s =
  from_string ?filename parse_program_lexbuf s

let expression_from_string ?filename s =
  from_string ?filename parse_expression_lexbuf s

let type_from_string ?filename s =
  from_string ?filename parse_type_expression_lexbuf s

let toplevel_from_string ?filename s =
  from_string ?filename parse_toplevel_lexbuf s

let toplevels_from_string ?filename s =
  from_string ?filename parse_toplevels_lexbuf s

let program_from_channel ?filename ic =
  let lexbuf = Lexing.from_channel ic in
  parse_program_lexbuf ?filename lexbuf
