{
open Parser
module L = Location

exception LexError of L.t * string

let loc lexbuf = L.span (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

let unexpected lexbuf =
  let chr = Lexing.lexeme lexbuf in
  raise (LexError (loc lexbuf, "unexpected character: " ^ chr))

let unterminated lexbuf what =
  raise (LexError (loc lexbuf, "unterminated " ^ what))
}

let digit = ['0'-'9']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let ident_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let tyvar = '\'' ident_char+
let lident = lowercase ident_char*
let uident = uppercase ident_char*

rule token = parse
  | [' ' '\t' '\r']       { token lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; token lexbuf }
  | "(*"                  { comment 1 lexbuf; token lexbuf }
  | digit+ as lit         { INT (int_of_string lit) }
  | '"'                   {
      let start = Lexing.lexeme_start_p lexbuf in
      string start (Buffer.create 16) lexbuf
    }
  | "_"                   { UNDERSCORE }
  | "->"                  { ARROW }
  | ":"                   { COLON }
  | "="                   { EQUAL }
  | "|"                   { BAR }
  | ","                   { COMMA }
  | "*"                   { STAR }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | tyvar as tv           { TYVAR tv }
  | uident as id          { UIDENT id }
  | lident as id          {
      match id with
      | "let" -> LET
      | "rec" -> REC
      | "and" -> AND
      | "in" -> IN
      | "fun" -> FUN
      | "match" -> MATCH
      | "with" -> WITH
      | "type" -> TYPE
      | "of" -> OF
      | "as" -> AS
      | "when" -> WHEN
      | "true" -> TRUE
      | "false" -> FALSE
      | _ -> LIDENT id
    }
  | eof                   { EOF }
  | _                     { unexpected lexbuf }

and comment depth = parse
  | "(*"                  { comment (depth + 1) lexbuf }
  | "*)"                  { if depth = 1 then () else comment (depth - 1) lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof                   { unterminated lexbuf "comment" }
  | _                     { comment depth lexbuf }

and string start buf = parse
  | '"'                   {
      lexbuf.lex_start_p <- start;
      STRING (Buffer.contents buf)
    }
  | '\\' ('\\' | '"' as c){ Buffer.add_char buf c; string start buf lexbuf }
  | "\\n"                 { Buffer.add_char buf '\n'; string start buf lexbuf }
  | "\\t"                 { Buffer.add_char buf '\t'; string start buf lexbuf }
  | "\\r"                 { Buffer.add_char buf '\r'; string start buf lexbuf }
  | "\\b"                 { Buffer.add_char buf '\b'; string start buf lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string start buf lexbuf }
  | eof                   { unterminated lexbuf "string literal" }
  | _ as c                { Buffer.add_char buf c; string start buf lexbuf }
