open Location

type check_result =
  | Success of string list
  | Failure of string

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
  | Type_infer.Occurs_check (_tv, ty) ->
    Format.asprintf "Occurs check at %s: cannot construct infinite type %s"
      (format_location err.loc)
      (Type_infer.string_of_ty ty)
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

let typecheck_string ?(filename = "repl") source =
  let lexbuf = Lexing.from_string source in
  Parse.set_initial_pos ~filename lexbuf;
  match Parse.parse_program_lexbuf lexbuf with
  | Error err -> Failure (format_parse_error err)
  | Ok prog ->
    let rec loop env acc = function
      | [] -> Success (List.rev acc)
      | tl :: rest ->
        (match Type_infer.infer_toplevel env tl with
         | Ok (env', info) ->
           let acc = List.rev_append (toplevel_outputs info) acc in
           loop env' acc rest
         | Error err -> Failure (format_type_error err))
    in
    loop Type_infer.initial_env [] prog
