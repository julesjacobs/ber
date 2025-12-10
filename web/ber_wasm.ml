open Js_of_ocaml

module Loc = Ber.Location

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
    val ty =
      match s.ty with
      | None -> Js.null
      | Some t -> Js.some (Js.string t)
  end

let js_loc (loc : Loc.t) =
  let start_col = loc.start.Lexing.pos_cnum - loc.start.Lexing.pos_bol in
  let end_col = loc.stop.Lexing.pos_cnum - loc.stop.Lexing.pos_bol in
  object%js
    val startLine = loc.start.Lexing.pos_lnum
    val startCol = start_col
    val endLine = loc.stop.Lexing.pos_lnum
    val endCol = end_col
  end

let js_loc_opt = function
  | None -> Js.null
  | Some loc -> Js.some (js_loc loc)

let rec js_type_tree = function
  | Ber.Frontend.TVarNode { name; loc } ->
    object%js
      val kind = Js.string "var"
      val name = Js.string name
      val loc = js_loc_opt loc
      val id = 0
      val args = Js.array [||]
    end
  | Ber.Frontend.TConNode { name; loc; id; args } ->
    let args = List.map js_type_tree args |> Array.of_list |> Js.array in
    object%js
      val kind = Js.string "con"
      val name = Js.string name
      val loc = js_loc_opt loc
      val id = id
      val args = args
    end

let js_loc_mark (m : Ber.Frontend.type_mark) =
  object%js
    val loc = js_loc m.loc
    val id = m.id
  end

let js_type_view (v : Ber.Frontend.type_view) =
  object%js
    val text = Js.string v.text
    val marks = Js.array (Array.of_list (List.map js_loc_mark v.marks))
    val tree = js_type_tree v.tree
  end

let js_expr_info = function
  | None -> Js.null
  | Some (e : Ber.Frontend.expr_info) ->
    Js.some
      (object%js
        val expr = Js.string e.expr
        val ty =
          match e.ty with
          | None -> Js.null
          | Some ty -> Js.some (js_type_view ty)
      end)

let wrap_result (res : Ber.Frontend.check_result) =
  let output =
    match res.output_lines with
    | [] -> "ok"
    | _ -> String.concat "\n" res.output_lines
  in
  let spans = List.map js_span res.spans in
  let spans_arr = Js.array (Array.of_list spans) in
  let detail =
    match res.detail with
    | None -> Js.null
    | Some (Ber.Frontend.Mismatch d) ->
      Js.some
        (object%js
          val kind = Js.string "type_mismatch"
          val heading = Js.some (Js.string d.heading)
          val got = Js.some (js_type_view d.got)
          val expected = Js.some (js_type_view d.expected)
          val marksGot = Js.null
          val marksExpected = Js.null
          val mismatchGotIds = Js.array (Array.of_list d.mismatch_got_ids)
          val mismatchExpectedIds = Js.array (Array.of_list d.mismatch_expected_ids)
          val exprLeft = js_expr_info d.expr_left
          val exprRight = js_expr_info d.expr_right
          val occursTy = Js.null
        end)
    | Some (Ber.Frontend.Occurs ty_s) ->
      Js.some
        (object%js
          val kind = Js.string "occurs"
          val heading = Js.some (Js.string ty_s.heading)
          val got = Js.null
          val expected = Js.null
          val marksGot = Js.null
          val marksExpected = Js.null
          val mismatchGotIds = Js.array [||]
          val mismatchExpectedIds = Js.array [||]
          val exprLeft = Js.null
          val exprRight = Js.null
          val occursTy = Js.some (Js.string ty_s.ty)
        end)
  in
  object%js
    val ok = Js.bool res.ok
    val output = Js.string output
    val spans = spans_arr
    val detail = detail
  end

let typecheck code =
  let source = Js.to_string code in
  wrap_result (Ber.Frontend.typecheck_string source)

let () =
  Js.export "ber" (object%js
    method typecheck (code : Js.js_string Js.t) = typecheck code
  end)
