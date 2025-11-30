open Js_of_ocaml

let wrap_result = function
  | Ber.Frontend.Success lines ->
    let output =
      match lines with
      | [] -> "ok"
      | _ -> String.concat "\n" lines
    in
    object%js
      val ok = Js._true
      val output = Js.string output
    end
  | Ber.Frontend.Failure msg ->
    object%js
      val ok = Js._false
      val output = Js.string msg
    end

let typecheck code =
  let source = Js.to_string code in
  wrap_result (Ber.Frontend.typecheck_string source)

let () =
  Js.export "ber" (object%js
    method typecheck (code : Js.js_string Js.t) = typecheck code
  end)
