open Lexing

type position = Lexing.position

type t =
  { file : string
  ; start : position
  ; stop : position
  }

type 'a located =
  { loc : t
  ; node : 'a
  }

let copy_pos (p : position) : position =
  { Lexing.pos_fname = p.pos_fname
  ; pos_lnum = p.pos_lnum
  ; pos_bol = p.pos_bol
  ; pos_cnum = p.pos_cnum
  }

let file_of_positions start stop =
  match start.pos_fname, stop.pos_fname with
  | "", "" -> ""
  | "", f | f, "" -> f
  | f, _ -> f

let span (start : position) (stop : position) : t =
  let start = copy_pos start in
  let stop = copy_pos stop in
  let file = file_of_positions start stop in
  { file; start; stop }

let span_between (a : t) (b : t) : t = span a.start b.stop

let with_loc (loc : t) (node : 'a) : 'a located = { loc; node }

let map (f : 'a -> 'b) (loc_node : 'a located) : 'b located =
  { loc = loc_node.loc; node = f loc_node.node }

let pp_position fmt (p : position) =
  Format.fprintf fmt "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let pp fmt (t : t) =
  Format.fprintf fmt "%a-%a" pp_position t.start pp_position t.stop

let pp_located pp_node fmt (located : 'a located) =
  Format.fprintf fmt "%a@ (%a)" pp_node located.node pp located.loc
