open Lexing

type 'a loc = {
  loc_val : 'a;
  loc_start : Lexing.position;
  loc_end : Lexing.position
}

let pos_str s =
  if s.pos_fname = "" then
    Printf.sprintf "line %d, character %d" s.pos_lnum (s.pos_cnum - s.pos_bol)
  else
    Printf.sprintf
      "file \"%s\", line %d, character %d"
	s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol)

let pos_str2 s e =
  if s.pos_fname != e.pos_fname then
    Printf.sprintf
      "file \"%s\", line %d, character %d - file \"%s\", line %d, character %d"
      s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol)
      e.pos_fname e.pos_lnum (e.pos_cnum - e.pos_bol)
  else if s.pos_lnum != e.pos_lnum then
    if s.pos_fname = "" then
      Printf.sprintf
	"line %d, character %d - line %d, character %d"
	s.pos_lnum (s.pos_cnum - s.pos_bol)
	e.pos_lnum (e.pos_cnum - e.pos_bol)
    else
      Printf.sprintf
	"file \"%s\", line %d, character %d - line %d, character %d"
	s.pos_fname
	s.pos_lnum (s.pos_cnum - s.pos_bol)
	e.pos_lnum (e.pos_cnum - e.pos_bol)
  else
    if s.pos_fname = "" then
      Printf.sprintf
	"line %d, characters %d - %d" s.pos_lnum
	(s.pos_cnum - s.pos_bol)
	(e.pos_cnum - e.pos_bol)
    else
      Printf.sprintf
	"file \"%s\", line %d, characters %d - %d" s.pos_fname s.pos_lnum
	(s.pos_cnum - s.pos_bol)
	(e.pos_cnum - e.pos_bol)

let loc_str l = pos_str2 l.loc_start l.loc_end

let loc_dummy v = {
  loc_val = v;
  loc_start = dummy_pos;
  loc_end = dummy_pos
}

let loc_inherit l v = {
  loc_val = v;
  loc_start = l.loc_start;
  loc_end = l.loc_end
}
