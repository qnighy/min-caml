type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let cond_paren_open cond pf =
  if cond then Format.fprintf pf "@[(" else ()
let cond_paren_close cond pf =
  if cond then Format.fprintf pf ")@]" else ()

let cond_paren_printf cond pf =
  if cond then begin
    cond_paren_open cond pf;
    Format.kfprintf (cond_paren_close cond) pf
  end else
    Format.fprintf pf

open Lexing

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

let rec pp_prec prec pf = function
  | Unit -> Format.fprintf pf "()"
  | Bool b -> Format.fprintf pf "%b" b
  | Int i -> Format.fprintf pf "%d" i
  | Float f -> Format.fprintf pf "%.1g" f
  | Not x ->
      cond_paren_printf (prec < 10) pf "@[<2>~@,%a@]" (pp_prec 9) x
  | Neg x ->
      cond_paren_printf (prec < 20) pf "@[<2>-@,%a@]" (pp_prec 19) x
  | Add (x, y) ->
      cond_paren_printf (prec < 40) pf "@[<2>%a@ +@ %a@]"
        (pp_prec 40) x (pp_prec 39) y
  | Sub (x, y) ->
      cond_paren_printf (prec < 40) pf "@[<2>%a@ -@ %a@]"
        (pp_prec 40) x (pp_prec 39) y
  | FNeg x ->
      cond_paren_printf (prec < 20) pf "@[<2>.-@,%a@]" (pp_prec 19) x
  | FAdd (x, y) ->
      cond_paren_printf (prec < 40) pf "@[<2>%a@ .+@ %a@]"
        (pp_prec 40) x (pp_prec 39) y
  | FSub (x, y) ->
      cond_paren_printf (prec < 40) pf "@[<2>%a@ .-@ %a@]"
        (pp_prec 40) x (pp_prec 39) y
  | FMul (x, y) ->
      cond_paren_printf (prec < 30) pf "@[<2>%a@ .*@ %a@]"
        (pp_prec 30) x (pp_prec 29) y
  | FDiv (x, y) ->
      cond_paren_printf (prec < 30) pf "@[<2>%a@ ./@ %a@]"
        (pp_prec 30) x (pp_prec 29) y
  | Eq (x, y) ->
      cond_paren_printf (prec < 50) pf "@[<2>%a@ =@ %a@]"
        (pp_prec 50) x (pp_prec 49) y
  | LE (x, y) ->
      cond_paren_printf (prec < 50) pf "@[<2>%a@ <=@ %a@]"
        (pp_prec 50) x (pp_prec 49) y
  | If (x, y, z) ->
      begin match z with
      | If _ ->
          cond_paren_printf (prec < 80) pf
            "@[<2>if@ %a@ then@ %a@]@ else@ %a"
            (pp_prec 100) x (pp_prec 100) y (pp_prec 100) z
      | _ ->
          cond_paren_printf (prec < 80) pf
            "@[<2>if@ %a@ then@ %a@]@ @[<2>else@ %a@]"
            (pp_prec 100) x (pp_prec 100) y (pp_prec 100) z
      end
  | Let ((a, ta), x, y) ->
      cond_paren_printf (prec < 100) pf
        "@[<2>let@ %s@ :@ %a@ =@ %a@]@ in@ %a"
        a Type.pp ta (pp_prec 100) x (pp_prec 100) y
  | Var a ->
      Format.fprintf pf "%s" a
  | LetRec (f, y) ->
      cond_paren_open (prec < 100) pf;
      Format.fprintf pf "@[<2>let rec@ %s" (fst f.name);
      List.iter (fun (argnam, argt) ->
        Format.fprintf pf "@ @[<2>(%s@ :@ %a)@]" argnam Type.pp argt
      ) f.args;
      Format.fprintf pf "@ :@ %a@ =@ %a@]@ in@ %a"
        Type.pp (snd f.name) (pp_prec 100) f.body (pp_prec 100) y;
      cond_paren_close (prec < 100) pf
  | App (f, args) ->
      cond_paren_open (prec < 10) pf;
      Format.fprintf pf "@[<2>%a" (pp_prec 9) f;
      List.iter (fun arg ->
        Format.fprintf pf "@ %a" (pp_prec 0) arg
      ) args;
      Format.fprintf pf "@]";
      cond_paren_close (prec < 10) pf
  | Tuple [] -> Format.fprintf pf "()"
  | Tuple (tvals_hd :: tvals) ->
      cond_paren_open (prec < 110) pf;
      Format.fprintf pf "@[<2>(%a" (pp_prec 109) tvals_hd;
      List.iter (fun tval ->
        Format.fprintf pf ",@ %a" (pp_prec 109) tval
      ) tvals;
      Format.fprintf pf ")@]";
      cond_paren_close (prec < 110) pf
  | LetTuple (tpats, tv, y) ->
      cond_paren_open (prec < 100) pf;
      Format.fprintf pf "@[<2>let@ (";
      begin match tpats with
      | [] -> ()
      | ((tpatnam_hd, tpatt_hd) :: tpats_tl) ->
          Format.fprintf pf "@[<2>(%s@ :@ %a)@]" tpatnam_hd Type.pp tpatt_hd;
          List.iter (fun (tpatnam, tpatt) ->
            Format.fprintf pf ",@ @[<2>(%s@ :@ %a)@]" tpatnam Type.pp tpatt
          ) tpats_tl
      end;
      Format.fprintf pf "@ =@ %a@]@ in@ %a"
        (pp_prec 100) tv (pp_prec 100) y;
      cond_paren_close (prec < 100) pf
  | Array (x, y) ->
      cond_paren_printf (prec < 10) pf "@[<2>Array.create %a@ %a@]"
        (pp_prec 0) x (pp_prec 0) y
  | Get (x, y) ->
      cond_paren_printf (prec < 0) pf "@[<2>%a.(%a)@]"
        (pp_prec 0) x (pp_prec 100) y
  | Put (x, y, z) ->
      cond_paren_printf (prec < 70) pf "@[<2>%a.(%a)@ <-@ %a@]"
        (pp_prec 0) x (pp_prec 100) y (pp_prec 40) z

let pp = pp_prec 100
