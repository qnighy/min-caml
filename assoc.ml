(* flatten let-bindings (just for prettier printing) *)

open Loc
open KNormal

let rec f el = (* �ͥ��Ȥ���let�δ��� (caml2html: assoc_f) *)
  loc_inherit el begin match el.loc_val with
  | IfEq(x, y, e1, e2) -> IfEq(x, y, f e1, f e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, f e1, f e2)
  | Let(xt, e1, e2) -> (* let�ξ�� (caml2html: assoc_let) *)
      let rec insert el =
	loc_inherit el begin match el.loc_val with
	| Let(yt, e3, e4) -> Let(yt, e3, insert e4)
	| LetRec(fundefs, e) -> LetRec(fundefs, insert e)
	| LetTuple(yts, z, e) -> LetTuple(yts, z, insert e)
	| _ -> Let(xt, el, f e2)
	end in
      (insert (f e1)).loc_val
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = xt; args = yts; body = f e1 }, f e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, f e)
  | e -> e
  end
