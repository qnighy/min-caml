open Loc
open KNormal

let rec effect el = (* �����Ѥ�̵ͭ (caml2html: elim_effect) *)
  match el.loc_val with
  | Let(_, e1, e2) | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) -> effect e1 || effect e2
  | LetRec(_, e) | LetTuple(_, _, e) -> effect e
  | App _ | Put _ | ExtFunApp _ -> true
  | _ -> false

let rec f el = (* �����������롼�������� (caml2html: elim_f) *)
  loc_inherit el begin match el.loc_val with
  | IfEq(x, y, e1, e2) -> IfEq(x, y, f e1, f e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, f e1, f e2)
  | Let((x, t), e1, e2) -> (* let�ξ�� (caml2html: elim_let) *)
      let e1' = f e1 in
      let e2' = f e2 in
      if effect e1' || S.mem x (fv e2') then Let((x, t), e1', e2') else
      (Format.eprintf "eliminating variable %s@." x;
       e2'.loc_val)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let rec�ξ�� (caml2html: elim_letrec) *)
      let e2' = f e2 in
      if S.mem x (fv e2') then
	LetRec({ name = (x, t); args = yts; body = f e1 }, e2')
      else
	(Format.eprintf "eliminating function %s@." x;
	 e2'.loc_val)
  | LetTuple(xts, y, e) ->
      let xs = List.map fst xts in
      let e' = f e in
      let live = fv e' in
      if List.exists (fun x -> S.mem x live) xs then LetTuple(xts, y, e') else
      (Format.eprintf "eliminating variables %s@." (Id.pp_list xs);
       e'.loc_val)
  | e -> e
  end
