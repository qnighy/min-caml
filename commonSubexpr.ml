open Loc
open KNormal

let lookup_lim = 10

let rec t_eql el1 el2 =
  match el1.loc_val, el2.loc_val with
  | Unit, Unit -> true
  | Int i1, Int i2 -> i1 = i2
  | Float f1, Float f2 ->f1 = f2 && 1.0 /. f1 = 1.0 /. f2
  | Neg x1, Neg x2 | FNeg x1, FNeg x2 -> x1 = x2
  | Add (x1, y1), Add (x2, y2)
  | FAdd (x1, y1), FAdd (x2, y2)
  | FMul (x1, y1), FMul (x2, y2) ->
      (x1 = x2 && y1 = y2) || (x1 = y2 && y1 = x2)
  | Sub (x1, y1), Sub (x2, y2)
  | FSub (x1, y1), FSub (x2, y2)
  | FDiv (x1, y1), FDiv (x2, y2) -> x1 = x2 && y1 = y2
  | IfEq (x1, y1, e11, e12), IfEq (x2, y2, e21, e22) ->
      ((x1 = x2 && y1 = y2) || (x1 = y2 && y1 = x2)) &&
	t_eql e11 e21 && t_eql e12 e22
  | IfLE (x1, y1, e11, e12), IfLE (x2, y2, e21, e22) ->
      (x1 = x2 && y1 = y2) && t_eql e11 e21 && t_eql e12 e22
  | Var x1, Var x2 -> x1 = x2
  | App (x1, ys1), App (x2, ys2) -> x1 = x2 && ys1 = ys2
  | Tuple xs1, Tuple xs2 -> xs1 = xs2
  | ExtArray x1, ExtArray x2 -> x1 = x2
  | _, _ -> false

let rec find_hist lim hist v =
  if lim <= 0 then None else
    match hist with
    | [] -> None
    | ((w, x) :: hist_tl) ->
	if t_eql w v then Some x else find_hist (lim - 1) hist_tl v

let rec is_pure pures el =
  match el.loc_val with
  | Unit | Int _ | Float _ | Neg _ | Add _ | Sub _ | FNeg _ | FAdd _ | FSub _
  | FMul _ | FDiv _ -> true
  | IfEq (_, _, e1, e2) | IfLE (_, _, e1, e2) | Let (_, e1, e2) ->
      is_pure pures e1 && is_pure pures e2
  | Var _ -> true
  | LetRec _ -> true
  | App (x, _) -> S.mem x pures
  | Tuple _ -> true
  | LetTuple (_, _, e1) -> is_pure pures e1
  | Get _ | Put _ -> false
  | ExtArray _ -> true
  | ExtFunApp _ -> false

let rec g pures hist el =
  let (x,y) = begin match el.loc_val with
  | Unit | Int _ | Float _ | Neg _ | Add _ | Sub _ | FNeg _ | FAdd _ | FSub _
  | FMul _ | FDiv _ -> (el.loc_val, true)
  | IfEq (x, y, e1, e2) ->
      let (e1', e1pure) = g pures hist e1 in
      let (e2', e2pure) = g pures hist e2 in
      (IfEq (x, y, e1', e2'), e1pure && e2pure)
  | IfLE (x, y, e1, e2) ->
      let (e1', e1pure) = g pures hist e1 in
      let (e2', e2pure) = g pures hist e2 in
      (IfLE (x, y, e1', e2'), e1pure && e2pure)
  | Let ((x, t), e1, e2) ->
      let (e1', e1pure) = g pures hist e1 in
      let pures' = if e1pure then S.add x pures else pures in
      let (e2', e2pure) = g pures' ((e1', x) :: hist) e2 in
      begin match find_hist lookup_lim hist e1 with
      | Some y when is_pure pures e1 ->
	  (Let ((x, t), loc_dummy (Var y), e2'), e2pure)
      | _ ->
	  (Let ((x, t), e1', e2'), e2pure)
      end
  | Var x -> (el.loc_val, S.mem x pures)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      (* let recの外の定義を使うのはまずいっぽい *)
      (* let (e1', e1pure) = g (S.add x pures) hist e1 in *)
      let (e1', e1pure) = g (S.add x pures) [] e1 in
      let pures' = if e1pure then S.add x pures else pures in
      let (e2', e2pure) = g pures' ((e1', x) :: hist) e2 in
      (LetRec ({ name = (x, t); args = yts; body = e1' }, e2'), e2pure)
  | App (x, ys) -> (el.loc_val, S.mem x pures)
  | Tuple xs -> (el.loc_val, List.for_all (fun x -> S.mem x pures) xs)
  | LetTuple (xts, y, e1) ->
      let pures' =
	if S.mem y pures then
	  List.fold_right S.add (List.map fst xts) pures
	else pures
      in
      let (e1', e1pure) = g pures' hist e1 in
      (LetTuple (xts, y, e1'), e1pure)
  | Get _ | Put _ -> (el.loc_val, false)
  | ExtArray _ -> (el.loc_val, true)
  | ExtFunApp _ -> (el.loc_val, false)
  end
  in
  (loc_inherit el x, y)

let f e = fst (g S.empty [] e)
