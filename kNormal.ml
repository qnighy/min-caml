(* give names to intermediate values (K-normalization) *)

open Loc

type t = t_real loc
and t_real = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec fv el = (* 式に出現する（自由な）変数 (caml2html: knormal_fv) *)
  match el.loc_val with
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e.loc_val with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      loc_inherit e (Let((x, t), e, e')), t'

let rec g env el = (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  match el.loc_val with
  | Syntax.Unit -> loc_inherit el Unit, Type.Unit
  | Syntax.Bool(b) ->
      loc_inherit el (Int(if b then 1 else 0)), Type.Int (* 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> loc_inherit el (Int(i)), Type.Int
  | Syntax.Float(d) -> loc_inherit el (Float(d)), Type.Float
  | Syntax.Not(e) ->
      g env (loc_dummy (Syntax.If(e, loc_dummy (Syntax.Bool(false)), loc_dummy (Syntax.Bool(true)))))
  | Syntax.Neg(e) ->
      let _, t as g_e = g env e in
      insert_let g_e
	(fun x ->
	  loc_inherit el begin match t with
	  | Type.Int -> Neg(x)
	  | Type.Float -> FNeg(x)
	  | _ -> failwith "neg operand is not Int nor Float"
	  end, t)
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)
      let _, t1 as g_e1 = g env e1 in
      insert_let g_e1
	(fun x -> insert_let (g env e2)
	    (fun y ->
	      loc_inherit el begin match t1 with
	      | Type.Int -> Add(x, y)
	      | Type.Float -> FAdd(x, y)
	      | _ -> failwith "add operand is not Int nor Float"
	      end, t1))
  | Syntax.Sub(e1, e2) ->
      let _, t1 as g_e1 = g env e1 in
      insert_let g_e1
	(fun x -> insert_let (g env e2)
	    (fun y ->
	      loc_inherit el begin match t1 with
	      | Type.Int -> Sub(x, y)
	      | Type.Float -> FSub(x, y)
	      | _ -> failwith "sub operand is not Int nor Float"
	      end, t1))
  | Syntax.FNeg(e) ->
      insert_let (g env e)
	(fun x -> loc_inherit el (FNeg(x)), Type.Float)
  | Syntax.FAdd(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> loc_inherit el (FAdd(x, y)), Type.Float))
  | Syntax.FSub(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> loc_inherit el (FSub(x, y)), Type.Float))
  | Syntax.FMul(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> loc_inherit el (FMul(x, y)), Type.Float))
  | Syntax.FDiv(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> loc_inherit el (FDiv(x, y)), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
      g env (loc_dummy (Syntax.If(loc_dummy cmp, loc_dummy (Syntax.Bool(true)), loc_dummy (Syntax.Bool(false)))))
  | Syntax.If({loc_val = Syntax.Not(e1)}, e2, e3) ->
      g env (loc_dummy (Syntax.If(e1, e3, e2))) (* notによる分岐を変換 (caml2html: knormal_not) *)
  | Syntax.If({loc_val = Syntax.Eq(e1, e2)}, e3, e4) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y ->
	      let e3', t3 = g env e3 in
	      let e4', t4 = g env e4 in
	      loc_inherit el (IfEq(x, y, e3', e4')), t3))
  | Syntax.If({loc_val = Syntax.LE(e1, e2)}, e3, e4) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y ->
	      let e3', t3 = g env e3 in
	      let e4', t4 = g env e4 in
	      loc_inherit el (IfLE(x, y, e3', e4')), t3))
  | Syntax.If(e1, e2, e3) ->
      g env (loc_dummy (Syntax.If(loc_dummy(Syntax.Eq(e1, loc_dummy(Syntax.Bool(false)))), e3, e2))) (* 比較のない分岐を変換 (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      loc_inherit el (Let((x, t), e1', e2')), t2
  | Syntax.Var(x) when M.mem x env -> loc_inherit el (Var(x)), M.find x env
  | Syntax.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> loc_inherit el (ExtArray x), t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      loc_inherit el (LetRec({ name = (x, t); args = yts; body = e1' }, e2')), t2
  | Syntax.App({loc_val = Syntax.Var(f)}, e2s) when not (M.mem f env) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      (match M.find f !Typing.extenv with
      | Type.Fun(_, t) ->
	  let rec bind xs = function (* "xs" are identifiers for the arguments *)
	    | [] -> loc_inherit el (ExtFunApp(f, xs)), t
	    | e2 :: e2s ->
		insert_let (g env e2)
		  (fun x -> bind (xs @ [x]) e2s) in
	  bind [] e2s (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
      | _, Type.Fun(_, t) as g_e1 ->
	  insert_let g_e1
	    (fun f ->
	      let rec bind xs = function (* "xs" are identifiers for the arguments *)
		| [] -> loc_inherit el (App(f, xs)), t
		| e2 :: e2s ->
		    insert_let (g env e2)
		      (fun x -> bind (xs @ [x]) e2s) in
	      bind [] e2s) (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
	| [] -> loc_inherit el (Tuple(xs)), Type.Tuple(ts)
	| e :: es ->
	    let _, t as g_e = g env e in
	    insert_let g_e
	      (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (g env e1)
	(fun y ->
	  let e2', t2 = g (M.add_list xts env) e2 in
	  loc_inherit el (LetTuple(xts, y, e2')), t2)
  | Syntax.Array(e1, e2) ->
      insert_let (g env e1)
	(fun x ->
	  let _, t2 as g_e2 = g env e2 in
	  insert_let g_e2
	    (fun y ->
	      let l =
		match t2 with
		| Type.Float -> "create_float_array"
		| _ -> "create_array" in
	      loc_inherit el (ExtFunApp(l, [x; y])), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
      (match g env e1 with
      |	_, Type.Array(t) as g_e1 ->
	  insert_let g_e1
	    (fun x -> insert_let (g env e2)
		(fun y -> loc_inherit el (Get(x, y)), t))
      | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> insert_let (g env e3)
		(fun z -> loc_inherit el (Put(x, y, z)), Type.Unit)))

let f e = fst (g M.empty e)

let rec pp pf x =
  begin match x.loc_val with
  | Unit -> Format.fprintf pf "()"
  | Int i -> Format.fprintf pf "%d" i
  | Float f -> Format.fprintf pf "%.1g" f
  | Neg y -> Format.fprintf pf "@[-@,%s@]" y
  | Add (y, z) -> Format.fprintf pf "@[%s@ +@ %s@]" y z
  | Sub (y, z) -> Format.fprintf pf "@[%s@ -@ %s@]" y z
  | FNeg y -> Format.fprintf pf "@[.-@,%s@]" y
  | FAdd (y, z) -> Format.fprintf pf "@[%s@ .+@ %s@]" y z
  | FSub (y, z) -> Format.fprintf pf "@[%s@ .-@ %s@]" y z
  | FMul (y, z) -> Format.fprintf pf "@[%s@ .*@ %s@]" y z
  | FDiv (y, z) -> Format.fprintf pf "@[%s@ ./@ %s@]" y z
  | IfEq (a, b, y, z) ->
      Format.fprintf pf
	"@[<hv>@[<2>if@ @[%s@ =@ %s@]@]@ @[<2>then@ %a@]@ @[<2>else@ %a@]@ @[<2>endif@]@]"
	a b pp y pp z
  | IfLE (a, b, y, z) ->
      Format.fprintf pf
	"@[<hv>@[<2>if@ @[%s@ <=@ %s@]@]@ @[<2>then@ %a@]@ @[<2>else@ %a@]@ @[<2>endif@]@]"
	a b pp y pp z
  | Let ((a, ta), y, z) ->
      Format.fprintf pf "@[<hv>@[<2>let@ %s@ :@ %a@ =@]@;<1 2>%a@ in@ %a@]"
	a Type.pp ta pp y pp z
  | Var a -> Format.fprintf pf "%s" a
  | LetRec (f, z) ->
      Format.fprintf pf "@[<hv>@[<2>let rec@ %s" (fst f.name);
      List.iter (fun (argnam, argt) ->
	Format.fprintf pf "@ @[<2>(%s@ :@ %a)@]" argnam Type.pp argt
      ) f.args;
      Format.fprintf pf "@ :@ %a@ =@]@;<1 2>%a@ in@ %a@]"
	Type.pp (snd f.name) pp f.body pp z
  | App (y, z) ->
      Format.fprintf pf "@[%s" y;
      List.iter (fun v ->
	Format.fprintf pf "@ %s" v
      ) z;
      Format.fprintf pf "@]"
  | Tuple [] -> Format.fprintf pf "@[()@]"
  | Tuple (tv_hd :: tv_tl) ->
      Format.fprintf pf "@[(%s" tv_hd;
      List.iter (fun v ->
	Format.fprintf pf ",@ %s" v
      ) tv_tl;
      Format.fprintf pf ")@]"
  | LetTuple (tvs, tname, cont) ->
      Format.fprintf pf "@[<hv>@[<2>let tuple@ (";
      begin match tvs with
      | [] -> ()
      | ((elemnam_hd, elemt_hd) :: tvs_tl) ->
	  Format.fprintf pf "@[<2>%s@ :@ %a@]" elemnam_hd Type.pp elemt_hd;
	  List.iter (fun (elemnam, elemt) ->
	    Format.fprintf pf ",@ @[<2>%s@ :@ %a@]" elemnam Type.pp elemt
	  ) tvs_tl
      end;
      Format.fprintf pf ")@ =@]@;<1 2>%s@ in@ %a@]" tname pp cont
  | Get (y, z) ->
      Format.fprintf pf "@[%s.(%s)@]" y z
  | Put (y, z, w) ->
      Format.fprintf pf "@[%s.(%s)@ <-@ %s@]" y z w
  | ExtArray y ->
      Format.fprintf pf "@[%s%s@]" "@@" y
  | ExtFunApp (y, z) ->
      Format.fprintf pf "@[%s%s" "@" y;
      List.iter (fun v ->
	Format.fprintf pf "@ %s" v
      ) z;
      Format.fprintf pf "@]"
  end
