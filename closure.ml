open Loc

type closure = { entry : Id.l; actual_fv : Id.t list }
type t = t_real Loc.loc
and t_real = (* クロージャ変換後の式 (caml2html: closure_t) *)
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
type fundef = { name : Id.l * Type.t;
		args : (Id.t * Type.t) list;
		formal_fv : (Id.t * Type.t) list;
		body : t }
type prog = Prog of fundef list * t

let rec fv el =
  match el.loc_val with
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

let rec g env known el = (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  loc_inherit el begin match el.loc_val with
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
	 xに自由変数がない(closureを介さずdirectに呼び出せる)
	 と仮定し、knownに追加してe1をクロージャ変換してみる *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
	if S.is_empty zs then known', e1' else
	(* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
	(Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
	 Format.eprintf "function %s cannot be directly applied in fact@." x;
	 toplevel := toplevel_backup;
	 let e1' = g (M.add_list yts env') known e1 in
	 known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
      toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') then (* xが変数としてe2'に出現するか *)
	MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
      else
	(Format.eprintf "eliminating closure(s) %s@." x;
	 e2'.loc_val) (* 出現しなければMakeClsを削除 *)
  | KNormal.App(x, ys) when S.mem x known -> (* 関数適用の場合 (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      AppDir(Id.L(x), ys)
  | KNormal.App(f, xs) -> AppCls(f, xs)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("min_caml_" ^ x), ys)
  end

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')

let rec pp_t pf el =
  match el.loc_val with
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
	a b pp_t y pp_t z
  | IfLE (a, b, y, z) ->
      Format.fprintf pf
	"@[<hv>@[<2>if@ @[%s@ <=@ %s@]@]@ @[<2>then@ %a@]@ @[<2>else@ %a@]@ @[<2>endif@]@]"
	a b pp_t y pp_t z
  | Let ((a, ta), y, z) ->
      Format.fprintf pf "@[<hv>@[<2>let@ %s@ :@ %a@ =@]@;<1 2>%a@ in@ %a@]"
	a Type.pp ta pp_t y pp_t z
  | Var a -> Format.fprintf pf "%s" a
  | MakeCls ((a, ta), cl, z) ->
      let cl_entry = match cl.entry with Id.L id -> id in
      Format.fprintf pf "@[<hv>@[<2>let@ %s@ :@ %a@ =@]@;<1 2>(%s@ #%s"
	a Type.pp ta "@closure" cl_entry;
      List.iter (fun fv ->
	Format.fprintf pf "@ %s" fv
      ) cl.actual_fv;
      Format.fprintf pf ")@ in@ %a@]" pp_t z
  | AppCls (cl_id, args) ->
      Format.fprintf pf "@[%s@ #%s" "@apply_closure" cl_id;
      List.iter (fun arg ->
	Format.fprintf pf "@ %s" arg
      ) args;
      Format.fprintf pf "@]"
  | AppDir (Id.L dir_id, args) ->
      Format.fprintf pf "@[%s@ #%s" "@apply" dir_id;
      List.iter (fun arg ->
	Format.fprintf pf "@ %s" arg
      ) args;
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
      Format.fprintf pf ")@ =@]@;<1 2>%s@ in@ %a@]" tname pp_t cont
  | Get (y, z) ->
      Format.fprintf pf "@[%s.(%s)@]" y z
  | Put (y, z, w) ->
      Format.fprintf pf "@[%s.(%s)@ <-@ %s@]" y z w
  | ExtArray (Id.L y) ->
      Format.fprintf pf "@[(%s@ %s)@]" "@ext_array" y

let pp_fundef pf f =
  let fname = match fst f.name with Id.L id -> id in
  Format.fprintf pf "@[<v>@[<2>let rec@ %s" fname;
  List.iter (fun (argnam, argt) ->
    Format.fprintf pf "@ @[<2>(%s@ :@ %a)@]" argnam Type.pp argt
  ) f.args;
  List.iter (fun (argnam, argt) ->
    Format.fprintf pf "@ @[<2>(free %s@ :@ %a)@]" argnam Type.pp argt
  ) f.formal_fv;
  Format.fprintf pf "@ :@ %a@ =@]@;<1 2>%a@]"
    Type.pp (snd f.name) pp_t f.body

let pp pf = function Prog (p, ep) ->
  Format.fprintf pf "@[<v>";
  List.iter (fun f ->
    Format.fprintf pf "%a;;@ " pp_fundef f
  ) p;
  Format.fprintf pf "@[<v>@[<2>let@ _@ =@]@;<1 2>%a@]" pp_t ep;
  Format.fprintf pf "@]"
