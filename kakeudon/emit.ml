open Asm

let int_repr_of_float x =
  Int32.to_int (Int32.bits_of_float x) land ((1 lsl 32) - 1)

let stackset = ref S.empty (* すでに Save された変数の集合 *)
let stackmap = ref [] (* Save された変数のスタックにおける位置 *)
let save x = 
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let locate x = 
  let rec loc = function 
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
    loc !stackmap
let stacksize () = align ((List.length !stackmap + 1) * 4)
let offset x = -4 - 4 * List.hd (locate x)

let reg r = 
  if is_reg r 
  then r
  else "!" ^ r

let load_label r label =
  Printf.sprintf "\tla\t%s, %s\n" (reg r) label

(* 関数呼び出しのために引数を並べ替える (register shuffling) *)
let rec shuffle sw xys = 
  (* remove identical moves *)
  let (_, xys) = List.partition (fun (x, y) -> x = y) xys in
    (* find acyclic moves *)
    match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
      | ([], []) -> []
      | ((x, y) :: xys, []) -> (* no acyclic moves; resolve a cyclic move *)
	  (y, sw) :: (x, y) :: 
	    shuffle sw (List.map (function 
				    | (y', z) when y = y' -> (sw, z)
				    | yz -> yz) xys)
      | (xys, acyc) -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t (* 末尾かどうかを表すデータ型 *)
let rec g oc = function (* 命令列のアセンブリ生成 *)
  | (dest, Ans (exp)) -> g' oc (dest, exp)
  | (dest, Let((x, t), exp, e)) -> g' oc (NonTail (x), exp); g oc (dest, e)
and g' oc = function (* 各命令のアセンブリ生成 *)
    (* 末尾でなかったら計算結果を dest にセット *)
  | (NonTail(_), Nop) -> ()
  | (NonTail(x), Li(i)) ->
      Printf.fprintf oc "\tli\t%s, %d\n" (reg x) i
  | (NonTail(x), FLi(i)) ->
      Printf.fprintf oc "\tli\t%s, %d\n" reg_tmp i;
      Printf.fprintf oc "\tmtc1\t%s, %s\n" reg_tmp (reg x)
  | (NonTail(x), SetL(Id.L(y))) -> 
      let s = load_label x y in
      Printf.fprintf oc "%s" s
  | (NonTail(x), Mr(y)) when x = y -> ()
  | (NonTail(x), Mr(y)) -> Printf.fprintf oc "\tmov\t%s, %s\n" (reg x) (reg y)
  | (NonTail(x), Neg(y)) -> Printf.fprintf oc "\tsubu\t%s, $zero, %s\n" (reg x) (reg y)
  | (NonTail(x), Add(y, V(z))) -> 
      Printf.fprintf oc "\taddu\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Add(y, C(z))) -> 
      Printf.fprintf oc "\taddiu\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(x), Sub(y, V(z))) -> 
      Printf.fprintf oc "\tsubu\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Sub(y, C(z))) -> 
      Printf.fprintf oc "\taddiu\t%s, %s, %d\n" (reg x) (reg y) (-z)
  | (NonTail(x), Mul(y, C(8))) -> 
      Printf.fprintf oc "\tsll\t%s, %s, 3\n" (reg x) (reg y)
  | (NonTail(x), Mul(y, C(4))) -> 
      Printf.fprintf oc "\tsll\t%s, %s, 2\n" (reg x) (reg y)
  | (NonTail(x), Mul(y, C(2))) -> 
      Printf.fprintf oc "\tsll\t%s, %s, 1\n" (reg x) (reg y)
  | (NonTail(x), Mul(y, C(_))) -> failwith "TODO: mul(C)"
  | (NonTail(x), Mul(y, V(_))) -> failwith "TODO: mul(V)"
  | (NonTail(x), Div(y, C(2))) -> 
      Printf.fprintf oc "\tsra\t%s, %s, 1\n" (reg x) (reg y)
  | (NonTail(x), Div(y, _)) -> failwith "TODO: div"
  | (NonTail(x), Slw(y, V(z))) -> 
      Printf.fprintf oc "\tsllv\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Slw(y, C(z))) -> 
      Printf.fprintf oc "\tsll\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(x), Lwz(y, V(z))) ->
      Printf.fprintf oc "\taddu\t$at, %s, %s\n" (reg y) (reg z);
      Printf.fprintf oc "\tlw\t%s, 0($at)\n" (reg x)
  | (NonTail(x), Lwz(y, C(z))) -> 
      Printf.fprintf oc "\tlw\t%s, %d(%s)\n" (reg x) z (reg y)
  | (NonTail(_), Stw(x, y, V(z))) ->
      Printf.fprintf oc "\taddu\t$at, %s, %s\n" (reg y) (reg z);
      Printf.fprintf oc "\tsw\t%s, 0($at)\n" (reg x)
  | (NonTail(_), Stw(x, y, C(z))) -> 
      Printf.fprintf oc "\tsw\t%s, %d(%s)\n" (reg x) z (reg y)
  | (NonTail(x), FMr(y)) when x = y -> ()
  | (NonTail(x), FMr(y)) -> Printf.fprintf oc "\tmov.s\t%s, %s\n" (reg x) (reg y)
  | (NonTail(x), FNeg(y)) -> 
      (* TODO *)
      Printf.fprintf oc "\tsub.s\t%s, $f31, %s\n" (reg x) (reg y)
  | (NonTail(x), FAdd(y, z)) -> 
      Printf.fprintf oc "\tadd.s\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FSub(y, z)) -> 
      Printf.fprintf oc "\tsub.s\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FMul(y, z)) -> 
      Printf.fprintf oc "\tmul.s\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FDiv(y, z)) -> 
      Printf.fprintf oc "\tdiv.s\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Lfd(y, V(z))) ->
      Printf.fprintf oc "\taddu\t$at, %s, %s\n" (reg y) (reg z);
      Printf.fprintf oc "\tlwc1\t%s, 0($at)\n" (reg x)
  | (NonTail(x), Lfd(y, C(z))) -> 
      Printf.fprintf oc "\tlwc1\t%s, %d(%s)\n" (reg x) z (reg y)
  | (NonTail(_), Stfd(x, y, V(z))) ->
      Printf.fprintf oc "\taddu\t$at, %s, %s\n" (reg y) (reg z);
      Printf.fprintf oc "\tswc1\t%s, 0($at)\n" (reg x)
  | (NonTail(_), Stfd(x, y, C(z))) ->
      Printf.fprintf oc "\tswc1\t%s, %d(%s)\n" (reg x) z (reg y)
  | (NonTail(_), Comment(s)) -> Printf.fprintf oc "#\t%s\n" s
  (* 退避の仮想命令の実装 *)
  | (NonTail(_), Save(x, y))
      when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
	Printf.fprintf oc "\tsw\t%s, %d(%s)\n" (reg x) (offset y) reg_sp
  | (NonTail(_), Save(x, y)) 
      when List.mem x allfregs && not (S.mem y !stackset) ->
      save y;
	Printf.fprintf oc "\tswc1\t%s, %d(%s)\n" (reg x) (offset y) reg_sp
  | (NonTail(_), Save(x, y)) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 *)
  | (NonTail(x), Restore(y)) when List.mem x allregs ->
      Printf.fprintf oc "\tlw\t%s, %d(%s)\n" (reg x) (offset y) reg_sp
  | (NonTail(x), Restore(y)) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tlwc1\t%s, %d(%s)\n" (reg x) (offset y) reg_sp
  (* 末尾だったら計算結果を第一レジスタにセット *)
  | (Tail, (Nop | Stw _ | Stfd _ | Comment _ | Save _ as exp)) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tjr $ra\n";
  | (Tail, (Li _ | SetL _ | Mr _ | Neg _ | Add _ | Sub _ | Slw _ |
            Lwz _ as exp)) -> 
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tjr $ra\n";
  | (Tail, (FLi _ | FMr _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ |
            Lfd _ as exp)) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tjr $ra\n";
  | (Tail, (Restore(x) as exp)) ->
      (match locate x with
	 | [i] -> g' oc (NonTail(regs.(0)), exp)
	 | [i; j] when (i + 1 = j) -> g' oc (NonTail(fregs.(0)), exp)
	 | _ -> assert false);
      Printf.fprintf oc "\tjr $ra\n";
  | (Tail, IfEq(x, V(y), e1, e2)) ->
      g'_tail_if oc e1 e2 "beq" "bne" (reg x) (reg y)
  | (Tail, IfEq(x, C(y), e1, e2)) -> failwith "todo"
  | (Tail, IfLE(x, V(y), e1, e2)) ->
      g'_tail_if oc e1 e2 "ble" "bgt" (reg x) (reg y)
  | (Tail, IfLE(x, C(y), e1, e2)) -> failwith "todo"
  | (Tail, IfGE(x, V(y), e1, e2)) ->
      g'_tail_if oc e1 e2 "bge" "blt" (reg x) (reg y)
  | (Tail, IfGE(x, C(y), e1, e2)) -> failwith "todo"
  | (Tail, IfFEq(x, y, e1, e2)) ->
      Printf.fprintf oc "\tc.eq.s\t%s, %s\n" (reg x) (reg y);
      g'_tail_if_fp oc e1 e2 "bc1t" "bc1f"
  | (Tail, IfFLE(x, y, e1, e2)) ->
      Printf.fprintf oc "\tc.ole.s\t%s, %s\n" (reg x) (reg y);
      g'_tail_if_fp oc e1 e2 "bc1t" "bc1f"
  | (NonTail(z), IfEq(x, V(y), e1, e2)) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne" (reg x) (reg y)
  | (NonTail(z), IfEq(x, C(y), e1, e2)) -> failwith "todo"
  | (NonTail(z), IfLE(x, V(y), e1, e2)) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt" (reg x) (reg y)
  | (NonTail(z), IfLE(x, C(y), e1, e2)) -> failwith "todo"
  | (NonTail(z), IfGE(x, V(y), e1, e2)) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "blt" (reg x) (reg y)
  | (NonTail(z), IfGE(x, C(y), e1, e2)) -> failwith "todo"
  | (NonTail(z), IfFEq(x, y, e1, e2)) ->
      Printf.fprintf oc "\tc.eq.s\t%s, %s\n" (reg x) (reg y);
      g'_non_tail_if_fp oc (NonTail(z)) e1 e2 "bc1t" "bc1f"
  | (NonTail(z), IfFLE(x, y, e1, e2)) ->
      Printf.fprintf oc "\tc.ole.s\t%s, %s\n" (reg x) (reg y);
      g'_non_tail_if_fp oc (NonTail(z)) e1 e2 "bc1t" "bc1f"
  (* 関数呼び出しの仮想命令の実装 *)
  | (Tail, CallCls(x, ys, zs)) -> (* 末尾呼び出し *)
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tlw\t%s, 0(%s)\n" (reg reg_sw) (reg reg_cl);
      Printf.fprintf oc "\tjr\t%s\n" (reg reg_sw);
  | (Tail, CallDir(Id.L(x), ys, zs)) -> (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tj\t%s\n" x
  | (NonTail(a), CallCls(x, ys, zs)) ->
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
	Printf.fprintf oc "\tsw\t$ra, %d(%s)\n" (-4 - (ss - 4)) reg_sp;
	Printf.fprintf oc "\taddiu\t%s, %s, %d\n" reg_sp reg_sp (-ss);
	Printf.fprintf oc "\tlw\t$at, 0(%s)\n" (reg reg_cl);
	Printf.fprintf oc "\tjalr\t$at\n";
	Printf.fprintf oc "\taddiu\t%s, %s, %d\n" reg_sp reg_sp (ss);
	Printf.fprintf oc "\tlw\t$ra, %d(%s)\n" (-4 - (ss - 4)) reg_sp;
	(if List.mem a allregs && a <> regs.(0) then 
	   Printf.fprintf oc "\tmov\t%s, %s\n" (reg a) (reg regs.(0)) 
	 else if List.mem a allfregs && a <> fregs.(0) then 
	   Printf.fprintf oc "\tmov.s\t%s, %s\n" (reg a) (reg fregs.(0)))
  | (NonTail(a), CallDir(Id.L(x), ys, zs)) -> 
      g'_args oc [] ys zs;
      let ss = stacksize () in
	Printf.fprintf oc "\tsw\t$ra, %d(%s)\n" (-4 - (ss - 4)) reg_sp;
	Printf.fprintf oc "\taddiu\t%s, %s, %d\n" reg_sp reg_sp (-ss);
	Printf.fprintf oc "\tjal\t%s\n" x;
	Printf.fprintf oc "\taddiu\t%s, %s, %d\n" reg_sp reg_sp (ss);
	Printf.fprintf oc "\tlw\t$ra, %d(%s)\n" (-4 - (ss - 4)) reg_sp;
	(if List.mem a allregs && a <> regs.(0) then
	   Printf.fprintf oc "\tmov\t%s, %s\n" (reg a) (reg regs.(0))
	 else if List.mem a allfregs && a <> fregs.(0) then
	   Printf.fprintf oc "\tmov.s\t%s, %s\n" (reg a) (reg fregs.(0)))
and g'_tail_if oc e1 e2 b bn rs rt = 
  let b_else = Id.genid (b ^ "_else") in
    Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn rs rt b_else;
    let stackset_back = !stackset in
      g oc (Tail, e1);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e2)
and g'_tail_if_fp oc e1 e2 b bn = 
  let b_else = Id.genid (b ^ "_else") in
    Printf.fprintf oc "\t%s\t%s\n" bn b_else;
    let stackset_back = !stackset in
      g oc (Tail, e1);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e2)
and g'_non_tail_if oc dest e1 e2 b bn rs rt = 
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
    Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn rs rt b_else;
    let stackset_back = !stackset in
      g oc (dest, e1);
      let stackset1 = !stackset in
	Printf.fprintf oc "\tj\t%s\n" b_cont;
	Printf.fprintf oc "%s:\n" b_else;
	stackset := stackset_back;
	g oc (dest, e2);
	Printf.fprintf oc "%s:\n" b_cont;
	let stackset2 = !stackset in
	  stackset := S.inter stackset1 stackset2
and g'_non_tail_if_fp oc dest e1 e2 b bn = 
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
    Printf.fprintf oc "\t%s\t%s\n" bn b_else;
    let stackset_back = !stackset in
      g oc (dest, e1);
      let stackset1 = !stackset in
	Printf.fprintf oc "\tj\t%s\n" b_cont;
	Printf.fprintf oc "%s:\n" b_else;
	stackset := stackset_back;
	g oc (dest, e2);
	Printf.fprintf oc "%s:\n" b_cont;
	let stackset2 = !stackset in
	  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs = 
  let (i, yrs) = 
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl) ys in
    List.iter
      (fun (y, r) -> Printf.fprintf oc "\tmov\t%s, %s\n" (reg r) (reg y))
      (shuffle reg_sw yrs);
    let (d, zfrs) = 
      List.fold_left
	(fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
	(0, []) zs in
      List.iter
        (fun (z, fr) -> Printf.fprintf oc "\tmov.s\t%s, %s\n" (reg fr) (reg z))
	(shuffle reg_fsw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  (if data <> [] then
    (List.iter
       (fun (Id.L(x), d) ->
	 Printf.fprintf oc "\t.align 4\n";
	 Printf.fprintf oc "%s:\t # %f\n" x d;
	 Printf.fprintf oc "\t.long\t%u\n" (int_repr_of_float d))
       data));
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "\t.globl _min_caml_start\n";
  Printf.fprintf oc "_min_caml_start: # main entry point\n";
  Printf.fprintf oc "\t# main program start\n";
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail("_R_0"), e);
  Printf.fprintf oc "\t# main program end\n";
  Printf.fprintf oc "\tjr\t$ra\n"
