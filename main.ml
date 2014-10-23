let limit = ref 1000

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = CommonSubexpr.f e in
  let e' = Beta.f e' in
  let e' = Assoc.f e' in
  let e' = Inline.f e' in
  let e' = ConstFold.f e' in
  let e' = Elim.f e' in
  if e = e' then e else
  iter (n - 1) e'

let debug_after_parser = ref false
let debug_after_typing = ref false
let debug_after_knormal = ref false
let debug_after_alpha = ref false
let debug_after_opt = ref false
let debug_after_closure = ref false
let debug_after_virtual = ref false
let debug_after_simm = ref false
let debug_after_reg_alloc = ref false

let lexbuf outchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  let e = Parser.exp Lexer.token l in
  (if !debug_after_parser then
    Format.eprintf "@[<v 2>DEBUG After Parser:@ %a@]@." Syntax.pp e);
  let e = Typing.f e in
  (if !debug_after_typing then
    Format.eprintf "@[<v 2>DEBUG After Typing:@ %a@]@." Syntax.pp e);
  let e = KNormal.f e in
  (if !debug_after_knormal then
    Format.eprintf "@[<v 2>DEBUG After KNormal:@ %a@]@." KNormal.pp e);
  let e = Alpha.f e in
  (if !debug_after_alpha then
    Format.eprintf "@[<v 2>DEBUG After Alpha:@ %a@]@." KNormal.pp e);
  let e = iter !limit e in
  (if !debug_after_opt then
    Format.eprintf "@[<v 2>DEBUG After opt:@ %a@]@." KNormal.pp e);
  let e = Closure.f e in
  (if !debug_after_closure then
    Format.eprintf "@[<v 2>DEBUG After Closure:@ %a@]@." Closure.pp e);
  let e = Virtual.f e in
  let e = Simm.f e in
  let e = RegAlloc.f e in
  Emit.f outchan e

let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i),
      "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i),
      "maximum number of optimizations iterated");
     ("-debug-after-parser", Arg.Set debug_after_parser,
      "show code after parsing");
     ("-debug-after-typing", Arg.Set debug_after_typing,
      "show code after typing");
     ("-debug-after-knormal", Arg.Set debug_after_knormal,
      "show code after K-normalization");
     ("-debug-after-alpha", Arg.Set debug_after_alpha,
      "show code after alpha translation");
     ("-debug-after-opt", Arg.Set debug_after_opt,
      "show code after optimization");
     ("-debug-after-closure", Arg.Set debug_after_closure,
      "show code after closure generation");
     ("-debug-after-virtual", Arg.Set debug_after_virtual,
      "show code after virtual");
     ("-debug-after-simm", Arg.Set debug_after_simm,
      "show code after simm");
     ("-debug-after-reg-alloc", Arg.Set debug_after_reg_alloc,
      "show code after register allocation");
     ]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
