%{
(* parserが利用する変数、関数、型などの定義 *)
open Loc
open Syntax
let addtyp x = (x, Type.gentyp ())

let gen_loc x = {
  loc_val = x;
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
}
%}

/* 字句を表すデータ型の定義 (caml2html: parser_token) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* 開始記号の定義 */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { gen_loc Unit }
| BOOL
    { gen_loc (Bool($1)) }
| INT
    { gen_loc (Int($1)) }
| FLOAT
    { gen_loc (Float($1)) }
| IDENT
    { gen_loc (Var($1)) }
| simple_exp DOT LPAREN exp RPAREN
    { gen_loc (Get($1, $4)) }

exp: /* 一般の式 (caml2html: parser_exp) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { gen_loc (Not($2)) }
| MINUS exp
    %prec prec_unary_minus
    { match $2.loc_val with
    | Float(f) -> gen_loc (Float(-.f)) (* -1.23などは型エラーではないので別扱い *)
    | _ -> gen_loc (Neg($2)) }
| exp PLUS exp /* 足し算を構文解析するルール (caml2html: parser_add) */
    { gen_loc (Add($1, $3)) }
| exp MINUS exp
    { gen_loc (Sub($1, $3)) }
| exp EQUAL exp
    { gen_loc (Eq($1, $3)) }
| exp LESS_GREATER exp
    { gen_loc (Not(gen_loc(Eq($1, $3)))) }
| exp LESS exp
    { gen_loc (Not(gen_loc(LE($3, $1)))) }
| exp GREATER exp
    { gen_loc (Not(gen_loc(LE($1, $3)))) }
| exp LESS_EQUAL exp
    { gen_loc (LE($1, $3)) }
| exp GREATER_EQUAL exp
    { gen_loc (LE($3, $1)) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { gen_loc (If($2, $4, $6)) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { gen_loc (FNeg($2)) }
| exp PLUS_DOT exp
    { gen_loc (FAdd($1, $3)) }
| exp MINUS_DOT exp
    { gen_loc (FSub($1, $3)) }
| exp AST_DOT exp
    { gen_loc (FMul($1, $3)) }
| exp SLASH_DOT exp
    { gen_loc (FDiv($1, $3)) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { gen_loc (Let(addtyp $2, $4, $6)) }
| LET REC fundef IN exp
    %prec prec_let
    { gen_loc (LetRec($3, $5)) }
| exp actual_args
    %prec prec_app
    { gen_loc (App($1, $2)) }
| elems
    { gen_loc (Tuple($1)) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { gen_loc (LetTuple($3, $6, $8)) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { gen_loc (Put($1, $4, $7)) }
| exp SEMICOLON exp
    { gen_loc (Let((Id.gentmp Type.Unit, Type.Unit), $1, $3)) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { gen_loc (Array($2, $3)) }
| error
    { failwith
	(Printf.sprintf "parse error near %s"
	  (pos_str2
	    (Parsing.symbol_start_pos ())
	    (Parsing.symbol_end_pos ()))) }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }
