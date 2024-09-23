/* *** G2d *** */
// 9 conflicts - 6 po's 3 assoc's
// - vs. *
// - vs. +
// + vs. *
// + vs. -
// * vs. +
// * vs. -
// - assoc
// + assoc
// * assoc

%{
open Ast;;

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a loc =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

%token EOF
%token <int64>  INT
%token <string> IDENT
%token <string> STRING
%token ELSE     /* else */
%token IF       /* if */
%token TINT     /* int */
%token RETURN   /* return */
%token WHILE    /* while */
%token SEMI     /* ; */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token PLUS     /* + */
%token DASH     /* - */
%token STAR     /* * */
%token EQ       /* = */
%token LPAREN   /* ( */
%token RPAREN   /* ) */

/* ---------------------------------------------------------------------- */
%start toplevel
%type <Ast.prog> toplevel
%type <Ast.exp> exp
%type <Ast.block> block
%type <Ast.const> const
%%


toplevel:
  | p=list(stmt) EOF  { p }

ident:
  | id=IDENT  { loc $startpos $endpos id }

decl:
  | TINT id=ident EQ init=exp { loc $startpos $endpos @@ {id; init} }

const:
  | i=INT { loc $startpos $endpos @@ CInt i }

%inline bop:
  | PLUS { Add }
  | DASH { Sub }
  | STAR { Mul }

exp:
  | id=ident            { loc $startpos $endpos @@ Id (id) }
  | c=const             { loc $startpos $endpos @@ Const (c) }
  | e1=exp b=bop e2=exp { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | LPAREN e=exp RPAREN { e }


stmt: 
  | d=decl SEMI             { loc $startpos $endpos @@ Decl(d) }
  | id=ident EQ e=exp SEMI  { loc $startpos $endpos @@ Assn(id, e) }
  | ifs=if_stmt             { ifs }
  | RETURN e=exp SEMI       { loc $startpos $endpos @@ Ret(e) }
  | WHILE LPAREN e=exp RPAREN b=block
                            { loc $startpos $endpos @@ While(e, b) }

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF LPAREN e=exp RPAREN b1=block b2=else_stmt
       { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }
