/* *** G2b *** */
// 3 conflicts - 3 po's 1 assoc
// if1 vs. if2
// - vs. +
// * assoc

%{
open Ast;;

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a loc =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

%token EOF
%token <int64>  INT
%token <string> IDENT
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
%start topl1
%type <Ast.prog> topl1
%type <Ast.exp> ex5
%type <Ast.const> cons7
%%

cons7:
  | i=INT  { loc $startpos $endpos @@ CInt i }
  ;

decl9:
  | TINT id=iden8 EQ init=ex5  { loc $startpos $endpos @@ {id; init} }
  ;

ex5:
  | exp23  { $1 }
  | e1=ex5 PLUS e2=exp23  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | e1=ex5 DASH e2=exp23  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  ;

exp23:
  | e1=exp24 STAR e2=exp23  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  | exp24  { $1 }
  ;

exp24:
  | id=iden8  { loc $startpos $endpos @@ Id (id) }
  | c=cons7  { loc $startpos $endpos @@ Const (c) }
  | LPAREN e=ex5 RPAREN  { e }
  ;

iden8:
  | id=IDENT  { loc $startpos $endpos id }
  ;

stmt1:
  | IF LPAREN e=ex5 RPAREN s1=stmt1 ELSE s2=stmt1  { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  | stmt2  { $1 }
  ;

stmt2:
  | id=iden8 EQ e=ex5 SEMI  { loc $startpos $endpos @@ Assn(id, e) }
  | IF LPAREN e=ex5 RPAREN s1=stmt2  { loc $startpos $endpos @@ If(e, [s1], []) }
  | RETURN e=ex5 SEMI  { loc $startpos $endpos @@ Ret(e) }
  | d=decl9 SEMI  { loc $startpos $endpos @@ Decl(d) }
  | LBRACE ss=stmt6 RBRACE  { loc $startpos $endpos @@ Block(ss) }
  | WHILE LPAREN e=ex5 RPAREN s=stmt1  { loc $startpos $endpos @@ While(e, [s]) }
  ;

stmt6:
  |   { [] }
  | s=stmt1 ss=stmt6  { s::ss }
  ;

topl1:
  | p=stmt6 EOF  { p }
  ;
