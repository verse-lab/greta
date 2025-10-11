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

%left DASH 
%start topl1
%type <Ast.prog> topl1
%type <Ast.exp> ex6
%type <Ast.const> cons9
%%

cons9:
  | i=INT  { loc $startpos $endpos @@ CInt i }
  ;

decl10:
  | TINT id=iden11 EQ init=ex1  { loc $startpos $endpos @@ {id; init} }
  ;

ex1:
  | ex2  { $1 }
  | e1=ex1 DASH e2=ex1  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  ;

ex2:
  | ex4  { $1 }
  | e1=ex4 STAR e2=ex2  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  ;

ex4:
  | e1=ex6 PLUS e2=ex4  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | ex6  { $1 }
  ;

ex6:
  | LPAREN e=ex1 RPAREN  { e }
  | id=iden11  { loc $startpos $endpos @@ Id (id) }
  | c=cons9  { loc $startpos $endpos @@ Const (c) }
  ;

iden11:
  | id=IDENT  { loc $startpos $endpos id }
  ;

stmt3:
  | stmt5  { $1 }
  | IF LPAREN e=ex1 RPAREN s1=stmt3  { loc $startpos $endpos @@ If(e, [s1], []) }
  ;

stmt5:
  | IF LPAREN e=ex1 RPAREN s1=stmt5 ELSE s2=stmt5  { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  | id=iden11 EQ e=ex1 SEMI  { loc $startpos $endpos @@ Assn(id, e) }
  | d=decl10 SEMI  { loc $startpos $endpos @@ Decl(d) }
  | stmt7  { $1 }
  ;

stmt7:
  | RETURN e=ex1 SEMI  { loc $startpos $endpos @@ Ret(e) }
  | WHILE LPAREN e=ex1 RPAREN s=stmt7  { loc $startpos $endpos @@ While(e, [s]) }
  | LBRACE ss=stmt8 RBRACE  { loc $startpos $endpos @@ Block(ss) }
  ;

stmt8:
  |   { [] }
  | s=stmt3 ss=stmt8  { s::ss }
  ;

topl1:
  | p=stmt8 EOF  { p }
  ;
