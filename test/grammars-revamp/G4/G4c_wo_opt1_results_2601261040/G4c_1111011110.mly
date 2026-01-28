%{

open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a loc =
  { elt ; loc=Range.mk_lex_range startpos endpos }


%}
%token EOF
%token <int64> INT
%token NULL
%token <string> IDENT
%token IF
%token THEN
%token ELSE
%token WHILE
%token RETURN
%token VAR
%token SEMI
%token COMMA
%token PLUS
%token DASH
%token STAR
%token EQEQ
%token EQ
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token TILDE
%token BANG
%token TRUE
%token FALSE%start prog9
%type <Ast.exp> ex6
%type <Ast.prog> prog9
%%

bool7:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

ex12:
  | exp22  {  $1  }
  ;

ex3:
  | e1=ex3 STAR e2=ex4  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | ex4  { $1 }
  ;

ex4:
  | e1=ex5 EQEQ e2=ex4  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | ex5  { $1 }
  ;

ex5:
  | ex6  { $1 }
  | e1=ex6 DASH e2=ex5  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

ex6:
  | e1=ex12 PLUS e2=ex6  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | ex12  { $1 }
  ;

exp22:
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp22  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp22  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | DASH e=exp22  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | LPAREN e=ex3 RPAREN  {  e  }
  ;

expr1:
  | IF e=bool7 THEN s1=expr1 ELSE s2=expr1  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | WHILE LPAREN e=ex3 RPAREN b=expr1  {  loc $startpos $endpos @@ While(e, b)  }
  | e=ex3 LPAREN COMMA es=ex3 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | d=vdec8 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | RETURN e=ex3 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | p=lh10 EQ e=ex3 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  ;

iden11:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh10:
  | e=ex3 LBRACKET i=ex3 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog9:
  | p=expr1 EOF  {  p  }
  ;

vdec8:
  | VAR id=iden11 EQ init=ex3  {  loc $startpos $endpos @@ {id; init}  }
  ;
