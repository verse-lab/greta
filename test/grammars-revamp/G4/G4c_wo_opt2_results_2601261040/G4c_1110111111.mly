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
%token FALSE%start prog1
%type <Ast.exp> ex9
%type <Ast.prog> prog1
%%

bool10:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

ex11:
  | exp24  {  $1  }
  ;

ex5:
  | ex6  { $1 }
  ;

ex6:
  | e1=ex7 EQEQ e2=ex5  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | ex7  { $1 }
  ;

ex7:
  | ex8  { $1 }
  | e1=ex8 STAR e2=ex7  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

ex8:
  | ex9  { $1 }
  | e1=ex8 DASH e2=ex9  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

ex9:
  | ex11  { $1 }
  | e1=ex11 PLUS e2=ex9  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  ;

exp23:
  | exp24  { $1 }
  ;

exp24:
  | LPAREN e=ex5 RPAREN  {  e  }
  | BANG e=exp23  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp23  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | DASH e=exp23  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr2:
  | IF e=bool10 THEN s1=expr1 ELSE s2=expr1  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | e=ex6 LPAREN COMMA es=ex6 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | p=lh13 EQ e=ex6 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | d=vdec14 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | RETURN e=ex6 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=ex6 RPAREN b=expr1  {  loc $startpos $endpos @@ While(e, b)  }
  ;

iden12:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh13:
  | e=ex5 LBRACKET i=ex5 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog1:
  | p=expr2 EOF  {  p  }
  ;

vdec14:
  | VAR id=iden12 EQ init=ex5  {  loc $startpos $endpos @@ {id; init}  }
  ;
