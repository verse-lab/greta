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
%type <Ast.exp> exp14
%type <Ast.prog> prog9
%%

bool7:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

exp14:
  | exp26  {  $1  }
  | e1=exp14 PLUS e2=exp26  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | e1=exp14 DASH e2=exp26  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

exp213:
  | exp33  {  $1   }
  ;

exp26:
  | e1=exp213 STAR e2=exp26  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp213  { $1 }
  ;

exp312:
  | exp42  {  $1   }
  ;

exp33:
  | exp35  { $1 }
  | e1=exp33 EQEQ e2=exp35  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  ;

exp35:
  | exp312  { $1 }
  | DASH e=exp35  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

exp42:
  | INT  {  loc $startpos $endpos @@ CInt  }
  | LPAREN e=exp14 RPAREN  {  e  }
  | TILDE e=exp42  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | BANG e=exp42  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

expr1:
  | IF e=bool7 THEN s1=expr1 ELSE s2=expr1  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | e=exp14 LPAREN COMMA es=exp14 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | RETURN e=exp14 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=exp14 RPAREN b=expr1  {  loc $startpos $endpos @@ While(e, b)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | d=vdec8 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | p=lh10 EQ e=exp14 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  ;

iden11:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh10:
  | e=exp14 LBRACKET i=exp14 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog9:
  | p=expr1 EOF  {  p  }
  ;

vdec8:
  | VAR id=iden11 EQ init=exp14  {  loc $startpos $endpos @@ {id; init}  }
  ;
