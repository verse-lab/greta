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
%type <Ast.exp> exp19
%type <Ast.prog> prog1
%%

bool10:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

exp111:
  | exp24  {  $1  }
  ;

exp15:
  | exp16  { $1 }
  ;

exp16:
  | e1=exp15 PLUS e2=exp24  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | exp17  { $1 }
  ;

exp17:
  | exp18  { $1 }
  | e1=exp17 DASH e2=exp18  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

exp18:
  | exp19  { $1 }
  | e1=exp19 EQEQ e2=exp18  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  ;

exp19:
  | e1=exp111 STAR e2=exp19  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp111  { $1 }
  ;

exp23:
  | exp24  { $1 }
  ;

exp24:
  | LPAREN e=exp15 RPAREN  {  e  }
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
  | e=exp16 LPAREN COMMA es=exp16 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | p=lh13 EQ e=exp16 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | d=vdec14 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | RETURN e=exp16 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=exp16 RPAREN b=expr1  {  loc $startpos $endpos @@ While(e, b)  }
  ;

iden12:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh13:
  | e=exp15 LBRACKET i=exp15 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog1:
  | p=expr2 EOF  {  p  }
  ;

vdec14:
  | VAR id=iden12 EQ init=exp15  {  loc $startpos $endpos @@ {id; init}  }
  ;
