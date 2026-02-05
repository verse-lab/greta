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

bool11:
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  ;

exp110:
  | e1=exp110 PLUS e2=exp25  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | exp24  {  $1  }
  ;

exp16:
  | exp17  { $1 }
  ;

exp17:
  | e1=exp18 EQEQ e2=exp16  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | exp18  { $1 }
  ;

exp18:
  | exp19  { $1 }
  | e1=exp18 STAR e2=exp19  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

exp19:
  | e1=exp19 DASH e2=exp110  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | exp110  { $1 }
  ;

exp23:
  | exp24  { $1 }
  ;

exp24:
  | exp25  { $1 }
  ;

exp25:
  | LPAREN e=exp16 RPAREN  {  e  }
  | TILDE e=exp23  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | DASH e=exp23  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | BANG e=exp23  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr2:
  | IF e=bool11 THEN s1=expr1 ELSE s2=expr1  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | e=exp17 LPAREN COMMA es=exp17 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | p=lh13 EQ e=exp17 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | d=vdec14 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | RETURN e=exp17 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=exp17 RPAREN b=expr1  {  loc $startpos $endpos @@ While(e, b)  }
  ;

iden12:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh13:
  | e=exp16 LBRACKET i=exp16 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog1:
  | p=expr2 EOF  {  p  }
  ;

vdec14:
  | VAR id=iden12 EQ init=exp16  {  loc $startpos $endpos @@ {id; init}  }
  ;
