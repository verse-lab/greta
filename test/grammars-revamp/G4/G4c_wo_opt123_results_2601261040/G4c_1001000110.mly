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
%token FALSE%start prog31
%type <Ast.exp> ex47
%type <Ast.prog> prog31
%%

bool17:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

bool18:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

bool19:
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  ;

bool20:
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  ;

bool21:
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  ;

bool22:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

bool23:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

ex11:
  | e1=ex16 EQEQ e2=ex47  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | e1=ex11 STAR e2=ex14  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | e1=ex15 PLUS e2=ex14  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | exp24  {  $1  }
  | e1=ex16 DASH e2=ex15  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

ex12:
  | e1=ex11 STAR e2=ex14  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp24  {  $1  }
  | e1=ex15 PLUS e2=ex14  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | e1=ex16 EQEQ e2=ex47  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | e1=ex16 DASH e2=ex15  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

ex13:
  | e1=ex15 PLUS e2=ex14  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | e1=ex16 EQEQ e2=ex47  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | e1=ex11 STAR e2=ex14  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp24  {  $1  }
  | e1=ex16 DASH e2=ex15  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

ex14:
  | exp24  {  $1  }
  | e1=ex16 EQEQ e2=ex47  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | e1=ex16 DASH e2=ex15  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | e1=ex15 PLUS e2=ex14  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  ;

ex15:
  | exp24  {  $1  }
  | e1=ex16 EQEQ e2=ex47  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | e1=ex16 DASH e2=ex15  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

ex16:
  | exp24  {  $1  }
  | e1=ex16 EQEQ e2=ex47  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  ;

ex47:
  | exp24  {  $1  }
  ;

exp210:
  | LPAREN e=ex11 RPAREN  {  e  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  ;

exp23:
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | LPAREN e=ex11 RPAREN  {  e  }
  ;

exp24:
  | LPAREN e=ex11 RPAREN  {  e  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  ;

exp25:
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | LPAREN e=ex11 RPAREN  {  e  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  ;

exp26:
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | LPAREN e=ex11 RPAREN  {  e  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

exp27:
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | LPAREN e=ex11 RPAREN  {  e  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

exp28:
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | LPAREN e=ex11 RPAREN  {  e  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  ;

exp29:
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | LPAREN e=ex11 RPAREN  {  e  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

expr1:
  | IF e=bool23 THEN s1=expr2 ELSE s2=expr2  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | WHILE LPAREN e=ex13 RPAREN b=expr2  {  loc $startpos $endpos @@ While(e, b)  }
  | e=ex13 LPAREN COMMA es=ex13 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | d=vdec30 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | p=lh38 EQ e=ex13 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | RETURN e=ex13 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  ;

expr2:
  | p=lh38 EQ e=ex13 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | d=vdec30 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | IF e=bool23 THEN s1=expr2 ELSE s2=expr2  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | e=ex13 LPAREN COMMA es=ex13 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | RETURN e=ex13 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=ex13 RPAREN b=expr2  {  loc $startpos $endpos @@ While(e, b)  }
  ;

iden39:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden40:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden41:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden42:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden43:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden44:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden45:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden46:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh32:
  | e=ex11 LBRACKET i=ex11 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh33:
  | e=ex11 LBRACKET i=ex11 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh34:
  | e=ex11 LBRACKET i=ex11 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh35:
  | e=ex11 LBRACKET i=ex11 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh36:
  | e=ex11 LBRACKET i=ex11 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh37:
  | e=ex11 LBRACKET i=ex11 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh38:
  | e=ex11 LBRACKET i=ex11 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog31:
  | p=expr1 EOF  {  p  }
  ;

vdec24:
  | VAR id=iden40 EQ init=ex11  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec25:
  | VAR id=iden40 EQ init=ex11  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec26:
  | VAR id=iden40 EQ init=ex11  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec27:
  | VAR id=iden40 EQ init=ex11  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec28:
  | VAR id=iden40 EQ init=ex11  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec29:
  | VAR id=iden40 EQ init=ex11  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec30:
  | VAR id=iden40 EQ init=ex11  {  loc $startpos $endpos @@ {id; init}  }
  ;
