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
%type <Ast.exp> exp147
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

exp111:
  | e1=exp116 STAR e2=exp147  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp24  {  $1  }
  | e1=exp115 PLUS e2=exp28  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | e1=exp114 DASH e2=exp115  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | e1=exp111 EQEQ e2=exp114  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  ;

exp112:
  | e1=exp116 STAR e2=exp147  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | e1=exp115 PLUS e2=exp28  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | exp24  {  $1  }
  | e1=exp114 DASH e2=exp115  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | e1=exp111 EQEQ e2=exp114  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  ;

exp113:
  | e1=exp116 STAR e2=exp147  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp24  {  $1  }
  | e1=exp114 DASH e2=exp115  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | e1=exp115 PLUS e2=exp28  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | e1=exp111 EQEQ e2=exp114  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  ;

exp114:
  | e1=exp115 PLUS e2=exp28  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | exp24  {  $1  }
  | e1=exp114 DASH e2=exp115  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | e1=exp116 STAR e2=exp147  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

exp115:
  | e1=exp115 PLUS e2=exp28  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | e1=exp116 STAR e2=exp147  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp24  {  $1  }
  ;

exp116:
  | exp24  {  $1  }
  | e1=exp116 STAR e2=exp147  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

exp147:
  | exp24  {  $1  }
  ;

exp210:
  | LPAREN e=exp111 RPAREN  {  e  }
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
  | LPAREN e=exp111 RPAREN  {  e  }
  ;

exp24:
  | LPAREN e=exp111 RPAREN  {  e  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  ;

exp25:
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | LPAREN e=exp111 RPAREN  {  e  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  ;

exp26:
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | LPAREN e=exp111 RPAREN  {  e  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

exp27:
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | LPAREN e=exp111 RPAREN  {  e  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

exp28:
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | LPAREN e=exp111 RPAREN  {  e  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  ;

exp29:
  | BANG e=exp26  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp26  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | LPAREN e=exp111 RPAREN  {  e  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | DASH e=exp26  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

expr1:
  | IF e=bool23 THEN s1=expr2 ELSE s2=expr2  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | WHILE LPAREN e=exp113 RPAREN b=expr2  {  loc $startpos $endpos @@ While(e, b)  }
  | e=exp113 LPAREN COMMA es=exp113 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | d=vdec30 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | p=lh38 EQ e=exp113 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | RETURN e=exp113 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  ;

expr2:
  | p=lh38 EQ e=exp113 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | d=vdec30 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | IF e=bool23 THEN s1=expr2 ELSE s2=expr2  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | e=exp113 LPAREN COMMA es=exp113 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | RETURN e=exp113 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=exp113 RPAREN b=expr2  {  loc $startpos $endpos @@ While(e, b)  }
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
  | e=exp111 LBRACKET i=exp111 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh33:
  | e=exp111 LBRACKET i=exp111 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh34:
  | e=exp111 LBRACKET i=exp111 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh35:
  | e=exp111 LBRACKET i=exp111 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh36:
  | e=exp111 LBRACKET i=exp111 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh37:
  | e=exp111 LBRACKET i=exp111 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh38:
  | e=exp111 LBRACKET i=exp111 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog31:
  | p=expr1 EOF  {  p  }
  ;

vdec24:
  | VAR id=iden40 EQ init=exp111  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec25:
  | VAR id=iden40 EQ init=exp111  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec26:
  | VAR id=iden40 EQ init=exp111  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec27:
  | VAR id=iden40 EQ init=exp111  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec28:
  | VAR id=iden40 EQ init=exp111  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec29:
  | VAR id=iden40 EQ init=exp111  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec30:
  | VAR id=iden40 EQ init=exp111  {  loc $startpos $endpos @@ {id; init}  }
  ;
