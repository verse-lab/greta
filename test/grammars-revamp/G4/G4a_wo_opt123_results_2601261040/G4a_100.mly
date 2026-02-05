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
%token FALSE%start prog32
%type <Ast.exp> exp120
%type <Ast.prog> prog32
%%

bool26:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

bool27:
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  ;

bool28:
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  ;

exp118:
  | exp222  {  $1  }
  | e1=exp118 DASH e2=exp222  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | e1=exp118 PLUS e2=exp222  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  ;

exp119:
  | e1=exp118 DASH e2=exp222  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | exp222  {  $1  }
  | e1=exp118 PLUS e2=exp222  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  ;

exp120:
  | e1=exp118 DASH e2=exp222  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  | e1=exp118 PLUS e2=exp222  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | exp222  {  $1  }
  ;

exp222:
  | exp313  {  $1   }
  | e1=exp242 STAR e2=exp223  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

exp223:
  | e1=exp242 STAR e2=exp223  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  | exp313  {  $1   }
  ;

exp224:
  | exp313  {  $1   }
  | e1=exp242 STAR e2=exp223  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

exp225:
  | exp313  {  $1   }
  | e1=exp242 STAR e2=exp223  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

exp242:
  | exp313  {  $1   }
  ;

exp312:
  | e1=exp315 EQEQ e2=exp321  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | exp49  {  $1   }
  | DASH e=exp321  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

exp313:
  | e1=exp315 EQEQ e2=exp321  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | DASH e=exp321  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | exp49  {  $1   }
  ;

exp314:
  | exp49  {  $1   }
  | e1=exp315 EQEQ e2=exp321  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | DASH e=exp321  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

exp315:
  | e1=exp315 EQEQ e2=exp321  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | DASH e=exp321  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | exp49  {  $1   }
  ;

exp316:
  | e1=exp315 EQEQ e2=exp321  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | exp49  {  $1   }
  | DASH e=exp321  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

exp317:
  | DASH e=exp321  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | e1=exp315 EQEQ e2=exp321  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | exp49  {  $1   }
  ;

exp321:
  | exp49  {  $1   }
  | DASH e=exp321  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  ;

exp341:
  | exp49  {  $1   }
  ;

exp410:
  | LPAREN e=exp118 RPAREN  {  e  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  ;

exp411:
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | LPAREN e=exp118 RPAREN  {  e  }
  ;

exp43:
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | LPAREN e=exp118 RPAREN  {  e  }
  ;

exp44:
  | LPAREN e=exp118 RPAREN  {  e  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

exp45:
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | LPAREN e=exp118 RPAREN  {  e  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  ;

exp46:
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | LPAREN e=exp118 RPAREN  {  e  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

exp47:
  | LPAREN e=exp118 RPAREN  {  e  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  ;

exp48:
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | LPAREN e=exp118 RPAREN  {  e  }
  ;

exp49:
  | LPAREN e=exp118 RPAREN  {  e  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | TILDE e=exp45  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | BANG e=exp45  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

expr1:
  | IF e=bool28 THEN s1=expr2 ELSE s2=expr2  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | e=exp120 LPAREN COMMA es=exp120 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | RETURN e=exp120 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=exp120 RPAREN b=expr2  {  loc $startpos $endpos @@ While(e, b)  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | d=vdec31 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | p=lh35 EQ e=exp120 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  ;

expr2:
  | IF e=bool28 THEN s1=expr2 ELSE s2=expr2  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | e=exp120 LPAREN COMMA es=exp120 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | d=vdec31 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | p=lh35 EQ e=exp120 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | RETURN e=exp120 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=exp120 RPAREN b=expr2  {  loc $startpos $endpos @@ While(e, b)  }
  ;

iden36:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden37:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden38:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden39:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden40:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh33:
  | e=exp118 LBRACKET i=exp118 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh34:
  | e=exp118 LBRACKET i=exp118 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

lh35:
  | e=exp118 LBRACKET i=exp118 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog32:
  | p=expr1 EOF  {  p  }
  ;

vdec29:
  | VAR id=iden36 EQ init=exp118  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec30:
  | VAR id=iden36 EQ init=exp118  {  loc $startpos $endpos @@ {id; init}  }
  ;

vdec31:
  | VAR id=iden36 EQ init=exp118  {  loc $startpos $endpos @@ {id; init}  }
  ;
