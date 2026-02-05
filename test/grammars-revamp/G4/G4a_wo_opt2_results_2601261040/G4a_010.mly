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
%type <Ast.exp> exp18
%type <Ast.prog> prog1
%%

bool12:
  | TRUE  {  loc $startpos $endpos @@ CBool true  }
  | FALSE  {  loc $startpos $endpos @@ CBool false  }
  ;

exp17:
  | exp18  { $1 }
  ;

exp18:
  | exp211  {  $1  }
  | e1=exp17 PLUS e2=exp211  {  loc $startpos $endpos @@ Bop (Add, e1, e2)  }
  | e1=exp17 DASH e2=exp211  {  loc $startpos $endpos @@ Bop (Sub, e1, e2)  }
  ;

exp210:
  | exp211  { $1 }
  ;

exp211:
  | exp213  { $1 }
  | e1=exp210 STAR e2=exp213  {  loc $startpos $endpos @@ Bop (Mul, e1, e2)  }
  ;

exp213:
  | exp36  {  $1   }
  ;

exp35:
  | exp36  { $1 }
  ;

exp36:
  | e1=exp39 EQEQ e2=exp35  {  loc $startpos $endpos @@ Bop (Eq, e1, e2)  }
  | exp39  { $1 }
  ;

exp39:
  | DASH e=exp39  {  loc $startpos $endpos @@ Uop (Neg, e)  }
  | exp44  {  $1   }
  ;

exp43:
  | exp44  { $1 }
  ;

exp44:
  | LPAREN e=exp17 RPAREN  {  e  }
  | TILDE e=exp43  {  loc $startpos $endpos @@ Uop (Bitnot, e)  }
  | INT  {  loc $startpos $endpos @@ CInt  }
  | BANG e=exp43  {  loc $startpos $endpos @@ Uop (Lognot, e)  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr2:
  | IF e=bool12 THEN s1=expr1 ELSE s2=expr1  {  loc $startpos $endpos @@ If(e,[s1],[s2])  }
  | NULL  {  loc $startpos $endpos @@ CNull  }
  | e=exp18 LPAREN COMMA es=exp18 RPAREN SEMI  {  loc $startpos $endpos @@ SCall (e, es)  }
  | d=vdec16 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | RETURN SEMI  {  loc $startpos $endpos @@ Ret(None)  }
  | p=lh15 EQ e=exp18 SEMI  {  loc $startpos $endpos @@ Assn(p,e)  }
  | RETURN e=exp18 SEMI  {  loc $startpos $endpos @@ Ret(Some e)  }
  | WHILE LPAREN e=exp18 RPAREN b=expr1  {  loc $startpos $endpos @@ While(e, b)  }
  ;

iden14:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

lh15:
  | e=exp17 LBRACKET i=exp17 RBRACKET  {  loc $startpos $endpos @@ Index (e, i)  }
  ;

prog1:
  | p=expr2 EOF  {  p  }
  ;

vdec16:
  | VAR id=iden14 EQ init=exp17  {  loc $startpos $endpos @@ {id; init}  }
  ;
