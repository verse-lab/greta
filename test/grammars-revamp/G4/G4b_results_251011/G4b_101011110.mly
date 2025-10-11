// G3a - 3 conflicts - 3 po's 
%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a loc =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}


%token EOF
%token <int64>  INT
%token NULL
%token <string> IDENT

%token IF       /* if */
%token THEN     /* then */
%token ELSE     /* else */
%token WHILE    /* while */
%token RETURN   /* return */
%token VAR      /* var */
%token SEMI     /* ; */
%token COMMA    /* , */
%token PLUS     /* + */
%token DASH     /* - */
%token STAR     /* * */
%token EQEQ     /* == */
%token EQ       /* = */
%token LPAREN   /* ( */
%token RPAREN   /* ) */
%token LBRACKET /* [ */
%token RBRACKET /* ] */
%token TILDE    /* ~ */
%token BANG     /* ! */
%token TRUE 
%token FALSE


/* ---------------------------------------------------------------------- */

%start prog1
%type <Ast.exp> exp18
%type <Ast.prog> prog1
%%

bool7:
  | FALSE  { loc $startpos $endpos @@ CBool false }
  | TRUE  { loc $startpos $endpos @@ CBool true }
  ;

exp13:
  | exp14  { $1 }
  | e1=exp14 EQEQ e2=exp13  { loc $startpos $endpos @@ Bop (Eq, e1, e2) }
  ;

exp14:
  | exp15  { $1 }
  | e1=exp14 PLUS e2=exp22  { loc $startpos $endpos @@ Bop (Add, e1, e2) }
  ;

exp15:
  | exp16  { $1 }
  | e1=exp16 STAR e2=exp15  { loc $startpos $endpos @@ Bop (Mul, e1, e2) }
  ;

exp16:
  | e1=exp16 DASH e2=exp18  { loc $startpos $endpos @@ Bop (Sub, e1, e2) }
  | exp18  { $1 }
  ;

exp18:
  | exp22  { $1 }
  ;

exp22:
  | INT  { loc $startpos $endpos @@ CInt }
  | BANG e=exp22  { loc $startpos $endpos @@ Uop (Lognot, e) }
  | TILDE e=exp22  { loc $startpos $endpos @@ Uop (Bitnot, e) }
  | DASH e=exp22  { loc $startpos $endpos @@ Uop (Neg, e) }
  | LPAREN e=exp13 RPAREN  { e }
  ;

expr1:
  | IF e=bool7 THEN s1=expr1 ELSE s2=expr1  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | WHILE LPAREN e=exp13 RPAREN b=expr1  { loc $startpos $endpos @@ While(e, b) }
  | e=exp13 LPAREN COMMA es=exp13 RPAREN SEMI  { loc $startpos $endpos @@ SCall (e, es) }
  | d=vdec11 SEMI  { loc $startpos $endpos @@ Decl(d) }
  | RETURN SEMI  { loc $startpos $endpos @@ Ret(None) }
  | NULL  { loc $startpos $endpos @@ CNull }
  | RETURN e=exp13 SEMI  { loc $startpos $endpos @@ Ret(Some e) }
  | p=lh10 EQ e=exp13 SEMI  { loc $startpos $endpos @@ Assn(p,e) }
  ;

iden9:
  | id=IDENT  { loc $startpos $endpos id }
  ;

lh10:
  | e=exp13 LBRACKET i=exp13 RBRACKET  { loc $startpos $endpos @@ Index (e, i) }
  ;

prog1:
  | p=expr1 EOF  { p }
  ;

vdec11:
  | VAR id=iden9 EQ init=exp13  { loc $startpos $endpos @@ {id; init} }
  ;
