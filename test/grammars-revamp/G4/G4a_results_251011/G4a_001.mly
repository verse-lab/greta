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
%type <Ast.exp> exp14
%type <Ast.prog> prog1
%%

bool7:
  | FALSE  { loc $startpos $endpos @@ CBool false }
  | TRUE  { loc $startpos $endpos @@ CBool true }
  ;

exp14:
  | exp26  { $1 }
  | e1=exp14 PLUS e2=exp26  { loc $startpos $endpos @@ Bop (Add, e1, e2) }
  | e1=exp14 DASH e2=exp26  { loc $startpos $endpos @@ Bop (Sub, e1, e2) }
  ;

exp26:
  | e1=exp26 STAR e2=exp29  { loc $startpos $endpos @@ Bop (Mul, e1, e2) }
  | exp29  { $1 }
  ;

exp29:
  | exp33  { $1  }
  ;

exp33:
  | exp35  { $1 }
  | DASH e=exp33  { loc $startpos $endpos @@ Uop (Neg, e) }
  ;

exp35:
  | exp38  { $1 }
  | e1=exp35 EQEQ e2=exp38  { loc $startpos $endpos @@ Bop (Eq, e1, e2) }
  ;

exp38:
  | exp42  { $1  }
  ;

exp42:
  | INT  { loc $startpos $endpos @@ CInt }
  | LPAREN e=exp14 RPAREN  { e }
  | TILDE e=exp42  { loc $startpos $endpos @@ Uop (Bitnot, e) }
  | BANG e=exp42  { loc $startpos $endpos @@ Uop (Lognot, e) }
  ;

expr1:
  | IF e=bool7 THEN s1=expr1 ELSE s2=expr1  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | e=exp14 LPAREN COMMA es=exp14 RPAREN SEMI  { loc $startpos $endpos @@ SCall (e, es) }
  | RETURN e=exp14 SEMI  { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp14 RPAREN b=expr1  { loc $startpos $endpos @@ While(e, b) }
  | NULL  { loc $startpos $endpos @@ CNull }
  | d=vdec12 SEMI  { loc $startpos $endpos @@ Decl(d) }
  | p=lh11 EQ e=exp14 SEMI  { loc $startpos $endpos @@ Assn(p,e) }
  | RETURN SEMI  { loc $startpos $endpos @@ Ret(None) }
  ;

iden10:
  | id=IDENT  { loc $startpos $endpos id }
  ;

lh11:
  | e=exp14 LBRACKET i=exp14 RBRACKET  { loc $startpos $endpos @@ Index (e, i) }
  ;

prog1:
  | p=expr1 EOF  { p }
  ;

vdec12:
  | VAR id=iden10 EQ init=exp14  { loc $startpos $endpos @@ {id; init} }
  ;
