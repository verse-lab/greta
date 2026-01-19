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

%start prog
%type <Ast.exp> exp

%type <Ast.prog> prog
%%

prog:
  | p=expr EOF  { p }

lhs:  
  | e=exp LBRACKET i=exp RBRACKET { loc $startpos $endpos @@ Index (e, i) }

exp:
  | e1=exp PLUS e2=exp { loc $startpos $endpos @@ Bop (Add, e1, e2) }
  | e1=exp DASH e2=exp { loc $startpos $endpos @@ Bop (Sub, e1, e2) }
  | e1=exp STAR e2=exp { loc $startpos $endpos @@ Bop (Mul, e1, e2) }
  | e1=exp EQEQ e2=exp { loc $startpos $endpos @@ Bop (Eq, e1, e2) }
  | exp2 { $1 }

exp2:
  | INT                 { loc $startpos $endpos @@ CInt }
  | DASH e=exp2         { loc $startpos $endpos @@ Uop (Neg, e) }
  | BANG e=exp2         { loc $startpos $endpos @@ Uop (Lognot, e) }
  | TILDE e=exp2        { loc $startpos $endpos @@ Uop (Bitnot, e) }
  | LPAREN e=exp RPAREN { e } 
  

ident:
  | id=IDENT { loc $startpos $endpos id }

vdecl:
  | VAR id=ident EQ init=exp { loc $startpos $endpos @@ {id; init} }

expr: 
  | d=vdecl SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=exp LPAREN COMMA es=exp RPAREN SEMI { loc $startpos $endpos @@ SCall (e, es) }
  | IF e=bool_expr THEN s1=expr ELSE s2=expr  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp RPAREN b=expr  { loc $startpos $endpos @@ While(e, b) } 
  | NULL  { loc $startpos $endpos @@ CNull }

bool_expr:
  | TRUE { loc $startpos $endpos @@ CBool true }
  | FALSE { loc $startpos $endpos @@ CBool false }

