// 4 conflicts - all 4 po's
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

%left PLUS DASH


/* ---------------------------------------------------------------------- */

%start prog
%type <Ast.exp> exp1
%type <Ast.cond> bool_expr
%type <Ast.stmt> stmt

%type <Ast.prog> prog
%%

prog:
  | p=stmt EOF  { p }

lhs:  
  | e=exp1 LBRACKET i=exp1 RBRACKET { loc $startpos $endpos @@ Index (e, i) }

exp1:
  | exp2 { $1 }
  | e1=exp1 PLUS e2=exp1 { loc $startpos $endpos @@ Bop (Add, e1, e2) }
  | e1=exp1 DASH e2=exp1 { loc $startpos $endpos @@ Bop (Sub, e1, e2) }
  | e1=exp1 STAR e2=exp2 { loc $startpos $endpos @@ Bop (Mul, e1, e2) }
  | e1=exp1 EQEQ e2=exp2 { loc $startpos $endpos @@ Bop (Eq, e1, e2) }
  
exp2:
  | INT                 { loc $startpos $endpos @@ CInt }
  | DASH e=exp2         { loc $startpos $endpos @@ Uop (Neg, e) }
  | BANG e=exp2         { loc $startpos $endpos @@ Uop (Lognot, e) }
  | TILDE e=exp2        { loc $startpos $endpos @@ Uop (Bitnot, e) }
  | LPAREN e=exp1 RPAREN { e } 
  

ident:
  | id=IDENT { loc $startpos $endpos id }

vdecl:
  | VAR id=ident EQ init=exp1 { loc $startpos $endpos @@ {id; init} }

stmt: 
  | d=vdecl SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp1 SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=exp1 LPAREN COMMA es=exp1 RPAREN SEMI { loc $startpos $endpos @@ SCall (e, es) }
  | IF e=bool_expr THEN s1=stmt ELSE s2=stmt  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp1 SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp1 RPAREN b=stmt  { loc $startpos $endpos @@ While(e, b) } 
  | NULL  { loc $startpos $endpos @@ CNull }


bool_expr:
  | TRUE { loc $startpos $endpos @@ CBool true }
  | FALSE { loc $startpos $endpos @@ CBool false }
