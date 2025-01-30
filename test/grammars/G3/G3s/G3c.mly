// 6 conflicts - all 6 po's
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


%start prog
%type <Ast.exp> exp1
%type <Ast.cond> bool_expr
%type <Ast.stmt> e1

%type <Ast.prog> prog
%%

prog:
  | e1 EOF  { $1 }

lhs:  
  | exp1 LBRACKET exp1 RBRACKET { loc $startpos $endpos @@ Index ($1, $3) }

e1: 
  | d=vdecl SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp1 SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=exp1 LPAREN COMMA es=exp1 RPAREN SEMI { loc $startpos $endpos @@ SCall (e, es) }
  | IF e=bool_expr THEN s1=e1 ELSE s2=e1  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp1 SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp1 RPAREN b=e1  { loc $startpos $endpos @@ While(e, b) } 
  | NULL  { loc $startpos $endpos @@ CNull }

exp1:
  | exp2 { $1 }
  | exp1 PLUS exp1 { loc $startpos $endpos @@ Bop (Add, $1, $3) }
  | exp1 DASH exp1 { loc $startpos $endpos @@ Bop (Sub, $1, $3) }
  | exp1 STAR exp2 { loc $startpos $endpos @@ Bop (Mul, $1, $3) }
  | exp1 EQEQ exp2 { loc $startpos $endpos @@ Bop (Eq, $1, $3) }
  | DASH exp1         { loc $startpos $endpos @@ Uop (Neg, $2) }
  
exp2:
  | BANG exp2         { loc $startpos $endpos @@ Uop (Lognot, $2) }
  | TILDE exp2        { loc $startpos $endpos @@ Uop (Bitnot, $2) }
  | INT                 { loc $startpos $endpos @@ CInt }
  | LPAREN exp1 RPAREN { $2 } 
  

ident:
  | id=IDENT { loc $startpos $endpos id }

vdecl:
  | VAR id=ident EQ init=exp1 { loc $startpos $endpos @@ {id; init} }

bool_expr:
  | TRUE { loc $startpos $endpos @@ CBool true }
  | FALSE { loc $startpos $endpos @@ CBool false }
