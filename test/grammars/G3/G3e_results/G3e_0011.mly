// 10 conflicts - 9 po's 1 assoc
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
%type <Ast.exp> x1
%type <Ast.cond> bool_expr
%type <Ast.stmt> e1
%type <Ast.prog> prog
%%

prog:
  | p=e1 EOF  { p }

ident:
  | id=IDENT { loc $startpos $endpos id }

bool_expr:
  | TRUE { loc $startpos $endpos @@ CBool true }
  | FALSE { loc $startpos $endpos @@ CBool false }

x4:  
  | e=x2 LBRACKET i=x2 RBRACKET { loc $startpos $endpos @@ Index (e, i) }

x3:
  | BANG e=x3         { loc $startpos $endpos @@ Uop (Lognot, e) }
  | TILDE e=x3        { loc $startpos $endpos @@ Uop (Bitnot, e) }
  | INT                 { loc $startpos $endpos @@ CInt }
  | LPAREN e=x1 RPAREN { e } 
  

x5:
  | VAR id=ident EQ init=x2 { loc $startpos $endpos @@ {id; init} }

e1: 
  | d=x5 SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=x4 EQ e=x1 SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=x1 LPAREN COMMA es=x1 RPAREN SEMI { loc $startpos $endpos @@ SCall (e, es) }
  | IF e=bool_expr THEN s1=e1 ELSE s2=e1  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=x1 SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=x1 RPAREN b=e1  { loc $startpos $endpos @@ While(e, b) } 
  | NULL  { loc $startpos $endpos @@ CNull }

x2:
  | x3 { $1 }
  | e1=x2 STAR e2=x3 { loc $startpos $endpos @@ Bop (Mul, e1, e2) }
  | e1=x2 EQEQ e2=x3 { loc $startpos $endpos @@ Bop (Eq, e1, e2) }
  ;

x1:
  | x2 { $1 }
  | e1=x1 PLUS e2=x1 { loc $startpos $endpos @@ Bop (Add, e1, e2) }
  | e1=x1 DASH e2=x1 { loc $startpos $endpos @@ Bop (Sub, e1, e2) }
  | DASH e=x1         { loc $startpos $endpos @@ Uop (Neg, e) }
  ;

