// 8 conflicts
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

%left DASH STAR EQEQ





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

x5:  
  | e=x4 LBRACKET i=x4 RBRACKET { loc $startpos $endpos @@ Index (e, i) }

x4:
  | BANG e=x4         { loc $startpos $endpos @@ Uop (Lognot, e) }
  | TILDE e=x4        { loc $startpos $endpos @@ Uop (Bitnot, e) }
  | INT                 { loc $startpos $endpos @@ CInt }
  | LPAREN e=x1 RPAREN { e } 
  

x6:
  | VAR id=ident EQ init=x4 { loc $startpos $endpos @@ {id; init} }

e1: 
  | d=x6 SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=x5 EQ e=x2 SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=x2 LPAREN COMMA es=x2 RPAREN SEMI { loc $startpos $endpos @@ SCall (e, es) }
  | IF e=bool_expr THEN s1=e1 ELSE s2=e1  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=x2 SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=x2 RPAREN b=e1  { loc $startpos $endpos @@ While(e, b) } 
  | NULL  { loc $startpos $endpos @@ CNull }

x1:
  | x2 { $1 }
  | e1=x1 STAR e2=x1 { loc $startpos $endpos @@ Bop (Mul, e1, e2) }
  | DASH e=x1         { loc $startpos $endpos @@ Uop (Neg, e) }
  ;

x3:
  | x4 { $1 }
  | e1=x3 EQEQ e2=x3 { loc $startpos $endpos @@ Bop (Eq, e1, e2) }
  ;

x2:
  | x3 { $1 }
  | e1=x2 PLUS e2=x3 { loc $startpos $endpos @@ Bop (Add, e1, e2) }
  | e1=x2 DASH e2=x2 { loc $startpos $endpos @@ Bop (Sub, e1, e2) }
  ;

