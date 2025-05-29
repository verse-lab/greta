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
%type <Ast.exp> x1
%type <Ast.cond> bool_expr
%type <Ast.stmt> e1
%type <Ast.prog> prog
%%

prog:
  | e1 EOF  { $1 }

ident:
  | id=IDENT { loc $startpos $endpos id }

bool_expr:
  | TRUE { loc $startpos $endpos @@ CBool true }
  | FALSE { loc $startpos $endpos @@ CBool false }

x7:  
  | x4 LBRACKET x4 RBRACKET { loc $startpos $endpos @@ Index ($1, $3) }

e1: 
  | d=x8 SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=x7 EQ e=x1 SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=x1 LPAREN COMMA es=x1 RPAREN SEMI { loc $startpos $endpos @@ SCall (e, es) }
  | IF e=bool_expr THEN s1=e1 ELSE s2=e1  { loc $startpos $endpos @@ If(e,[s1],[s2]) }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=x1 SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=x1 RPAREN b=e1  { loc $startpos $endpos @@ While(e, b) } 
  | NULL  { loc $startpos $endpos @@ CNull }

x6:
  | BANG x6         { loc $startpos $endpos @@ Uop (Lognot, $2) }
  | TILDE x6        { loc $startpos $endpos @@ Uop (Bitnot, $2) }
  | INT                 { loc $startpos $endpos @@ CInt }
  | LPAREN x1 RPAREN { $2 } 
  

x8:
  | VAR id=ident EQ init=x4 { loc $startpos $endpos @@ {id; init} }

x5:
  | x6 { $1 }
  | x5 PLUS x5 { loc $startpos $endpos @@ Bop (Add, $1, $3) }
  | x5 DASH x5 { loc $startpos $endpos @@ Bop (Sub, $1, $3) }
  ;

x4:
  | x5 { $1 }
  | x4 EQEQ x6 { loc $startpos $endpos @@ Bop (Eq, $1, $3) }
  ;

x3:
  | x4 { $1 }
  | DASH x3         { loc $startpos $endpos @@ Uop (Neg, $2) }
  ;

x1:
  | x3 { $1 }
  | x1 STAR x6 { loc $startpos $endpos @@ Bop (Mul, $1, $3) }
  ;

