%{
open Ast;;

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a loc =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

%token EOF
%token <int64>  INT
%token <string> IDENT
%token <string> STRING
%token ELSE     /* else */
%token IF       /* if */
%token TINT     /* int */
%token RETURN   /* return */
%token WHILE    /* while */
%token SEMI     /* ; */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token PLUS     /* + */
%token DASH     /* - */
%token STAR     /* * */
%token EQ       /* = */
%token LPAREN   /* ( */
%token RPAREN   /* ) */

%left DASH

%start prog0
%type <Ast.prog> prog0
%type <Ast.exp> x3
%type <Ast.const> const
%%


prog0:
  | p=e1 EOF  { p }
  ;

e1:
  | x5            { $1 }
  | s=x1 ss=e1   { s::ss }
  ;

x1:
  | IF LPAREN e=x3 RPAREN s1=x1   { loc $startpos $endpos @@ If(e, [s1], []) }
  | x4 { $1 }
  ;

x2:
  | TINT id=ident EQ init=x6 { loc $startpos $endpos @@ {id; init} }
  ;

x3:
  | e1=x6 PLUS e2=x7  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | x8 { $1 }
  ;

x4:
  | d=x2 SEMI                      { loc $startpos $endpos @@ Decl(d) }
  | id=ident EQ e=x3 SEMI           { loc $startpos $endpos @@ Assn(id, e) }
  | IF LPAREN e=x3 RPAREN s1=x4 ELSE s2=x4 { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  | RETURN e=x3 SEMI                { loc $startpos $endpos @@ Ret(e) }
  | WHILE LPAREN e=x3 RPAREN s=x4 { loc $startpos $endpos @@ While(e, [s]) }
  | LBRACE ss=x5 RBRACE           { loc $startpos $endpos @@ Block(ss) }
  ;

x5:
  |   /* empty */   { [] }
  ;

x6:
  | e1=x6 PLUS e2=x7  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | x7 { $1 }
  ;

x8:
  | e1=x8 DASH e2=x8  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  | x7 { $1 }
  ;

x7:
  | e1=x9 STAR e2=x7  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  | LPAREN e=x6 RPAREN { e }
  | x9 { $1 }
  ;

x9:
  | id=ident            { loc $startpos $endpos @@ Id (id) }
  | c=const             { loc $startpos $endpos @@ Const (c) }
  | LPAREN e=x3 RPAREN { e }
  ;

ident:
  | id=IDENT  { loc $startpos $endpos id }
  ;

const:
  | i=INT { loc $startpos $endpos @@ CInt i }
  ;

