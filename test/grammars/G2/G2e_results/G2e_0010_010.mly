/* *** G2e *** */
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



%start toplevel
%type <Ast.prog> toplevel
%type <Ast.exp> x3
%type <Ast.const> const
%%

toplevel:
  | p=e1 EOF  { p }

ident:
  | id=IDENT  { loc $startpos $endpos id }

const:
  | i=INT { loc $startpos $endpos @@ CInt i }

x11:
  | TINT id=ident EQ init=x7 { loc $startpos $endpos @@ {id; init} }

x9:
  | id=ident            { loc $startpos $endpos @@ Id (id) }
  | c=const             { loc $startpos $endpos @@ Const (c) }
  | LPAREN e=x4 RPAREN { e }

x8:
  | e1=x9 PLUS e2=x7  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | id=x9            { loc $startpos $endpos @@ Id (id) }
  ;

x7:
  | id=x8            { loc $startpos $endpos @@ Id (id) }
  ;

x6:
  | id=x12            { loc $startpos $endpos @@ Id (id) }
  | d=x11 SEMI                      { loc $startpos $endpos @@ Decl(d) }
  | id=ident EQ e=x3 SEMI           { loc $startpos $endpos @@ Assn(id, e) }
  | WHILE LPAREN e=x3 RPAREN s=x6 { loc $startpos $endpos @@ While(e, [s]) }
  | RETURN e=x3 SEMI                { loc $startpos $endpos @@ Ret(e) }
  | LBRACE ss=e1 RBRACE           { loc $startpos $endpos @@ Block(ss) }
  | IF LPAREN e=x3 RPAREN s1=x6 ELSE s2=x6 { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  ;

x5:
  | e1=x8 STAR e2=x5  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  | id=x7            { loc $startpos $endpos @@ Id (id) }
  | e1=x3 DASH e2=x8  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  | id=x12            { loc $startpos $endpos @@ Id (id) }
  | LPAREN e=x4 RPAREN { e }
  ;

x4:
  | e1=x8 STAR e2=x5  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  | id=x7            { loc $startpos $endpos @@ Id (id) }
  | e1=x3 DASH e2=x8  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  | id=e1            { loc $startpos $endpos @@ Id (id) }
  ;

x3:
  | id=x7            { loc $startpos $endpos @@ Id (id) }
  | e1=x3 DASH e2=x8  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  ;

x2:
  | id=x6            { loc $startpos $endpos @@ Id (id) }
  | IF LPAREN e=x5 RPAREN s1=x2   { loc $startpos $endpos @@ If(e, [s1], []) }
  ;

x1:
  | x2 { $1 }
  | d=x11 SEMI                      { loc $startpos $endpos @@ Decl(d) }
  | id=ident EQ e=x3 SEMI           { loc $startpos $endpos @@ Assn(id, e) }
  | id=e1            { loc $startpos $endpos @@ Id (id) }
  | WHILE LPAREN e=x3 RPAREN s=x6 { loc $startpos $endpos @@ While(e, [s]) }
  | RETURN e=x3 SEMI                { loc $startpos $endpos @@ Ret(e) }
  | LBRACE ss=e1 RBRACE           { loc $startpos $endpos @@ Block(ss) }
  | IF LPAREN e=x3 RPAREN s1=x6 ELSE s2=x6 { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  ;

e1:
  | s=x1 ss=e1   { s::ss }
  | id=e1            { loc $startpos $endpos @@ Id (id) }
  ;

