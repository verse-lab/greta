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

x7:
  | TINT id=ident EQ init=x4 { loc $startpos $endpos @@ {id; init} }

x7:
  | id=ident            { loc $startpos $endpos @@ Id (id) }
  | c=const             { loc $startpos $endpos @@ Const (c) }
  | LPAREN e=x3 RPAREN { e }
  ;

x5:
  | e1=x7 STAR e2=x5  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  | x7 { $1 }
  ;

x2:
  | d=x6 SEMI                      { loc $startpos $endpos @@ Decl(d) }
  | id=ident EQ e=x3 SEMI           { loc $startpos $endpos @@ Assn(id, e) }
  | WHILE LPAREN e=x3 RPAREN s=x2 { loc $startpos $endpos @@ While(e, [s]) }
  | RETURN e=x3 SEMI                { loc $startpos $endpos @@ Ret(e) }
  | LBRACE ss=x8 RBRACE           { loc $startpos $endpos @@ Block(ss) }
  | IF LPAREN e=x3 RPAREN s1=x2 ELSE s2=x2 { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  ;

x1:
  | x2 { $1 }
  | IF LPAREN e=x3 RPAREN s1=x1   { loc $startpos $endpos @@ If(e, [s1], []) }
  ;

x4:
  | e1=x5 DASH e2=x4  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  | x5 { $1 }
  ;

x3:
  | e1=x4 PLUS e2=x3  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | x4 { $1 }
  ;

e1:
  | id=x8            { loc $startpos $endpos @@ Id (id) }
  | s=x1 ss=e1   { s::ss }
  ;

