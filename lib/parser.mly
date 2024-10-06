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

%left PLUS DASH
%left STAR                           
                         
/* ---------------------------------------------------------------------- */
%start prog0
%type <Ast.prog> prog0
%type <Ast.exp> x2
%type <Ast.const> const
%%


prog0:
  | p=e1 EOF  { p }

ident:
  | id=IDENT  { loc $startpos $endpos id }

x5:
  | TINT id=ident EQ init=x2 { loc $startpos $endpos @@ {id; init} }

const:
  | k=INT { loc $startpos $endpos @@ CInt k }

x2:
  | e1=x2 PLUS e2=x2  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | e1=x2 DASH e2=x2  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  | e1=x2 STAR e2=x2  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  | id=ident            { loc $startpos $endpos @@ Id (id) }
  | c=const             { loc $startpos $endpos @@ Const (c) }
  | LPAREN e=x2 RPAREN { e }


x1: 
  | d=x5 SEMI                      { loc $startpos $endpos @@ Decl(d) }
  | id=ident EQ e=x2 SEMI           { loc $startpos $endpos @@ Assn(id, e) }
  | IF LPAREN e=x2 RPAREN s1=x1   { loc $startpos $endpos @@ If(e, [s1], []) }
  | IF LPAREN e=x2 RPAREN s1=x1 ELSE s2=x1
                                     { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  | RETURN e=x2 SEMI                { loc $startpos $endpos @@ Ret(e) }
  | WHILE LPAREN e=x2 RPAREN s=x1 { loc $startpos $endpos @@ While(e, [s]) }
  | LBRACE ss=e1 RBRACE           { loc $startpos $endpos @@ Block(ss) }

e1:
  |   /* empty */   { [] }
  | s=x1 ss=e1   { s::ss }
  