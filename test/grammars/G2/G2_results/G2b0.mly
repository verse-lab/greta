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


%start prog0
%type <Ast.prog> prog0
%type <Ast.exp> x5
%type <Ast.const> const
%%



prog0:
  | p=e1 EOF  { p }
  ;

ident:
  | id=IDENT  { loc $startpos $endpos id }
  ;

const:
  | i=INT { loc $startpos $endpos @@ CInt i }
  ;

x6:
  | TINT id=ident EQ init=x2 { loc $startpos $endpos @@ {id; init} }
  ;

x5:
  | e1=x5 PLUS e2=x2  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  ;

x7:
  | id=x8            { loc $startpos $endpos @@ Id (id) }
  | e1=x8 STAR e2=x7  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  ;

x4:
  | id=x8            { loc $startpos $endpos @@ Id (id) }
  | IF LPAREN e=x5 RPAREN s1=x4 ELSE s2=x4 { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  ;

x3:
  |   /* empty */   { [] }
  | id=x8            { loc $startpos $endpos @@ Id (id) }
  ;

x2:
  | id=x7            { loc $startpos $endpos @@ Id (id) }
  | e1=x2 DASH e2=x2  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  | LPAREN e=x8 RPAREN { e }
  ;

x1:
  | d=x6 SEMI                      { loc $startpos $endpos @@ Decl(d) }
  | id=x4            { loc $startpos $endpos @@ Id (id) }
  | id=ident EQ e=x5 SEMI           { loc $startpos $endpos @@ Assn(id, e) }
  | WHILE LPAREN e=x5 RPAREN s=x1 { loc $startpos $endpos @@ While(e, [s]) }
  | RETURN e=x5 SEMI                { loc $startpos $endpos @@ Ret(e) }
  | LBRACE ss=x3 RBRACE           { loc $startpos $endpos @@ Block(ss) }
  | IF LPAREN e=x5 RPAREN s1=x1   { loc $startpos $endpos @@ If(e, [s1], []) }
  ;

e1:
  | id=x3            { loc $startpos $endpos @@ Id (id) }
  | s=x1 ss=e1   { s::ss }
  ;

