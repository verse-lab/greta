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

%start toplevel
%type <Ast.prog> toplevel
%type <Ast.exp> exp
%type <Ast.const> const
%%


toplevel:
  | p=stmts EOF  { p }

ident:
  | id=IDENT  { loc $startpos $endpos id }

decl:
  | TINT id=ident EQ init=exp { loc $startpos $endpos @@ {id; init} }

const:
  | i=INT { loc $startpos $endpos @@ CInt i }

exp:
  | e1=exp PLUS e2=exp  { loc $startpos $endpos @@ Bop(Add, e1, e2) }
  | e1=exp DASH e2=exp  { loc $startpos $endpos @@ Bop(Sub, e1, e2) }
  | e1=exp STAR e2=exp  { loc $startpos $endpos @@ Bop(Mul, e1, e2) }
  | id=ident            { loc $startpos $endpos @@ Id (id) }
  | c=const             { loc $startpos $endpos @@ Const (c) }
  | LPAREN e=exp RPAREN { e }


stmt1: 
  | d=decl SEMI                      { loc $startpos $endpos @@ Decl(d) }
  | id=ident EQ e=exp SEMI           { loc $startpos $endpos @@ Assn(id, e) }
  | IF LPAREN e=exp RPAREN s1=stmt1   { loc $startpos $endpos @@ If(e, [s1], []) }
  | IF LPAREN e=exp RPAREN s1=stmt1 ELSE s2=stmt1 { loc $startpos $endpos @@ If(e, [s1], [s2]) }
  | stmt2                            { $1 }

stmt2:
  | RETURN e=exp SEMI                { loc $startpos $endpos @@ Ret(e) }
  | WHILE LPAREN e=exp RPAREN s=stmt2 { loc $startpos $endpos @@ While(e, [s]) }
  | LBRACE ss=stmts RBRACE           { loc $startpos $endpos @@ Block(ss) }

stmts:
  |   /* empty */   { [] }
  | s=stmt1 ss=stmts   { s::ss }