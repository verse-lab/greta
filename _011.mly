/* *** G0a *** */
// 3 po's 1 assoc
// if2 vs. *
// * assoc
// if1 vs. *
/* ---------- */
// if1 vs. if2

%{
  open Ast
%}

%token <int> INT

%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE

%token PLUS
%token MUL

%token LPAREN
%token RPAREN

%token EOF

%type <Ast.t> prog1
%start prog1
%%

cond5:
  | TRUE  { Bool true }
  | FALSE  { Bool false }
  | expr7  { $1 }
  ;

expr1:
  | expr2  { $1 }
  | IF cond5 THEN expr1  { If ($2, Then ($4, Else Na)) }
  ;

expr2:
  | expr3  { $1 }
  | expr2 MUL expr3  { Mul ($1, $3) }
  ;

expr3:
  | expr4  { $1 }
  | IF cond5 THEN expr3  { If ($2, Then ($4, Else Na)) }
  | IF cond5 THEN expr3 ELSE expr3  { If ($2, Then ($4, Else $6)) }
  ;

expr4:
  | INT  { Int $1 }
  | IF cond5 THEN expr4 ELSE expr4  { If ($2, Then ($4, Else $6)) }
  | expr7  { $1 }
  | LPAREN expr7 RPAREN  { Paren $2 }
  ;

expr6:
  | expr7  { $1 }
  | expr6 PLUS expr1  { Plus ($1, $3) }
  | expr1  { $1 }
  ;

prog1:
  | expr6 EOF  { $1 }
  | expr7  { $1 }
  ;
