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

cond4:
  | FALSE  { Bool false }
  | TRUE  { Bool true }
  ;

expr1:
  | expr2 MUL expr1  { Mul ($1, $3) }
  | expr2  { $1 }
  ;

expr2:
  | IF cond4 THEN expr2  { If ($2, Then ($4, Else Na)) }
  | expr3  { $1 }
  ;

expr3:
  | LPAREN expr5 RPAREN  { Paren $2 }
  | IF cond4 THEN expr3 ELSE expr3  { If ($2, Then ($4, Else $6)) }
  | INT  { Int $1 }
  ;

expr5:
  | expr5 PLUS expr1  { Plus ($1, $3) }
  | expr1  { $1 }
  ;

prog1:
  | expr5 EOF  { $1 }
  ;
