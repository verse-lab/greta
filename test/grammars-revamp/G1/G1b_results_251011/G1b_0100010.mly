/* *** G0d *** */
// 6 po's 1 assoc
// if2 vs. +
// if2 vs. *
// * vs. +
// if1 vs. +
// if1 vs. *
// if1 vs. if2
// * assoc

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

cond6:
  | TRUE  { Bool true }
  | FALSE  { Bool false }
  ;

expr1:
  | expr2  { $1 }
  | expr1 PLUS expr4  { Plus ($1, $3) }
  ;

expr2:
  | expr3  { $1 }
  | IF cond6 THEN expr2 ELSE expr2  { If ($2, Then ($4, Else $6)) }
  ;

expr3:
  | IF cond6 THEN expr3  { If ($2, Then ($4, Else Na)) }
  | expr5  { $1 }
  ;

expr4:
  | INT  { Int $1 }
  | LPAREN expr1 RPAREN  { Paren $2 }
  ;

expr5:
  | expr5 MUL expr7  { Mul ($1, $3) }
  | expr7  { $1 }
  ;

expr7:
  | expr4  { $1 }
  ;

prog1:
  | expr1 EOF  { $1 }
  ;
