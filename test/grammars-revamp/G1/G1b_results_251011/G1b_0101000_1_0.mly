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

%token EOF%type <Ast.t> prog1
%start prog1
%%

cond3:
  | FALSE  { Bool false }
  | TRUE  { Bool true }
  ;

expr1:
  | expr4  { $1 }
  | IF cond3 THEN expr1  { If ($2, Then ($4, Else Na)) }
  ;

expr2:
  | expr1 MUL expr2  { Mul ($1, $3) }
  | expr1  { $1 }
  ;

expr4:
  | LPAREN expr6 RPAREN  { Paren $2 }
  | INT  { Int $1 }
  ;

expr5:
  | expr2  { $1 }
  | IF cond3 THEN expr5 ELSE expr5  { If ($2, Then ($4, Else $6)) }
  ;

expr6:
  | expr6 PLUS expr4  { Plus ($1, $3) }
  | expr5  { $1 }
  ;

prog1:
  | expr6 EOF  { $1 }
  ;
