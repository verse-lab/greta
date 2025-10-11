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

%token EOF%type <Ast.t> prog1%start prog1
%%

cond2:
  | FALSE  { Bool false }
  | TRUE  { Bool true }
  ;

expr1:
  | expr5  { $1 }
  | IF cond2 THEN expr1  { If ($2, Then ($4, Else Na)) }
  ;

expr3:
  | expr1  { $1 }
  | expr1 MUL expr3  { Mul ($1, $3) }
  ;

expr4:
  | IF cond2 THEN expr4 ELSE expr4  { If ($2, Then ($4, Else $6)) }
  | expr3  { $1 }
  ;

expr5:
  | INT  { Int $1 }
  | LPAREN expr6 RPAREN  { Paren $2 }
  ;

expr6:
  | expr4  { $1 }
  | expr6 PLUS expr5  { Plus ($1, $3) }
  ;

prog1:
  | expr6 EOF  { $1 }
  ;
