/* *** G0e *** */
// 7 po's 2 assoc's
// if2 vs. +
// if2 vs. *
// * vs. +
// + vs. *
// if1 vs. +
// if1 vs. * 
// if1 vs. if2
// * assoc
// + assoc

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

cond3:
  | TRUE  { Bool true }
  | FALSE  { Bool false }
  ;

expr1:
  | LPAREN expr5 RPAREN  { Paren $2 }
  | IF cond3 THEN expr1  { If ($2, Then ($4, Else Na)) }
  | INT  { Int $1 }
  ;

expr2:
  | expr1  { $1 }
  | expr1 MUL expr2  { Mul ($1, $3) }
  ;

expr4:
  | expr2  { $1 }
  | expr2 PLUS expr4  { Plus ($1, $3) }
  ;

expr5:
  | IF cond3 THEN expr5 ELSE expr5  { If ($2, Then ($4, Else $6)) }
  | expr4  { $1 }
  ;

prog1:
  | expr5 EOF  { $1 }
  ;
