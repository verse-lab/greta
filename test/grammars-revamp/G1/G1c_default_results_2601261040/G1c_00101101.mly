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

cond5:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

expr1:
  | expr2  { $1 }
  | expr1 MUL expr2  {  Mul ($1, $3)  }
  ;

expr2:
  | expr3  { $1 }
  | IF cond5 THEN expr2  {  If ($2, Then ($4, Else Na))  }
  ;

expr3:
  | expr4 PLUS expr3  {  Plus ($1, $3)  }
  | expr4  { $1 }
  ;

expr4:
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | IF cond5 THEN expr4 ELSE expr4  {  If ($2, Then ($4, Else $6))  }
  | INT  {  Int $1  }
  ;

prog1:
  | expr1 EOF  {  $1  }
  ;
