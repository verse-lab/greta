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
%token EOF%type <Ast.t> prog7
%start prog7
%%

cond6:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

expr1:
  | expr2 MUL expr1  {  Mul ($1, $3)  }
  | expr2  { $1 }
  ;

expr2:
  | IF cond6 THEN expr2  {  If ($2, Then ($4, Else Na))  }
  | expr3  { $1 }
  ;

expr3:
  | IF cond6 THEN expr3 ELSE expr3  {  If ($2, Then ($4, Else $6))  }
  | expr4  { $1 }
  ;

expr4:
  | LPAREN expr5 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  ;

expr5:
  | expr5 PLUS expr1  {  Plus ($1, $3)  }
  | expr1  {  $1  }
  ;

prog7:
  | expr5 EOF  {  $1  }
  ;
