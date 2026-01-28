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

cond6:
  | cond7  { $1 }
  ;

cond7:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr2:
  | expr1 MUL expr3  {  Mul ($1, $3)  }
  | expr3  { $1 }
  ;

expr3:
  | expr4 PLUS expr3  {  Plus ($1, $3)  }
  | expr4  { $1 }
  ;

expr4:
  | expr5  { $1 }
  | IF cond7 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  ;

expr5:
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | IF cond6 THEN expr5 ELSE expr5  {  If ($2, Then ($4, Else $6))  }
  | INT  {  Int $1  }
  ;

prog1:
  | expr2 EOF  {  $1  }
  ;
