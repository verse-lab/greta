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
  | IF cond6 THEN expr1 ELSE expr1  {  If ($2, Then ($4, Else $6))  }
  | expr3  { $1 }
  ;

expr3:
  | expr3 PLUS expr4  {  Plus ($1, $3)  }
  | expr4  { $1 }
  ;

expr4:
  | expr5  { $1 }
  | expr4 MUL expr5  {  Mul ($1, $3)  }
  ;

expr5:
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | IF cond7 THEN expr5  {  If ($2, Then ($4, Else Na))  }
  ;

prog1:
  | expr2 EOF  {  $1  }
  ;
