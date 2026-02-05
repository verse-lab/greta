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

cond4:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

expr1:
  | LPAREN expr5 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | expr2 MUL expr3  {  Mul ($1, $3)  }
  | IF cond4 THEN expr3 ELSE expr3  {  If ($2, Then ($4, Else $6))  }
  | IF cond4 THEN expr1  {  If ($2, Then ($4, Else Na))  }
  ;

expr2:
  | IF cond4 THEN expr3 ELSE expr3  {  If ($2, Then ($4, Else $6))  }
  | LPAREN expr5 RPAREN  {  Paren $2  }
  | expr2 MUL expr3  {  Mul ($1, $3)  }
  | INT  {  Int $1  }
  ;

expr3:
  | LPAREN expr5 RPAREN  {  Paren $2  }
  | IF cond4 THEN expr3 ELSE expr3  {  If ($2, Then ($4, Else $6))  }
  | INT  {  Int $1  }
  ;

expr5:
  | expr5 PLUS expr1  {  Plus ($1, $3)  }
  | expr1  {  $1  }
  ;

prog1:
  | expr5 EOF  {  $1  }
  ;
