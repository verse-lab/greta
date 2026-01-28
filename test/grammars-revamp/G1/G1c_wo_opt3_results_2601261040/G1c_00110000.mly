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
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | IF cond5 THEN expr3 ELSE expr3  {  If ($2, Then ($4, Else $6))  }
  | INT  {  Int $1  }
  | expr2 MUL expr1  {  Mul ($1, $3)  }
  | expr2 PLUS expr3  {  Plus ($1, $3)  }
  | IF cond5 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  ;

expr2:
  | IF cond5 THEN expr3 ELSE expr3  {  If ($2, Then ($4, Else $6))  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | expr2 PLUS expr3  {  Plus ($1, $3)  }
  | IF cond5 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  | INT  {  Int $1  }
  ;

expr3:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | IF cond5 THEN expr3 ELSE expr3  {  If ($2, Then ($4, Else $6))  }
  | IF cond5 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  ;

expr4:
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | IF cond5 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  | INT  {  Int $1  }
  ;

prog1:
  | expr1 EOF  {  $1  }
  ;
