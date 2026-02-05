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
%token EOF%type <Ast.t> prog20
%start prog20
%%

cond13:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond14:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond15:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

cond16:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond17:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

cond18:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

cond19:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

expr1:
  | expr11  {  $1  }
  | IF cond19 THEN expr2 ELSE expr2  {  If ($2, Then ($4, Else $6))  }
  | expr12 MUL expr21  {  Mul ($1, $3)  }
  | IF cond15 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  | expr3 PLUS expr5  {  Plus ($1, $3)  }
  ;

expr10:
  | INT  {  Int $1  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  ;

expr11:
  | INT  {  Int $1  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  ;

expr12:
  | expr12 MUL expr21  {  Mul ($1, $3)  }
  | expr11  {  $1  }
  ;

expr2:
  | IF cond19 THEN expr2 ELSE expr2  {  If ($2, Then ($4, Else $6))  }
  | IF cond15 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  | expr11  {  $1  }
  | expr12 MUL expr21  {  Mul ($1, $3)  }
  | expr3 PLUS expr5  {  Plus ($1, $3)  }
  ;

expr21:
  | expr11  {  $1  }
  ;

expr3:
  | expr12 MUL expr21  {  Mul ($1, $3)  }
  | expr11  {  $1  }
  | expr3 PLUS expr5  {  Plus ($1, $3)  }
  | IF cond15 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  ;

expr4:
  | IF cond15 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  | expr12 MUL expr21  {  Mul ($1, $3)  }
  | expr11  {  $1  }
  ;

expr5:
  | INT  {  Int $1  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  ;

expr6:
  | LPAREN expr2 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  ;

expr7:
  | INT  {  Int $1  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  ;

expr8:
  | INT  {  Int $1  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  ;

expr9:
  | LPAREN expr2 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  ;

prog20:
  | expr1 EOF  {  $1  }
  ;
