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
%token EOF%type <Ast.t> prog16
%start prog16
%%

cond10:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond11:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond12:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

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

cond9:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

expr1:
  | IF cond11 THEN expr4 ELSE expr4  {  If ($2, Then ($4, Else $6))  }
  | LPAREN expr8 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | expr1 MUL expr4  {  Mul ($1, $3)  }
  | IF cond13 THEN expr5  {  If ($2, Then ($4, Else Na))  }
  ;

expr2:
  | LPAREN expr8 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | IF cond13 THEN expr5  {  If ($2, Then ($4, Else Na))  }
  | IF cond11 THEN expr4 ELSE expr4  {  If ($2, Then ($4, Else $6))  }
  | expr1 MUL expr4  {  Mul ($1, $3)  }
  ;

expr3:
  | expr1 MUL expr4  {  Mul ($1, $3)  }
  | LPAREN expr8 RPAREN  {  Paren $2  }
  | IF cond13 THEN expr5  {  If ($2, Then ($4, Else Na))  }
  | IF cond11 THEN expr4 ELSE expr4  {  If ($2, Then ($4, Else $6))  }
  | INT  {  Int $1  }
  ;

expr4:
  | INT  {  Int $1  }
  | IF cond13 THEN expr5  {  If ($2, Then ($4, Else Na))  }
  | IF cond11 THEN expr4 ELSE expr4  {  If ($2, Then ($4, Else $6))  }
  | LPAREN expr8 RPAREN  {  Paren $2  }
  ;

expr5:
  | INT  {  Int $1  }
  | LPAREN expr8 RPAREN  {  Paren $2  }
  | IF cond13 THEN expr5  {  If ($2, Then ($4, Else Na))  }
  ;

expr6:
  | INT  {  Int $1  }
  | LPAREN expr8 RPAREN  {  Paren $2  }
  ;

expr7:
  | expr3  {  $1  }
  | expr8 PLUS expr3  {  Plus ($1, $3)  }
  ;

expr8:
  | expr8 PLUS expr3  {  Plus ($1, $3)  }
  | expr3  {  $1  }
  ;

prog16:
  | expr7 EOF  {  $1  }
  ;
