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
  | cond6  { $1 }
  ;

cond6:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr2:
  | expr3  { $1 }
  | expr3 MUL expr1  {  Mul ($1, $3)  }
  ;

expr3:
  | IF cond5 THEN expr3  {  If ($2, Then ($4, Else Na))  }
  | expr4  { $1 }
  ;

expr4:
  | LPAREN expr7 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | IF cond6 THEN expr4 ELSE expr4  {  If ($2, Then ($4, Else $6))  }
  ;

expr7:
  | expr8  { $1 }
  ;

expr8:
  | expr2  {  $1  }
  | expr7 PLUS expr2  {  Plus ($1, $3)  }
  ;

prog1:
  | expr8 EOF  {  $1  }
  ;
