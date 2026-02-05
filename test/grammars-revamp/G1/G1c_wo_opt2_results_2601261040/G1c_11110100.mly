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
  | cond8  { $1 }
  ;

cond8:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr2:
  | IF cond6 THEN expr1 ELSE expr1  {  If ($2, Then ($4, Else $6))  }
  | expr3  { $1 }
  ;

expr3:
  | expr4 MUL expr3  {  Mul ($1, $3)  }
  | expr4  { $1 }
  ;

expr4:
  | expr5  { $1 }
  | IF cond8 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  ;

expr5:
  | expr5 PLUS expr7  {  Plus ($1, $3)  }
  | expr7  { $1 }
  ;

expr7:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  ;

prog1:
  | expr2 EOF  {  $1  }
  ;
