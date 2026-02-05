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
  | expr2  { $1 }
  | IF cond6 THEN expr1  {  If ($2, Then ($4, Else Na))  }
  ;

expr2:
  | expr3  { $1 }
  | expr2 PLUS expr4  {  Plus ($1, $3)  }
  ;

expr3:
  | expr5 MUL expr3  {  Mul ($1, $3)  }
  | expr5  { $1 }
  ;

expr4:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  ;

expr5:
  | expr8  { $1 }
  | IF cond6 THEN expr5 ELSE expr5  {  If ($2, Then ($4, Else $6))  }
  ;

expr8:
  | expr4  {  $1  }
  ;

prog7:
  | expr1 EOF  {  $1  }
  ;
