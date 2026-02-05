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

cond7:
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
  | expr3  { $1 }
  | expr1 PLUS expr5  {  Plus ($1, $3)  }
  ;

expr3:
  | IF cond8 THEN expr3  {  If ($2, Then ($4, Else Na))  }
  | expr4  { $1 }
  ;

expr4:
  | expr6  { $1 }
  | expr4 MUL expr6  {  Mul ($1, $3)  }
  ;

expr5:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  ;

expr6:
  | IF cond7 THEN expr6 ELSE expr6  {  If ($2, Then ($4, Else $6))  }
  | expr5  {  $1  }
  ;

prog1:
  | expr2 EOF  {  $1  }
  ;
