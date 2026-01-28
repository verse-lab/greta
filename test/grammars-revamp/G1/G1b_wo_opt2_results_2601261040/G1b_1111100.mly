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
  | expr3  { $1 }
  | IF cond6 THEN expr1 ELSE expr1  {  If ($2, Then ($4, Else $6))  }
  ;

expr3:
  | expr4 MUL expr3  {  Mul ($1, $3)  }
  | expr4  { $1 }
  ;

expr4:
  | expr9  { $1 }
  | IF cond7 THEN expr4  {  If ($2, Then ($4, Else Na))  }
  ;

expr5:
  | expr8  { $1 }
  ;

expr8:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  ;

expr9:
  | expr9 PLUS expr8  {  Plus ($1, $3)  }
  | expr5  {  $1  }
  ;

prog1:
  | expr2 EOF  {  $1  }
  ;
