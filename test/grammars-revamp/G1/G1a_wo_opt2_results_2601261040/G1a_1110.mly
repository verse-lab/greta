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
  | IF cond6 THEN expr1 ELSE expr1  {  If ($2, Then ($4, Else $6))  }
  | expr3  { $1 }
  ;

expr3:
  | IF cond5 THEN expr3  {  If ($2, Then ($4, Else Na))  }
  | expr4  { $1 }
  ;

expr4:
  | expr7  { $1 }
  | expr7 MUL expr4  {  Mul ($1, $3)  }
  ;

expr7:
  | LPAREN expr8 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  ;

expr8:
  | expr9  { $1 }
  ;

expr9:
  | expr8 PLUS expr2  {  Plus ($1, $3)  }
  | expr2  {  $1  }
  ;

prog1:
  | expr9 EOF  {  $1  }
  ;
