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

expr10:
  | expr5  {  $1  }
  ;

expr2:
  | expr3  { $1 }
  | IF cond8 THEN expr1  {  If ($2, Then ($4, Else Na))  }
  ;

expr3:
  | expr3 PLUS expr9  {  Plus ($1, $3)  }
  | expr4  { $1 }
  ;

expr4:
  | expr7  { $1 }
  | IF cond6 THEN expr4 ELSE expr4  {  If ($2, Then ($4, Else $6))  }
  ;

expr5:
  | expr9  { $1 }
  ;

expr7:
  | expr10 MUL expr7  {  Mul ($1, $3)  }
  | expr10  { $1 }
  ;

expr9:
  | LPAREN expr1 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  ;

prog1:
  | expr2 EOF  {  $1  }
  ;
