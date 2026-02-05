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

cond4:
  | FALSE  {   Bool false   }
  | TRUE  {   Bool true   }
  ;

expr1:
  | IF cond4 THEN expr1  {   If ($2, Then ($4, Else Na))   }
  | LPAREN expr5 RPAREN  {   Paren $2   }
  | INT  {   Int $1   }
  ;

expr2:
  | expr1 MUL expr2  {   Mul ($1, $3)   }
  | expr1  {  $1  }
  ;

expr3:
  | expr2 PLUS expr3  {   Plus ($1, $3)   }
  | expr2  {  $1  }
  ;

expr5:
  | IF cond4 THEN expr5 ELSE expr5  {   If ($2, Then ($4, Else $6))   }
  | expr3  {  $1  }
  ;

prog1:
  | expr5 EOF  {   $1   }
  ;
