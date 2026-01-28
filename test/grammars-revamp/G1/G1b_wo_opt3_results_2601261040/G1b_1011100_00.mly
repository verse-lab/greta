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
  | TRUE  {   Bool true   }
  | FALSE  {   Bool false   }
  ;

expr1:
  | IF cond4 THEN expr2 ELSE expr2  {   If ($2, Then ($4, Else $6))   }
  | expr2 MUL expr1  {   Mul ($1, $3)   }
  | expr5  {   $1   }
  | expr6 PLUS expr5  {   Plus ($1, $3)   }
  | IF cond4 THEN expr3  {   If ($2, Then ($4, Else Na))   }
  ;

expr2:
  | expr6 PLUS expr5  {   Plus ($1, $3)   }
  | IF cond4 THEN expr3  {   If ($2, Then ($4, Else Na))   }
  | IF cond4 THEN expr2 ELSE expr2  {   If ($2, Then ($4, Else $6))   }
  | expr5  {   $1   }
  ;

expr3:
  | expr5  {   $1   }
  | expr6 PLUS expr5  {   Plus ($1, $3)   }
  | IF cond4 THEN expr3  {   If ($2, Then ($4, Else Na))   }
  ;

expr5:
  | INT  {   Int $1   }
  | LPAREN expr1 RPAREN  {   Paren $2   }
  ;

expr6:
  | expr6 PLUS expr5  {   Plus ($1, $3)   }
  | expr5  {   $1   }
  ;

prog1:
  | expr1 EOF  {   $1   }
  ;
