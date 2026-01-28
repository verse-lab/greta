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
  | IF cond4 THEN expr3 ELSE expr3  {   If ($2, Then ($4, Else $6))   }
  | expr2 MUL expr1  {   Mul ($1, $3)   }
  | expr5  {   $1   }
  | expr2 PLUS expr5  {   Plus ($1, $3)   }
  | IF cond4 THEN expr6  {   If ($2, Then ($4, Else Na))   }
  ;

expr2:
  | expr2 PLUS expr5  {   Plus ($1, $3)   }
  | IF cond4 THEN expr6  {   If ($2, Then ($4, Else Na))   }
  | IF cond4 THEN expr3 ELSE expr3  {   If ($2, Then ($4, Else $6))   }
  | expr5  {   $1   }
  ;

expr3:
  | expr5  {   $1   }
  | IF cond4 THEN expr3 ELSE expr3  {   If ($2, Then ($4, Else $6))   }
  | IF cond4 THEN expr6  {   If ($2, Then ($4, Else Na))   }
  ;

expr5:
  | LPAREN expr1 RPAREN  {   Paren $2   }
  | INT  {   Int $1   }
  ;

expr6:
  | IF cond4 THEN expr6  {   If ($2, Then ($4, Else Na))   }
  | expr5  {   $1   }
  ;

prog1:
  | expr1 EOF  {   $1   }
  ;
