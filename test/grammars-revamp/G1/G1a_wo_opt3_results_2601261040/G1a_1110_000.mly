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
  | TRUE  {   Bool true   }
  | FALSE  {   Bool false   }
  ;

expr1:
  | expr4 MUL expr3  {   Mul ($1, $3)   }
  | IF cond5 THEN expr1 ELSE expr1  {   If ($2, Then ($4, Else $6))   }
  | LPAREN expr6 RPAREN  {   Paren $2   }
  | INT  {   Int $1   }
  | IF cond5 THEN expr2  {   If ($2, Then ($4, Else Na))   }
  ;

expr2:
  | LPAREN expr6 RPAREN  {   Paren $2   }
  | INT  {   Int $1   }
  | IF cond5 THEN expr2  {   If ($2, Then ($4, Else Na))   }
  | expr4 MUL expr3  {   Mul ($1, $3)   }
  ;

expr3:
  | INT  {   Int $1   }
  | LPAREN expr6 RPAREN  {   Paren $2   }
  | expr4 MUL expr3  {   Mul ($1, $3)   }
  ;

expr4:
  | INT  {   Int $1   }
  | LPAREN expr6 RPAREN  {   Paren $2   }
  ;

expr6:
  | expr6 PLUS expr1  {   Plus ($1, $3)   }
  | expr1  {   $1   }
  ;

prog1:
  | expr6 EOF  {   $1   }
  ;
