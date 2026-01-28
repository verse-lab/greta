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
  | INT  {   Int $1   }
  | LPAREN expr6 RPAREN  {   Paren $2   }
  ;

expr2:
  | expr1  {  $1  }
  | expr2 MUL expr1  {   Mul ($1, $3)   }
  ;

expr3:
  | IF cond5 THEN expr3  {   If ($2, Then ($4, Else Na))   }
  | expr2  {  $1  }
  ;

expr4:
  | expr3 PLUS expr4  {   Plus ($1, $3)   }
  | expr3  {  $1  }
  ;

expr6:
  | IF cond5 THEN expr6 ELSE expr6  {   If ($2, Then ($4, Else $6))   }
  | expr4  {  $1  }
  ;

prog1:
  | expr6 EOF  {   $1   }
  ;
