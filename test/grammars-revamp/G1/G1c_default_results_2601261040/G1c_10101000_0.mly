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

cond2:
  | FALSE  {   Bool false   }
  | TRUE  {   Bool true   }
  ;

expr1:
  | IF cond2 THEN expr1  {   If ($2, Then ($4, Else Na))   }
  | LPAREN expr5 RPAREN  {   Paren $2   }
  | INT  {   Int $1   }
  ;

expr3:
  | expr1 PLUS expr3  {   Plus ($1, $3)   }
  | expr1  {  $1  }
  ;

expr4:
  | expr3  {  $1  }
  | IF cond2 THEN expr4 ELSE expr4  {   If ($2, Then ($4, Else $6))   }
  ;

expr5:
  | expr4  {  $1  }
  | expr5 MUL expr4  {   Mul ($1, $3)   }
  ;

prog1:
  | expr5 EOF  {   $1   }
  ;
