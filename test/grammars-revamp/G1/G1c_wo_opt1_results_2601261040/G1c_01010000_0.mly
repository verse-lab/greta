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
%token EOF%type <Ast.t> prog7
%start prog7
%%

cond6:
  | FALSE  {   Bool false   }
  | TRUE  {   Bool true   }
  ;

expr1:
  | INT  {   Int $1   }
  | LPAREN expr5 RPAREN  {   Paren $2   }
  ;

expr2:
  | expr1  {  $1  }
  | IF cond6 THEN expr2  {   If ($2, Then ($4, Else Na))   }
  ;

expr3:
  | expr2  {  $1  }
  | expr2 MUL expr3  {   Mul ($1, $3)   }
  ;

expr4:
  | expr3  {  $1  }
  | IF cond6 THEN expr4 ELSE expr4  {   If ($2, Then ($4, Else $6))   }
  ;

expr5:
  | expr5 PLUS expr4  {   Plus ($1, $3)   }
  | expr4  {  $1  }
  ;

prog7:
  | expr5 EOF  {   $1   }
  ;
