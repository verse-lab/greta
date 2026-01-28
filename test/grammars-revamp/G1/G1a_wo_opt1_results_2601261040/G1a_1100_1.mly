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
  | expr1 PLUS expr5  {   Plus ($1, $3)   }
  | expr5  {   $1   }
  ;

expr2:
  | LPAREN expr1 RPAREN  {   Paren $2   }
  | INT  {   Int $1   }
  ;

expr3:
  | expr2  {  $1  }
  | IF cond6 THEN expr3  {   If ($2, Then ($4, Else Na))   }
  ;

expr4:
  | expr3 MUL expr4  {   Mul ($1, $3)   }
  | expr3  {  $1  }
  ;

expr5:
  | IF cond6 THEN expr5 ELSE expr5  {   If ($2, Then ($4, Else $6))   }
  | expr4  {  $1  }
  ;

prog7:
  | expr1 EOF  {   $1   }
  ;
