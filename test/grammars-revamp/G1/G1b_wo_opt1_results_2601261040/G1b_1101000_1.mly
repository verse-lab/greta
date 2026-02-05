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
  | expr8  {  $1  }
  | IF cond6 THEN expr1  {   If ($2, Then ($4, Else Na))   }
  ;

expr2:
  | LPAREN expr5 RPAREN  {   Paren $2   }
  | INT  {   Int $1   }
  ;

expr3:
  | expr1  {  $1  }
  | expr1 MUL expr3  {   Mul ($1, $3)   }
  ;

expr4:
  | expr4 PLUS expr2  {   Plus ($1, $3)   }
  | expr3  {  $1  }
  ;

expr5:
  | IF cond6 THEN expr5 ELSE expr5  {   If ($2, Then ($4, Else $6))   }
  | expr4  {  $1  }
  ;

expr8:
  | expr2  {   $1   }
  ;

prog7:
  | expr5 EOF  {   $1   }
  ;
