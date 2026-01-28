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
%token EOF%type <Ast.t> prog14
%start prog14
%%

cond10:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond11:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond12:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond13:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond7:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

cond8:
  | TRUE  {  Bool true  }
  | FALSE  {  Bool false  }
  ;

cond9:
  | FALSE  {  Bool false  }
  | TRUE  {  Bool true  }
  ;

expr1:
  | IF cond11 THEN expr5 ELSE expr5  {  If ($2, Then ($4, Else $6))  }
  | expr4 MUL expr5  {  Mul ($1, $3)  }
  | expr3 PLUS expr2  {  Plus ($1, $3)  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | IF cond7 THEN expr3  {  If ($2, Then ($4, Else Na))  }
  ;

expr2:
  | LPAREN expr2 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | expr4 MUL expr5  {  Mul ($1, $3)  }
  | IF cond11 THEN expr5 ELSE expr5  {  If ($2, Then ($4, Else $6))  }
  | expr3 PLUS expr2  {  Plus ($1, $3)  }
  | IF cond7 THEN expr3  {  If ($2, Then ($4, Else Na))  }
  ;

expr3:
  | expr4 MUL expr5  {  Mul ($1, $3)  }
  | IF cond11 THEN expr5 ELSE expr5  {  If ($2, Then ($4, Else $6))  }
  | INT  {  Int $1  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  | IF cond7 THEN expr3  {  If ($2, Then ($4, Else Na))  }
  ;

expr4:
  | LPAREN expr2 RPAREN  {  Paren $2  }
  | IF cond11 THEN expr5 ELSE expr5  {  If ($2, Then ($4, Else $6))  }
  | expr4 MUL expr5  {  Mul ($1, $3)  }
  | INT  {  Int $1  }
  ;

expr5:
  | IF cond11 THEN expr5 ELSE expr5  {  If ($2, Then ($4, Else $6))  }
  | INT  {  Int $1  }
  | LPAREN expr2 RPAREN  {  Paren $2  }
  ;

expr6:
  | LPAREN expr2 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  ;

prog14:
  | expr1 EOF  {  $1  }
  ;
