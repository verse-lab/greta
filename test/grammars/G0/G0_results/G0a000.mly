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

%token EOF



%type <Ast.t> prog0
%start prog0
%%



cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;
  

expr2:
  | expr2 MUL expr2 { Mul ($1, $3) }
  | IF cond_expr THEN expr2 { If ($2, Then ($4, Else Na)) }
  | IF cond_expr THEN expr2 ELSE expr2 { If ($2, Then ($4, Else $6)) }
  | INT  { Int $1 }
  | LPAREN expr1 RPAREN { Paren $2 }
  ;
  

x1:
  | x1 PLUS expr2 { Plus ($1, $3) }
  | expr2  { $1 }
  ;

