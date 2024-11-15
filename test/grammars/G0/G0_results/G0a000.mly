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
  

x3:
  | IF cond_expr THEN x3 { If ($2, Then ($4, Else Na)) }
  | IF cond_expr THEN x3 ELSE x3 { If ($2, Then ($4, Else $6)) }
  | INT  { Int $1 }
  

x2:
  | x3 MUL x2 { Mul ($1, $3) }
  | x3  { $1 }
  

x1:
  | x3  { $1 }
  | x3  { $1 }
  | x1 PLUS x3 { Plus ($1, $3) }
  

e1:
  | x2  { $1 }
  | x1  { $1 }
  

