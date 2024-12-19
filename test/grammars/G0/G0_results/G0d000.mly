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



%type <Ast.t> program
%start program
%%

program : e1 EOF { $1 };

cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;

x2:
  | INT  { Int $1 }
  | LPAREN e1 RPAREN { Paren $2 }
  ;

x1:
  | x2  { $1 }
  | x1 MUL x1 { Mul ($1, $3) }
  | IF cond_expr THEN x1 ELSE x1 { If ($2, Then ($4, Else $6)) }
  | IF cond_expr THEN x1 { If ($2, Then ($4, Else Na)) }
  ;

e1:
  | x1  { $1 }
  | e1 PLUS x2 { Plus ($1, $3) }
  ;

