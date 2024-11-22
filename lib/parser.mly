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

program : expr0 EOF { $1 } ;

expr0:
  | expr1 PLUS expr3 { Plus ($1, $3) }
  | expr2  { $1 }
  ;

expr1:
  | expr1 PLUS expr3 { Plus ($1, $3) }
  | expr3  { $1 }
  ;

expr2:
  | expr3 MUL expr2 { Mul ($1, $3) }
  | expr3  { $1 }
  ;

expr3:
  | IF cond_expr THEN expr3 { If ($2, Then ($4, Else Na)) }
  | IF cond_expr THEN expr3 ELSE expr3 { If ($2, Then ($4, Else $6)) }
  | INT  { Int $1 }
  | LPAREN expr0 RPAREN { Paren $2 }
  ;

cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;


