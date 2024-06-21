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

program : expr1 EOF { $1 };

expr1:
  | expr1 PLUS expr2 { Plus ($1, $3) }
  | expr2 { $1 }
  ;

expr2:
  | expr2 MUL expr3 { Mul ($1, $3) }
  | expr3 { $1 }
  ;

expr3:
  | IF cond_expr1 THEN expr3 { If ($2, Then ($4, Else Na)) }
  | expr4 { $1 }
  ;

expr4:
  | IF cond_expr1 THEN expr4 ELSE expr4 { If ($2, Then ($4, Else Na)) }
  | LPAREN expr1 RPAREN { Paren $2 }
  | INT { Int $1 }
  ;

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

