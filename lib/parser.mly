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

program : expr EOF { $1 };

cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } ;

expr:
  | INT  { Int $1 }
  | expr PLUS expr { Plus ($1, $3) }
  | expr MUL expr { Mul ($1, $3) }
  | LPAREN expr RPAREN { Paren $2 }
  | IF cond_expr THEN expr { If ($2, Then ($4, Else Na)) }
  | IF cond_expr THEN expr ELSE expr { If ($2, Then ($4, Else $6)) }
  ;
