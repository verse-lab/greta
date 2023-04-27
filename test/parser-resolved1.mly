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

%left PLUS

%left MUL

%type <Ast.t> program

%start program
%%

program : expr1 EOF { $1 };

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr18:
  | LPAREN expr1 RPAREN { Paren $2 }
  | expr18 MUL expr18 { Mul ($1, $3) }
  | INT { Int $1 }
  ;

expr1:
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | expr18 { $1 }
  | IF cond_expr1 THEN expr18 { If ($2, Then ($4, Else Na)) }
  | IF cond_expr1 THEN expr18 ELSE expr18 { If ($2, Then ($4, Else Na)) }
  ;
