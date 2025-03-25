%{
    open Ast
%}

%token <int64> INT
%token PLUS
%token STAR
%token LPAREN
%token RPAREN
%token EOF
%type <Ast.t> toplevel

%start toplevel
%%

toplevel:
  | expr EOF { $1 }
  ;

expr: 
  | expr PLUS expr { Plus ($1, $3) }
  | expr STAR expr { Mul ($1, $3) }
  | LPAREN expr RPAREN { Paren ($2) }
  | INT { Int $1 }
  ;