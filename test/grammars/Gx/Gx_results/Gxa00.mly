// Example discussed on Feb 19 
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
  | e1 EOF { $1 }
  ;

e1: 
  | e1 PLUS x1 { Plus ($1, $3) }
  | x1 { $1 }
  ;

x2:
  | INT { Int $1 }
  | LPAREN e1 RPAREN { Paren ($2) }
  ;

x1:
  | x2 { $1 }
  | x1 STAR x2 { Mul ($1, $3) }
  ;

