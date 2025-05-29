%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token IF
%token THEN
%token ELSE
%token PLUS
%token MUL
%token LPAREN
%token RPAREN
%token TINT
%token EQ 

%token EOF

%type <Ast.t> toplevel

%start toplevel
%%

toplevel : stmt EOF { $1 };

stmt:
  | decl SEMI  { Semi ($1) }
  | IF exp THEN stmt { If ($2, Then ($4, Else Na)) }
  | IF exp THEN stmt ELSE stmt { If ($2, Then ($4, Else $6)) }
  ;

exp:
  | exp PLUS exp { Plus ($1, $3) }
  | exp MUL exp { Mul ($1, $3) }
  | LPAREN exp RPAREN { Paren $2 }
  | ident { $1 }
  | INT  { Int $1 }
  ;
  
decl:
  | TINT ident EQ exp { Decl ($2, $4) }
  ; 

ident:
  | IDENT { Ident $1 }
  ;

