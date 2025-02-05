%{
    open Ast
%}

%token <string> IDENT
%token <int64> INT
%token SEMI
%token IF
%token THEN
%token ELSE
%token PLUS
%token STAR
%token TINT
%token EQ
%token LPAREN
%token RPAREN

%token EOF

%type <Ast.exp_t> expr
%type <Ast.prog> toplevel

%start toplevel
%% 

toplevel:
  | stmt EOF { $1 };

stmt:
  | decl SEMI { Decl ($1) }
  | IF expr THEN stmt { If2 ($2, Then ($4)) }
  | IF expr THEN stmt ELSE stmt { If3 ($2, Then ($4), Else ($6)) }
  ;

expr:
  | expr PLUS expr { Plus ($1, $3) }
  | expr STAR expr { Mul ($1, $3) }
  | LPAREN expr RPAREN { Paren ($2) }
  | ident { Ident $1 }
  | INT { CInt $1 }
  ;

decl:
  | TINT ident EQ expr { Assign ($2, $4)  }
  ;

ident:
  | IDENT { $1 }
  ;
