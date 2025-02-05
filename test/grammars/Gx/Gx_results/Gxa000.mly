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



%type <Ast.exp_t> x1
%type <Ast.prog> toplevel
%start toplevel
%%

toplevel:
  | e1 EOF { $1 };

ident:
  | IDENT { $1 }
  ;

x4:
  | TINT ident EQ x2 { Assign ($2, $4)  }
  ;

x5:
  | LPAREN x1 RPAREN { Paren ($2) }
  ;

x3:
  | x4 SEMI { Decl ($1) }
  | IF x1 THEN x3 ELSE x3 { If3 ($2, Then ($4), Else ($6)) }
  ;

x2:
  | x5 STAR x2 { Mul ($1, $3) }
  | x5 { $1 }
  | INT { CInt $1 }
  ;

x1:
  | x2 { $1 }
  | x1 PLUS x1 { Plus ($1, $3) }
  ;

e1:
  | x3 { $1 }
  | IF x1 THEN e1 { If2 ($2, Then ($4)) }
  ;

