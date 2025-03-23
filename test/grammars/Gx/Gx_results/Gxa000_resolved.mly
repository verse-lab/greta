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



%type <Ast.exp_t> expr1
%type <Ast.prog> toplevel
%start toplevel
%%

toplevel:
  | e1 EOF { $1 };

ident:
  | IDENT { $1 }
  ;

decl:
  | TINT ident EQ expr1 { Assign ($2, $4)  }
  ;

expr5:
  | INT { CInt $1 }
  | LPAREN expr1 RPAREN { Paren ($2) }
  ;

stmt2:
  | decl SEMI { Decl ($1) }
  | IF expr1 THEN stmt2 ELSE stmt2 { If3 ($2, Then ($4), Else ($6)) }
  ;

expr2:
  | expr5 STAR expr2 { Mul ($1, $3) }
  | expr5 { $1 }
  ;

expr1:
  | expr2 { $1 }
  | expr1 PLUS expr2 { Plus ($1, $3) }
  ;

e1:
  | stmt2 { $1 }
  | IF expr1 THEN e1 { If2 ($2, Then ($4)) }
  ;

