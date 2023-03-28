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

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr14:
  | LPAREN expr1 RPAREN { Paren $2 }
  | expr14 MUL expr14 { Mul ($1, $3) }
  ;

expr12:
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

expr6:
  | LPAREN expr1 RPAREN { Paren $2 }
  | IF cond_expr1 THEN expr6 ELSE expr6 { If ($2, Then ($4, Else Na)) }
  ;

expr4:
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

expr10:
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

expr8:
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

expr1:
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | INT { Int $1 }
  | expr8 { $1 }
  | expr10 { $1 }
  | expr4 { $1 }
  | expr6 { $1 }
  | expr12 { $1 }
  | expr14 { $1 }
  | expr2 { $1 }
  ;

expr2:
  | IF cond_expr1 THEN expr2 { If ($2, Then ($4, Else Na)) }
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

