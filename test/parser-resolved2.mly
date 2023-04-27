// The grammar below is presumably what the user would have wanted.
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

expr1:
  | IF cond_expr1 THEN expr1 { If ($2, Then ($4, Else Na)) }
  | expr2 { $1 }
  ;

expr2: 
  | IF cond_expr1 THEN expr2 ELSE expr2 { If ($2, Then ($4, Else Na)) }
  | expr3 { $1 }
  ;

expr3:
  | INT { Int $1 }
  | expr3 MUL expr3 { Mul ($1, $3) }
  | expr3 PLUS expr3 { Plus ($1, $3) }
  | LPAREN expr1 RPAREN { Paren $2 }
  ;