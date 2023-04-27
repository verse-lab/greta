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

cond_expr: 
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr1: 
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | expr2 { $1 }
  ;

expr2: 
  | expr2 MUL expr2 { Mul ($1, $3) }
  | expr3 { $1 }
  ;

expr3: 
  | IF cond_expr THEN expr3 { If ($2, Then ($4, Else Na)) }
  | expr4 { $1 }
  ;

expr4 : 
  | INT { Int $1 }
  | LPAREN expr1 RPAREN { Paren $2 }
  | IF cond_expr THEN expr4 ELSE expr4 { If ($2, Then ($4, Else $6)) }
  ;