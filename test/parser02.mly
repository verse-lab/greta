// Given 'parser01.mly', below is generated after selecting Option 1
//    Option 0: ( expr1 * ( expr1 + expr1 ) ) 
//    Option 1: ( ( expr1 * expr1 ) + expr1 ) 

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

expr2:
  | expr2 MUL expr2 { Mul ($1, $3) }
  ;

expr1:
  | INT { Int $1 }
  | expr1 PLUS expr1 { Plus ($1, $3) }
  ;

expr4:
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

expr6:
  | expr4 { $1 }
  ;

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

