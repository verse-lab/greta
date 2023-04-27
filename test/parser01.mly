// Given 'parser00.mly', below is generated after selecting Option 1
//    Option 0: ( IF cond_expr THEN expr ELSE ( expr + expr ) ) 
//    Option 1: ( ( IF cond_expr THEN expr ELSE expr ) + expr ) 

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

expr1:
  | expr1 MUL expr1 { Mul ($1, $3) }
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | IF cond_expr1 THEN expr1 { If ($2, Then ($4, Else Na)) }
  | INT { Int $1 }
  | expr2 { $1 }
  ;

expr2:
  | IF cond_expr1 THEN expr2 ELSE expr2 { If ($2, Then ($4, Else Na)) }
  | LPAREN expr1 RPAREN { Paren $2 }
  ;
