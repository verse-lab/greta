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

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr6:
  | LPAREN expr1 RPAREN { Paren $2 }
  | IF cond_expr1 THEN expr6 ELSE expr6 { If ($2, Then ($4, Else Na)) }
  ;

expr4:
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

expr1:
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | IF cond_expr1 THEN expr1 { If ($2, Then ($4, Else Na)) }
  | INT { Int $1 }
  | expr4 { $1 }
  | expr6 { $1 }
  | expr2 { $1 }
  ;

expr2:
  | expr2 MUL expr2 { Mul ($1, $3) }
  | LPAREN expr1 RPAREN { Paren $2 }
  ;

