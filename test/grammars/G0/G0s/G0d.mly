/* *** G0d *** */
// 6 po's 1 assoc
// if2 vs. +
// if2 vs. *
// * vs. +
// if1 vs. +
// if1 vs. *
// if1 vs. if2
// * assoc

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

cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;

expr1:
  | expr1 PLUS expr2 { Plus ($1, $3) }
  | expr1 MUL expr1 { Mul ($1, $3) }
  | IF cond_expr THEN expr1 { If ($2, Then ($4, Else Na)) }
  | IF cond_expr THEN expr1 ELSE expr1 { If ($2, Then ($4, Else $6)) }
  | expr2  { $1 }
  ;

expr2:
  | INT  { Int $1 }
  | LPAREN expr1 RPAREN { Paren $2 }
  ;
