/* *** G0e *** */
// 7 po's 2 assoc's
// if2 vs. +
// if2 vs. *
// * vs. +
// + vs. *
// if1 vs. +
// if1 vs. * 
// if1 vs. if2
// * assoc
// + assoc

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

cond:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;

expr1:
  | INT  { Int $1 }
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | expr1 MUL expr1 { Mul ($1, $3) }
  | LPAREN expr1 RPAREN { Paren $2 }
  | IF cond THEN expr1 { If ($2, Then ($4, Else Na)) }
  | IF cond THEN expr1 ELSE expr1 { If ($2, Then ($4, Else $6)) }
  ;



