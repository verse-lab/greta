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

program : e1 EOF { $1 };

cond:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;

x3:
  | IF cond THEN x3 { If ($2, Then ($4, Else Na)) }
  | INT  { Int $1 }
  | LPAREN e1 RPAREN { Paren $2 }
  ;

x2:
  | x3 PLUS x2 { Plus ($1, $3) }
  | x3 { $1 }
  ;

x1:
  | x2 { $1 }
  | x1 MUL x2 { Mul ($1, $3) }
  ;

e1:
  | x1 { $1 }
  | IF cond THEN e1 ELSE e1 { If ($2, Then ($4, Else $6)) }
  ;

