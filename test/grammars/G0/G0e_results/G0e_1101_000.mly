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
  | LPAREN e1 RPAREN { Paren $2 }
  ;

x2:
  | x3 PLUS x2 { Plus ($1, $3) }
  | x3 { $1 }
  | INT  { Int $1 }
  ;

x1:
  | x2 { $1 }
  | IF cond THEN x1 ELSE x1 { If ($2, Then ($4, Else $6)) }
  | IF cond THEN x1 { If ($2, Then ($4, Else Na)) }
  ;

e1:
  | x1 MUL e1 { Mul ($1, $3) }
  | x1 { $1 }
  ;

