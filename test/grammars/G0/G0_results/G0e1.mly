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

e1:
  | x1 PLUS e1 { Plus ($1, $3) }
  | x1 { $1 }
  ;

x4:
  | INT  { Int $1 }
  | LPAREN e1 RPAREN { Paren $2 }
  ;

x3:
  | x4 { $1 }
  | x3 MUL x4 { Mul ($1, $3) }
  ;

x2:
  | x3 { $1 }
  | IF cond THEN x2 ELSE x2 { If ($2, Then ($4, Else $6)) }
  ;

x1:
  | x2 { $1 }
  | IF cond THEN x1 { If ($2, Then ($4, Else Na)) }
  ;

