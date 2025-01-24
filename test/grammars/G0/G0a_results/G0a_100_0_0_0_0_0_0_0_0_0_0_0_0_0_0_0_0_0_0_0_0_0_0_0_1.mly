/* *** G0a *** */
// 3 po's 1 assoc
// if2 vs. *
// * assoc
// if1 vs. *
/* ---------- */
// if1 vs. if2

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

cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;

e1:
  | e1 PLUS x1 { Plus ($1, $3) }
  | x1  { $1 }
  ;

x4:
  | INT  { Int $1 }
  | LPAREN e1 RPAREN { Paren $2 }
  ;

x3:
  | x4  { $1 }
  | IF cond_expr THEN x3 { If ($2, Then ($4, Else Na)) }
  ;

x1:
  | x4 MUL x1 { Mul ($1, $3) }
  | x3  { $1 }
  | IF cond_expr THEN x1 ELSE x1 { If ($2, Then ($4, Else $6)) }
  ;

