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

x1:
  | x2  { $1 }
  | IF cond_expr THEN x1 ELSE x1 { If ($2, Then ($4, Else $6)) }
  ;

e1:
  | x1  { $1 }
  | e1 PLUS x2 { Plus ($1, $3) }
  ;

x5:
  | INT  { Int $1 }
  | LPAREN x5 RPAREN { Paren $2 }
  ;

x4:
  | x5  { $1 }
  | IF cond_expr THEN x4 { If ($2, Then ($4, Else Na)) }
  ;

x2:
  | x5 MUL x2 { Mul ($1, $3) }
  | x4  { $1 }
  ;

