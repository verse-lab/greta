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

%type <Ast.t> prog1
%start prog1
%%

cond6:
  | FALSE  { Bool false }
  | TRUE  { Bool true }
  | expr8  { $1 }
  ;

expr1:
  | IF cond6 THEN expr1 ELSE expr1  { If ($2, Then ($4, Else $6)) }
  | expr2  { $1 }
  | IF cond6 THEN expr1  { If ($2, Then ($4, Else Na)) }
  ;

expr2:
  | expr3  { $1 }
  | IF cond6 THEN expr2 ELSE expr2  { If ($2, Then ($4, Else $6)) }
  ;

expr3:
  | expr4  { $1 }
  | IF cond6 THEN expr3  { If ($2, Then ($4, Else Na)) }
  ;

expr4:
  | expr5 MUL expr4  { Mul ($1, $3) }
  | expr5  { $1 }
  ;

expr5:
  | INT  { Int $1 }
  | LPAREN expr7 RPAREN  { Paren $2 }
  | LPAREN expr8 RPAREN  { Paren $2 }
  | expr8  { $1 }
  ;

expr7:
  | expr8  { $1 }
  | expr1  { $1 }
  | expr7 PLUS expr1  { Plus ($1, $3) }
  ;

prog1:
  | expr7 EOF  { $1 }
  | expr8  { $1 }
  ;
