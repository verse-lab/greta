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

program : x2 EOF { $1 };

cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;

x5:
  | LPAREN x1 RPAREN { Paren $2 }
  ;

x4:
  | x5  { $1 }
  | x4 PLUS x5 { Plus ($1, $3) }
  ;

x3:
  | x5  { $1 }
  | INT  { Int $1 }
  ;

x2:
  | x3  { $1 }
  | IF cond_expr THEN x2 ELSE x2 { If ($2, Then ($4, Else $6)) }
  | IF cond_expr THEN x2 { If ($2, Then ($4, Else Na)) }
  ;

x1:
  | x4  { $1 }
  | e1 MUL x3 { Mul ($1, $3) }
  | INT  { Int $1 }
  | IF cond_expr THEN x2 ELSE x2 { If ($2, Then ($4, Else $6)) }
  | IF cond_expr THEN x2 { If ($2, Then ($4, Else Na)) }
  ;

e1:
  | x2  { $1 }
  | e1 MUL x3 { Mul ($1, $3) }
  ;

