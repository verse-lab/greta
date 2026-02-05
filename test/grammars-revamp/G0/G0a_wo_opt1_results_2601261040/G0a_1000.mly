%{

  open Ast

%}
%token <int> INT
%token <string> IDENT
%token SEMI
%token IF
%token THEN
%token ELSE
%token TINT
%token EQ
%token PLUS
%token STAR
%token LPAREN
%token RPAREN
%token EOF%type <Ast.t> prog7
%start prog7
%%

decl9:
  | TINT iden8 EQ expr1  {  TDecl ($2, $4)  }
  ;

expr1:
  | expr3  { $1 }
  | expr1 PLUS expr3  {  Plus ($1, $3)  }
  ;

expr3:
  | expr5  { $1 }
  | expr3 STAR expr5  {  Star ($1, $3)  }
  ;

expr5:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  ;

iden8:
  | IDENT  {  $1  }
  ;

prog7:
  | stmt2 EOF  {  $1  }
  ;

stmt2:
  | IF expr1 THEN stmt2 ELSE stmt2  {  If ($2, Then ($4, Else $6))  }
  | stmt4  { $1 }
  ;

stmt4:
  | stmt6  { $1 }
  | IF expr1 THEN stmt4  {  If ($2, Then ($4, Else Na))  }
  ;

stmt6:
  | decl9 SEMI  {  Semi ($1)  }
  ;
