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
%token EOF%type <Ast.t> prog1
%start prog1
%%

decl7:
  | TINT iden6 EQ expr1  {  TDecl ($2, $4)  }
  ;

expr1:
  | expr1 STAR expr2  {  Star ($1, $3)  }
  | expr2  { $1 }
  ;

expr2:
  | expr4  { $1 }
  | expr4 PLUS expr2  {  Plus ($1, $3)  }
  ;

expr4:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  ;

iden6:
  | IDENT  {  $1  }
  ;

prog1:
  | stmt3 EOF  {  $1  }
  ;

stmt3:
  | stmt5  { $1 }
  | IF expr1 THEN stmt3  {  If ($2, Then ($4, Else Na))  }
  ;

stmt5:
  | decl7 SEMI  {  Semi ($1)  }
  | IF expr1 THEN stmt5 ELSE stmt5  {  If ($2, Then ($4, Else $6))  }
  ;
