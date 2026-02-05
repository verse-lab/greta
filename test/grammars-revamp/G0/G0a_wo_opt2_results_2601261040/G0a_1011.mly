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

decl10:
  | TINT iden9 EQ expr1  {  TDecl ($2, $4)  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr2:
  | expr3  { $1 }
  ;

expr3:
  | expr4  { $1 }
  | expr4 PLUS expr1  {  Plus ($1, $3)  }
  ;

expr4:
  | expr7  { $1 }
  | expr7 STAR expr4  {  Star ($1, $3)  }
  ;

expr7:
  | INT  {  Int $1  }
  | LPAREN expr1 RPAREN  {  Paren $2  }
  ;

iden9:
  | IDENT  {  $1  }
  ;

prog1:
  | stmt6 EOF  {  $1  }
  ;

stmt5:
  | stmt6  { $1 }
  ;

stmt6:
  | stmt8  { $1 }
  | IF expr3 THEN stmt5 ELSE stmt5  {  If ($2, Then ($4, Else $6))  }
  ;

stmt8:
  | IF expr2 THEN stmt8  {  If ($2, Then ($4, Else Na))  }
  | decl10 SEMI  {  Semi ($1)  }
  ;
