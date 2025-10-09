// This one is exactly like the running example in paper
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
%token POW

%token LPAREN
%token RPAREN

%token EOF


%type <Ast.t> prog1
%start prog1
%%

decl4:
  | TINT iden8 EQ expr6  { TDecl ($2, $4) }
  | TINT iden8 EQ expr3  { TDecl ($2, $4) }
  | TINT iden8 EQ expr2  { TDecl ($2, $4) }
  | TINT iden8 EQ expr1  { TDecl ($2, $4) }
  ;

expr1:
  | LPAREN expr1 RPAREN  { Paren $2 }
  | expr1 PLUS expr2  { Plus ($1, $3) }
  | expr2  { $1 }
  ;

expr2:
  | expr3  { $1 }
  | LPAREN expr2 RPAREN  { Paren $2 }
  | expr2 STAR expr3  { Star ($1, $3) }
  ;

expr3:
  | LPAREN expr3 RPAREN  { Paren $2 }
  | expr6  { $1 }
  | expr3 POW expr6  { Pow ($1, $3) }
  ;

expr6:
  | INT  { Int $1 }
  | LPAREN expr6 RPAREN  { Paren $2 }
  ;

iden8:
  | IDENT  { $1 }
  ;

prog1:
  | stmt5 EOF  { $1 }
  ;

stmt5:
  | IF expr1 THEN stmt5  { If ($2, Then ($4, Else Na)) }
  | stmt7  { $1 }
  ;

stmt7:
  | decl4 SEMI  { Semi ($1) }
  | IF expr1 THEN stmt7 ELSE stmt7  { If ($2, Then ($4, Else $6)) }
  ;
