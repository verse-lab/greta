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


%type <Ast.t> program

%start program
%%

program : stmt EOF { $1 };

stmt:
  | decl SEMI { Semi ($1) }
  | IF expr1 THEN stmt { If ($2, Then ($4, Else Na)) }
  | IF expr1 THEN stmt ELSE stmt { If ($2, Then ($4, Else $6)) }
  ;

decl:
  | TINT ident EQ expr1 { TDecl ($2, $4) }
  ;

ident:
  | IDENT { $1 }

expr1:
  | expr1 PLUS expr2 { Plus ($1, $3) }
  | expr2 { $1 }

expr2: 
  | expr2 STAR expr3 { Star ($1, $3) }
  | expr3 { $1 }

expr3:
  | expr3 POW expr3 { Pow ($1, $3) }
  | INT  { Int $1 }
  | LPAREN expr1 RPAREN { Paren $2 }
  ;
