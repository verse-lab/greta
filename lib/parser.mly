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
  | IF expr THEN stmt { If ($2, Then ($4, Else Na)) }
  | IF expr THEN stmt ELSE stmt { If ($2, Then ($4, Else $6)) }
  ;

decl:
  | TINT ident EQ expr { TDecl ($2, $4) }
  ;

ident:
  | IDENT { $1 }

expr:
  | expr PLUS expr { Plus ($1, $3) }
  | expr STAR expr { Star ($1, $3) }
  | expr POW expr { Pow ($1, $3) }
  | INT  { Int $1 }
  | LPAREN expr RPAREN { Paren $2 }
  ;
