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
%token EOF%type <Ast.t> prog12
%start prog12
%%

decl21:
  | TINT iden17 EQ expr3  {  TDecl ($2, $4)  }
  ;

decl22:
  | TINT iden17 EQ expr3  {  TDecl ($2, $4)  }
  ;

decl23:
  | TINT iden17 EQ expr3  {  TDecl ($2, $4)  }
  ;

decl24:
  | TINT iden17 EQ expr3  {  TDecl ($2, $4)  }
  ;

decl25:
  | TINT iden17 EQ expr3  {  TDecl ($2, $4)  }
  ;

decl26:
  | TINT iden17 EQ expr3  {  TDecl ($2, $4)  }
  ;

decl27:
  | TINT iden17 EQ expr3  {  TDecl ($2, $4)  }
  ;

expr1:
  | expr3 STAR expr8  {  Star ($1, $3)  }
  | LPAREN expr3 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | expr10 PLUS expr8  {  Plus ($1, $3)  }
  ;

expr10:
  | LPAREN expr3 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  ;

expr2:
  | INT  {  Int $1  }
  | expr10 PLUS expr8  {  Plus ($1, $3)  }
  | LPAREN expr3 RPAREN  {  Paren $2  }
  | expr3 STAR expr8  {  Star ($1, $3)  }
  ;

expr3:
  | LPAREN expr3 RPAREN  {  Paren $2  }
  | INT  {  Int $1  }
  | expr3 STAR expr8  {  Star ($1, $3)  }
  | expr10 PLUS expr8  {  Plus ($1, $3)  }
  ;

expr4:
  | INT  {  Int $1  }
  | expr10 PLUS expr8  {  Plus ($1, $3)  }
  | LPAREN expr3 RPAREN  {  Paren $2  }
  | expr3 STAR expr8  {  Star ($1, $3)  }
  ;

expr5:
  | INT  {  Int $1  }
  | expr10 PLUS expr8  {  Plus ($1, $3)  }
  | LPAREN expr3 RPAREN  {  Paren $2  }
  | expr3 STAR expr8  {  Star ($1, $3)  }
  ;

expr8:
  | INT  {  Int $1  }
  | expr10 PLUS expr8  {  Plus ($1, $3)  }
  | LPAREN expr3 RPAREN  {  Paren $2  }
  ;

iden13:
  | IDENT  {  $1  }
  ;

iden14:
  | IDENT  {  $1  }
  ;

iden15:
  | IDENT  {  $1  }
  ;

iden16:
  | IDENT  {  $1  }
  ;

iden17:
  | IDENT  {  $1  }
  ;

iden18:
  | IDENT  {  $1  }
  ;

iden19:
  | IDENT  {  $1  }
  ;

iden20:
  | IDENT  {  $1  }
  ;

prog12:
  | stmt6 EOF  {  $1  }
  ;

stmt11:
  | decl27 SEMI  {  Semi ($1)  }
  ;

stmt6:
  | IF expr5 THEN stmt7 ELSE stmt7  {  If ($2, Then ($4, Else $6))  }
  | decl27 SEMI  {  Semi ($1)  }
  | IF expr1 THEN stmt9  {  If ($2, Then ($4, Else Na))  }
  ;

stmt7:
  | IF expr5 THEN stmt7 ELSE stmt7  {  If ($2, Then ($4, Else $6))  }
  | IF expr1 THEN stmt9  {  If ($2, Then ($4, Else Na))  }
  | decl27 SEMI  {  Semi ($1)  }
  ;

stmt9:
  | IF expr1 THEN stmt9  {  If ($2, Then ($4, Else Na))  }
  | decl27 SEMI  {  Semi ($1)  }
  ;
