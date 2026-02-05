%{

open Ast

%}
%token IFF
%token AND
%token OR
%token EQ
%token NE
%token GT
%token GTE
%token LT
%token LTE
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token POWER
%token NOT
%token LPAREN
%token RPAREN
%token IVAR
%token BVAR
%token INT
%token EOF%type <Ast.t> cons19
%type <Ast.exp> int_7
%start cons19
%%

bool1:
  | bool2 AND bool10  {  And($1, $3)  }
  | int_7 GTE int_7  {  Gte($1, $3)  }
  | int_7 GT int_7  {  Gt($1, $3)  }
  | bool10  {  $1  }
  | bool2 OR bool10  {  Or($1, $3)  }
  | int_7 LT int_7  {  Lt($1, $3)  }
  | int_7 EQ int_7  {  Eq($1, $3)  }
  | int_7 LTE int_7  {  Lte($1, $3)  }
  | int_7 NE int_7  {  Ne($1, $3)  }
  ;

bool10:
  | bool17 IFF bool8  {  Iff($1, $3)  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | NOT bool17  {  Not($2)  }
  | BVAR  {  Bvar  }
  ;

bool17:
  | NOT bool17  {  Not($2)  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  ;

bool18:
  | BVAR  {  Bvar  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  ;

bool2:
  | int_7 EQ int_7  {  Eq($1, $3)  }
  | int_7 GTE int_7  {  Gte($1, $3)  }
  | bool2 AND bool10  {  And($1, $3)  }
  | bool2 OR bool10  {  Or($1, $3)  }
  | int_7 NE int_7  {  Ne($1, $3)  }
  | int_7 LTE int_7  {  Lte($1, $3)  }
  | bool10  {  $1  }
  | int_7 GT int_7  {  Gt($1, $3)  }
  | int_7 LT int_7  {  Lt($1, $3)  }
  ;

bool8:
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | NOT bool17  {  Not($2)  }
  | BVAR  {  Bvar  }
  | bool17 IFF bool8  {  Iff($1, $3)  }
  ;

bool9:
  | bool17 IFF bool8  {  Iff($1, $3)  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | NOT bool17  {  Not($2)  }
  | BVAR  {  Bvar  }
  ;

cons19:
  | bool1 EOF  {  $1  }
  ;

int_11:
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  | MINUS int_14  {  Negative($2)  }
  ;

int_12:
  | MINUS int_14  {  Negative($2)  }
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;

int_13:
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | MINUS int_14  {  Negative($2)  }
  ;

int_14:
  | IVAR  {  Ivar  }
  | MINUS int_14  {  Negative($2)  }
  | INT  {  Int  }
  ;

int_15:
  | IVAR  {  Ivar  }
  | MINUS int_14  {  Negative($2)  }
  | INT  {  Int  }
  ;

int_16:
  | MINUS int_14  {  Negative($2)  }
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;

int_3:
  | int_3 MINUS int_11  {  Minus($1, $3)  }
  | int_11  {  $1  }
  | int_3 DIVIDE int_11  {  Divide($1, $3)  }
  | int_3 TIMES int_11  {  Times($1, $3)  }
  | int_3 PLUS int_11  {  Plus($1, $3)  }
  | int_3 POWER int_11  {  Power($1, $3)  }
  ;

int_4:
  | int_3 POWER int_11  {  Power($1, $3)  }
  | int_3 MINUS int_11  {  Minus($1, $3)  }
  | int_11  {  $1  }
  | int_3 TIMES int_11  {  Times($1, $3)  }
  | int_3 DIVIDE int_11  {  Divide($1, $3)  }
  | int_3 PLUS int_11  {  Plus($1, $3)  }
  ;

int_5:
  | int_3 DIVIDE int_11  {  Divide($1, $3)  }
  | int_3 TIMES int_11  {  Times($1, $3)  }
  | int_3 PLUS int_11  {  Plus($1, $3)  }
  | int_11  {  $1  }
  | int_3 POWER int_11  {  Power($1, $3)  }
  | int_3 MINUS int_11  {  Minus($1, $3)  }
  ;

int_6:
  | int_3 POWER int_11  {  Power($1, $3)  }
  | int_3 PLUS int_11  {  Plus($1, $3)  }
  | int_3 TIMES int_11  {  Times($1, $3)  }
  | int_3 MINUS int_11  {  Minus($1, $3)  }
  | int_3 DIVIDE int_11  {  Divide($1, $3)  }
  | int_11  {  $1  }
  ;

int_7:
  | int_3 TIMES int_11  {  Times($1, $3)  }
  | int_11  {  $1  }
  | int_3 PLUS int_11  {  Plus($1, $3)  }
  | int_3 MINUS int_11  {  Minus($1, $3)  }
  | int_3 DIVIDE int_11  {  Divide($1, $3)  }
  | int_3 POWER int_11  {  Power($1, $3)  }
  ;
