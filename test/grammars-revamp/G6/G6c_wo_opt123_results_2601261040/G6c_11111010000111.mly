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
%token EOF%type <Ast.t> cons23
%type <Ast.exp> int_22
%start cons23
%%

bool1:
  | bool2 AND bool13  {  And($1, $3)  }
  | int_17 GTE int_17  {  Gte($1, $3)  }
  | int_17 GT int_17  {  Gt($1, $3)  }
  | bool13  {  $1  }
  | bool2 OR bool13  {  Or($1, $3)  }
  | int_17 LT int_17  {  Lt($1, $3)  }
  | int_17 EQ int_17  {  Eq($1, $3)  }
  | int_17 LTE int_17  {  Lte($1, $3)  }
  | int_17 NE int_17  {  Ne($1, $3)  }
  ;

bool11:
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  | bool21 IFF bool18  {  Iff($1, $3)  }
  | NOT bool11  {  Not($2)  }
  ;

bool12:
  | BVAR  {  Bvar  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | NOT bool11  {  Not($2)  }
  | bool21 IFF bool18  {  Iff($1, $3)  }
  ;

bool13:
  | bool21 IFF bool18  {  Iff($1, $3)  }
  | BVAR  {  Bvar  }
  | NOT bool11  {  Not($2)  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  ;

bool18:
  | BVAR  {  Bvar  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | bool21 IFF bool18  {  Iff($1, $3)  }
  ;

bool2:
  | int_17 EQ int_17  {  Eq($1, $3)  }
  | int_17 GTE int_17  {  Gte($1, $3)  }
  | bool2 AND bool13  {  And($1, $3)  }
  | bool2 OR bool13  {  Or($1, $3)  }
  | int_17 NE int_17  {  Ne($1, $3)  }
  | int_17 LTE int_17  {  Lte($1, $3)  }
  | bool13  {  $1  }
  | int_17 GT int_17  {  Gt($1, $3)  }
  | int_17 LT int_17  {  Lt($1, $3)  }
  ;

bool21:
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  ;

cons23:
  | bool1 EOF  {  $1  }
  ;

int_10:
  | int_10 POWER int_14  {  Power($1, $3)  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_14:
  | INT  {  Int  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  | IVAR  {  Ivar  }
  ;

int_15:
  | int_3  {  $1  }
  | int_20 MINUS int_15  {  Minus($1, $3)  }
  | int_22 PLUS int_20  {  Plus($1, $3)  }
  ;

int_16:
  | int_22 PLUS int_20  {  Plus($1, $3)  }
  | int_20 MINUS int_15  {  Minus($1, $3)  }
  | int_3  {  $1  }
  ;

int_17:
  | int_22 PLUS int_20  {  Plus($1, $3)  }
  | int_3  {  $1  }
  | int_20 MINUS int_15  {  Minus($1, $3)  }
  ;

int_19:
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_20:
  | int_3  {  $1  }
  | int_22 PLUS int_20  {  Plus($1, $3)  }
  ;

int_22:
  | int_3  {  $1  }
  ;

int_3:
  | int_10 POWER int_14  {  Power($1, $3)  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  | int_9 TIMES int_10  {  Times($1, $3)  }
  | INT  {  Int  }
  | MINUS int_6  {  Negative($2)  }
  | IVAR  {  Ivar  }
  ;

int_4:
  | int_9 TIMES int_10  {  Times($1, $3)  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  | int_10 POWER int_14  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | MINUS int_6  {  Negative($2)  }
  ;

int_5:
  | int_9 TIMES int_10  {  Times($1, $3)  }
  | MINUS int_6  {  Negative($2)  }
  | int_10 POWER int_14  {  Power($1, $3)  }
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  ;

int_6:
  | int_10 POWER int_14  {  Power($1, $3)  }
  | int_9 TIMES int_10  {  Times($1, $3)  }
  | INT  {  Int  }
  | MINUS int_6  {  Negative($2)  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  | IVAR  {  Ivar  }
  ;

int_7:
  | int_10 POWER int_14  {  Power($1, $3)  }
  | MINUS int_6  {  Negative($2)  }
  | int_9 TIMES int_10  {  Times($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  ;

int_8:
  | int_9 TIMES int_10  {  Times($1, $3)  }
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  | int_10 POWER int_14  {  Power($1, $3)  }
  | MINUS int_6  {  Negative($2)  }
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;

int_9:
  | int_14 DIVIDE int_19  {  Divide($1, $3)  }
  | int_10 POWER int_14  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | int_9 TIMES int_10  {  Times($1, $3)  }
  ;
