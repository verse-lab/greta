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
%type <Ast.exp> int_19
%start cons23
%%

bool1:
  | bool2 AND bool13  {  And($1, $3)  }
  | int_19 GTE int_19  {  Gte($1, $3)  }
  | int_19 GT int_19  {  Gt($1, $3)  }
  | bool13  {  $1  }
  | bool2 OR bool13  {  Or($1, $3)  }
  | int_19 LT int_19  {  Lt($1, $3)  }
  | int_19 EQ int_19  {  Eq($1, $3)  }
  | int_19 LTE int_19  {  Lte($1, $3)  }
  | int_19 NE int_19  {  Ne($1, $3)  }
  ;

bool11:
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  | bool22 IFF bool20  {  Iff($1, $3)  }
  | NOT bool11  {  Not($2)  }
  ;

bool12:
  | BVAR  {  Bvar  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | NOT bool11  {  Not($2)  }
  | bool22 IFF bool20  {  Iff($1, $3)  }
  ;

bool13:
  | bool22 IFF bool20  {  Iff($1, $3)  }
  | BVAR  {  Bvar  }
  | NOT bool11  {  Not($2)  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  ;

bool2:
  | int_19 EQ int_19  {  Eq($1, $3)  }
  | int_19 GTE int_19  {  Gte($1, $3)  }
  | bool2 AND bool13  {  And($1, $3)  }
  | bool2 OR bool13  {  Or($1, $3)  }
  | int_19 NE int_19  {  Ne($1, $3)  }
  | int_19 LTE int_19  {  Lte($1, $3)  }
  | bool13  {  $1  }
  | int_19 GT int_19  {  Gt($1, $3)  }
  | int_19 LT int_19  {  Lt($1, $3)  }
  ;

bool20:
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  | bool22 IFF bool20  {  Iff($1, $3)  }
  | BVAR  {  Bvar  }
  ;

bool22:
  | BVAR  {  Bvar  }
  | LPAREN bool2 RPAREN  {  Bparen($2)  }
  ;

cons23:
  | bool1 EOF  {  $1  }
  ;

int_10:
  | int_10 TIMES int_14  {  Times($1, $3)  }
  | int_14 POWER int_21  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_14:
  | int_14 POWER int_21  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_15:
  | int_3  {  $1  }
  | int_15 MINUS int_3  {  Minus($1, $3)  }
  | int_15 PLUS int_3  {  Plus($1, $3)  }
  ;

int_16:
  | int_15 PLUS int_3  {  Plus($1, $3)  }
  | int_15 MINUS int_3  {  Minus($1, $3)  }
  | int_3  {  $1  }
  ;

int_17:
  | int_15 PLUS int_3  {  Plus($1, $3)  }
  | int_3  {  $1  }
  | int_15 MINUS int_3  {  Minus($1, $3)  }
  ;

int_18:
  | int_15 PLUS int_3  {  Plus($1, $3)  }
  | int_3  {  $1  }
  | int_15 MINUS int_3  {  Minus($1, $3)  }
  ;

int_19:
  | int_15 PLUS int_3  {  Plus($1, $3)  }
  | int_15 MINUS int_3  {  Minus($1, $3)  }
  | int_3  {  $1  }
  ;

int_21:
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;

int_3:
  | int_14 POWER int_21  {  Power($1, $3)  }
  | int_6 DIVIDE int_9  {  Divide($1, $3)  }
  | int_10 TIMES int_14  {  Times($1, $3)  }
  | INT  {  Int  }
  | MINUS int_9  {  Negative($2)  }
  | IVAR  {  Ivar  }
  ;

int_4:
  | int_10 TIMES int_14  {  Times($1, $3)  }
  | int_6 DIVIDE int_9  {  Divide($1, $3)  }
  | int_14 POWER int_21  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | MINUS int_9  {  Negative($2)  }
  ;

int_5:
  | int_10 TIMES int_14  {  Times($1, $3)  }
  | MINUS int_9  {  Negative($2)  }
  | int_14 POWER int_21  {  Power($1, $3)  }
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  | int_6 DIVIDE int_9  {  Divide($1, $3)  }
  ;

int_6:
  | int_14 POWER int_21  {  Power($1, $3)  }
  | int_10 TIMES int_14  {  Times($1, $3)  }
  | INT  {  Int  }
  | MINUS int_9  {  Negative($2)  }
  | int_6 DIVIDE int_9  {  Divide($1, $3)  }
  | IVAR  {  Ivar  }
  ;

int_7:
  | int_14 POWER int_21  {  Power($1, $3)  }
  | MINUS int_9  {  Negative($2)  }
  | int_10 TIMES int_14  {  Times($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | int_6 DIVIDE int_9  {  Divide($1, $3)  }
  ;

int_8:
  | int_10 TIMES int_14  {  Times($1, $3)  }
  | int_6 DIVIDE int_9  {  Divide($1, $3)  }
  | int_14 POWER int_21  {  Power($1, $3)  }
  | MINUS int_9  {  Negative($2)  }
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;

int_9:
  | MINUS int_9  {  Negative($2)  }
  | int_14 POWER int_21  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | int_10 TIMES int_14  {  Times($1, $3)  }
  ;
