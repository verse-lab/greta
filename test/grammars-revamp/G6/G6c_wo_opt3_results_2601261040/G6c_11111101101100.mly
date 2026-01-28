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
%token EOF%type <Ast.t> cons1
%type <Ast.exp> int_8
%start cons1
%%

bool1:
  | bool1 AND bool5  {  And($1, $3)  }
  | int_8 GTE int_8  {  Gte($1, $3)  }
  | int_8 GT int_8  {  Gt($1, $3)  }
  | bool5  {  $1  }
  | bool1 OR bool5  {  Or($1, $3)  }
  | int_8 LT int_8  {  Lt($1, $3)  }
  | int_8 EQ int_8  {  Eq($1, $3)  }
  | int_8 LTE int_8  {  Lte($1, $3)  }
  | int_8 NE int_8  {  Ne($1, $3)  }
  ;

bool10:
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  ;

bool5:
  | bool10 IFF bool7  {  Iff($1, $3)  }
  | BVAR  {  Bvar  }
  | NOT bool5  {  Not($2)  }
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  ;

bool7:
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | bool10 IFF bool7  {  Iff($1, $3)  }
  | BVAR  {  Bvar  }
  ;

cons1:
  | bool1 EOF  {  $1  }
  ;

int_11:
  | int_12 PLUS int_11  {  Plus($1, $3)  }
  | int_2  {  $1  }
  ;

int_12:
  | int_2  {  $1  }
  ;

int_2:
  | IVAR  {  Ivar  }
  | int_9 TIMES int_6  {  Times($1, $3)  }
  | MINUS int_4  {  Negative($2)  }
  | INT  {  Int  }
  | int_3 DIVIDE int_2  {  Divide($1, $3)  }
  | int_3 POWER int_4  {  Power($1, $3)  }
  ;

int_3:
  | int_3 POWER int_4  {  Power($1, $3)  }
  | INT  {  Int  }
  | MINUS int_4  {  Negative($2)  }
  | IVAR  {  Ivar  }
  | int_9 TIMES int_6  {  Times($1, $3)  }
  ;

int_4:
  | int_9 TIMES int_6  {  Times($1, $3)  }
  | MINUS int_4  {  Negative($2)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_6:
  | int_9 TIMES int_6  {  Times($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_8:
  | int_12 PLUS int_11  {  Plus($1, $3)  }
  | int_2  {  $1  }
  | int_11 MINUS int_8  {  Minus($1, $3)  }
  ;

int_9:
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;
