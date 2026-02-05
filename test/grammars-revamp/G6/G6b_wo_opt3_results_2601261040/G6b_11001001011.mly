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

int_2:
  | IVAR  {  Ivar  }
  | int_3 TIMES int_2  {  Times($1, $3)  }
  | MINUS int_3  {  Negative($2)  }
  | INT  {  Int  }
  | int_6 DIVIDE int_4  {  Divide($1, $3)  }
  | int_6 POWER int_9  {  Power($1, $3)  }
  ;

int_3:
  | int_6 POWER int_9  {  Power($1, $3)  }
  | INT  {  Int  }
  | MINUS int_3  {  Negative($2)  }
  | int_6 DIVIDE int_4  {  Divide($1, $3)  }
  | IVAR  {  Ivar  }
  ;

int_4:
  | int_6 DIVIDE int_4  {  Divide($1, $3)  }
  | int_6 POWER int_9  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_6:
  | int_6 POWER int_9  {  Power($1, $3)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;

int_8:
  | int_8 PLUS int_2  {  Plus($1, $3)  }
  | int_2  {  $1  }
  | int_8 MINUS int_2  {  Minus($1, $3)  }
  ;

int_9:
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;
