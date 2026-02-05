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
%type <Ast.exp> int_16
%start cons1
%%

bool1:
  | bool2  { $1 }
  ;

bool10:
  | bool14 IFF bool10  {  Iff($1, $3)  }
  | bool14  { $1 }
  ;

bool14:
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  ;

bool2:
  | int_12 NE int_12  {  Ne($1, $3)  }
  | int_12 EQ int_12  {  Eq($1, $3)  }
  | int_12 LTE int_12  {  Lte($1, $3)  }
  | int_12 GTE int_12  {  Gte($1, $3)  }
  | bool1 AND bool8  {  And($1, $3)  }
  | bool8  {  $1  }
  | bool1 OR bool8  {  Or($1, $3)  }
  | int_12 GT int_12  {  Gt($1, $3)  }
  | int_12 LT int_12  {  Lt($1, $3)  }
  ;

bool7:
  | bool8  { $1 }
  ;

bool8:
  | bool10  { $1 }
  | NOT bool7  {  Not($2)  }
  ;

cons1:
  | bool2 EOF  {  $1  }
  ;

int_11:
  | int_12  { $1 }
  ;

int_12:
  | int_15 MINUS int_11  {  Minus($1, $3)  }
  | int_15  { $1 }
  ;

int_13:
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;

int_15:
  | int_16  { $1 }
  | int_16 PLUS int_15  {  Plus($1, $3)  }
  ;

int_16:
  | int_4  {  $1  }
  ;

int_3:
  | int_4  { $1 }
  ;

int_4:
  | int_5  { $1 }
  | MINUS int_3  {  Negative($2)  }
  ;

int_5:
  | int_5 TIMES int_6  {  Times($1, $3)  }
  | int_6  { $1 }
  ;

int_6:
  | int_6 POWER int_9  {  Power($1, $3)  }
  | int_9  { $1 }
  ;

int_9:
  | int_13 DIVIDE int_9  {  Divide($1, $3)  }
  | int_13  { $1 }
  ;
