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
%token EOF%type <Ast.t> cons13
%type <Ast.exp> int_7
%start cons13
%%

bool1:
  | bool1 AND bool5  {  And($1, $3)  }
  | int_7 GTE int_7  {  Gte($1, $3)  }
  | int_7 GT int_7  {  Gt($1, $3)  }
  | bool5  {  $1  }
  | bool1 OR bool5  {  Or($1, $3)  }
  | int_7 LT int_7  {  Lt($1, $3)  }
  | int_7 EQ int_7  {  Eq($1, $3)  }
  | int_7 LTE int_7  {  Lte($1, $3)  }
  | int_7 NE int_7  {  Ne($1, $3)  }
  ;

bool11:
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  ;

bool5:
  | NOT bool5  {  Not($2)  }
  | bool8  { $1 }
  ;

bool8:
  | bool11  { $1 }
  | bool11 IFF bool8  {  Iff($1, $3)  }
  ;

cons13:
  | bool1 EOF  {  $1  }
  ;

int_10:
  | int_12 PLUS int_10  {  Plus($1, $3)  }
  | int_12  { $1 }
  ;

int_12:
  | int_2  {  $1  }
  ;

int_2:
  | int_3  { $1 }
  | int_3 TIMES int_2  {  Times($1, $3)  }
  ;

int_3:
  | int_3 POWER int_4  {  Power($1, $3)  }
  | int_4  { $1 }
  ;

int_4:
  | int_6  { $1 }
  | MINUS int_4  {  Negative($2)  }
  ;

int_6:
  | int_9  { $1 }
  | int_6 DIVIDE int_9  {  Divide($1, $3)  }
  ;

int_7:
  | int_10  { $1 }
  | int_10 MINUS int_7  {  Minus($1, $3)  }
  ;

int_9:
  | IVAR  {  Ivar  }
  | INT  {  Int  }
  ;
