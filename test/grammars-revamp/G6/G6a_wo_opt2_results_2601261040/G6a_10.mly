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
%type <Ast.exp> int_4
%start cons1
%%

bool1:
  | bool2  { $1 }
  ;

bool10:
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  ;

bool2:
  | int_4 NE int_4  {  Ne($1, $3)  }
  | int_4 EQ int_4  {  Eq($1, $3)  }
  | int_4 LTE int_4  {  Lte($1, $3)  }
  | int_4 GTE int_4  {  Gte($1, $3)  }
  | bool1 AND bool6  {  And($1, $3)  }
  | bool6  {  $1  }
  | bool1 OR bool6  {  Or($1, $3)  }
  | int_4 GT int_4  {  Gt($1, $3)  }
  | int_4 LT int_4  {  Lt($1, $3)  }
  ;

bool5:
  | bool6  { $1 }
  ;

bool6:
  | NOT bool5  {  Not($2)  }
  | bool8  { $1 }
  ;

bool8:
  | bool10  { $1 }
  | bool8 IFF bool10  {  Iff($1, $3)  }
  ;

cons1:
  | bool2 EOF  {  $1  }
  ;

int_3:
  | int_4  { $1 }
  ;

int_4:
  | int_3 POWER int_9  {  Power($1, $3)  }
  | int_3 MINUS int_9  {  Minus($1, $3)  }
  | int_3 TIMES int_9  {  Times($1, $3)  }
  | int_3 DIVIDE int_9  {  Divide($1, $3)  }
  | int_3 PLUS int_9  {  Plus($1, $3)  }
  | int_9  {  $1  }
  ;

int_7:
  | int_9  { $1 }
  ;

int_9:
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  | MINUS int_7  {  Negative($2)  }
  ;
