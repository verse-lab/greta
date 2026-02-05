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
%token EOF%type <Ast.t> cons7
%type <Ast.exp> int_2
%start cons7
%%

bool1:
  | bool1 AND bool3  {  And($1, $3)  }
  | int_2 GTE int_2  {  Gte($1, $3)  }
  | int_2 GT int_2  {  Gt($1, $3)  }
  | bool3  {  $1  }
  | bool1 OR bool3  {  Or($1, $3)  }
  | int_2 LT int_2  {  Lt($1, $3)  }
  | int_2 EQ int_2  {  Eq($1, $3)  }
  | int_2 LTE int_2  {  Lte($1, $3)  }
  | int_2 NE int_2  {  Ne($1, $3)  }
  ;

bool3:
  | bool5  { $1 }
  | NOT bool3  {  Not($2)  }
  ;

bool5:
  | bool5 IFF bool6  {  Iff($1, $3)  }
  | bool6  { $1 }
  ;

bool6:
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | BVAR  {  Bvar  }
  ;

cons7:
  | bool1 EOF  {  $1  }
  ;

int_2:
  | int_2 DIVIDE int_4  {  Divide($1, $3)  }
  | int_2 PLUS int_4  {  Plus($1, $3)  }
  | int_4  {  $1  }
  | int_2 TIMES int_4  {  Times($1, $3)  }
  | int_2 POWER int_4  {  Power($1, $3)  }
  | int_2 MINUS int_4  {  Minus($1, $3)  }
  ;

int_4:
  | MINUS int_4  {  Negative($2)  }
  | INT  {  Int  }
  | IVAR  {  Ivar  }
  ;
