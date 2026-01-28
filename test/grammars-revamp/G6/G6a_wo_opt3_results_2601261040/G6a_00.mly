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
%type <Ast.exp> int_2
%start cons1
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
  | bool3 IFF bool4  {  Iff($1, $3)  }
  | BVAR  {  Bvar  }
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | NOT bool4  {  Not($2)  }
  ;

bool4:
  | BVAR  {  Bvar  }
  | LPAREN bool1 RPAREN  {  Bparen($2)  }
  | NOT bool4  {  Not($2)  }
  ;

cons1:
  | bool1 EOF  {  $1  }
  ;

int_2:
  | int_2 DIVIDE int_5  {  Divide($1, $3)  }
  | int_2 PLUS int_5  {  Plus($1, $3)  }
  | int_5  {  $1  }
  | int_2 TIMES int_5  {  Times($1, $3)  }
  | int_2 POWER int_5  {  Power($1, $3)  }
  | int_2 MINUS int_5  {  Minus($1, $3)  }
  ;

int_5:
  | INT  {  Int  }
  | MINUS int_5  {  Negative($2)  }
  | IVAR  {  Ivar  }
  ;
