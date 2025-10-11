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

%token EOF

%type <Ast.t> cons1
%type <Ast.exp> int_9
%start cons1
%%

bool1:
  | bool1 AND bool5  { And($1, $3) }
  | int_8 GTE int_8  { Gte($1, $3) }
  | int_8 GT int_8  { Gt($1, $3) }
  | bool5  { $1 }
  | bool1 OR bool5  { Or($1, $3) }
  | int_8 LT int_8  { Lt($1, $3) }
  | int_8 EQ int_8  { Eq($1, $3) }
  | int_8 LTE int_8  { Lte($1, $3) }
  | int_8 NE int_8  { Ne($1, $3) }
  ;

bool5:
  | bool5 IFF bool7  { Iff($1, $3) }
  | bool7  { $1 }
  ;

bool7:
  | LPAREN bool1 RPAREN  { Bparen($2) }
  | BVAR  { Bvar }
  | NOT bool7  { Not($2) }
  ;

cons1:
  | bool1 EOF  { $1 }
  ;

int_10:
  | int_2  { $1 }
  ;

int_2:
  | int_3  { $1 }
  | int_2 TIMES int_3  { Times($1, $3) }
  ;

int_3:
  | int_4 DIVIDE int_3  { Divide($1, $3) }
  | int_4  { $1 }
  ;

int_4:
  | int_6  { $1 }
  | int_4 POWER int_6  { Power($1, $3) }
  ;

int_6:
  | MINUS int_6  { Negative($2) }
  | INT  { Int }
  | IVAR  { Ivar }
  ;

int_8:
  | int_8 PLUS int_9  { Plus($1, $3) }
  | int_9  { $1 }
  ;

int_9:
  | int_10  { $1 }
  | int_9 MINUS int_10  { Minus($1, $3) }
  ;
