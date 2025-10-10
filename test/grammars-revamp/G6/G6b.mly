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

%type <Ast.t> constr
%type <Ast.exp> int_expr1

%start constr
%%

constr: 
  | bool_expr EOF { $1 }
  ;

bool_expr:
  | int_expr1 LT int_expr1 { Lt($1, $3) }
  | int_expr1 GT int_expr1 { Gt($1, $3) }
  | int_expr1 EQ int_expr1 { Eq($1, $3) }
  | int_expr1 NE int_expr1 { Ne($1, $3) }
  | int_expr1 GTE int_expr1 { Gte($1, $3) }
  | int_expr1 LTE int_expr1 { Lte($1, $3) }
  | bool_expr AND bool_expr2 { And($1, $3) }
  | bool_expr OR bool_expr2 { Or($1, $3) }
  | bool_expr2 { $1 }
  ;

bool_expr2:
  | bool_expr2 IFF bool_expr2 { Iff($1, $3) }
  | NOT bool_expr2 { Not($2) }
  | BVAR { Bvar }
  | LPAREN bool_expr RPAREN { Bparen($2) } 
  ;

int_expr1:
  | int_expr1 PLUS int_expr2 { Plus($1, $3) }
  | int_expr1 MINUS int_expr2 { Minus($1, $3) }
  | int_expr2 { $1 }

int_expr2:
  | int_expr2 TIMES int_expr2 { Times($1, $3) }
  | int_expr2 DIVIDE int_expr2 { Divide($1, $3) }
  | int_expr2 POWER int_expr2 { Power($1, $3) }
  | MINUS int_expr2 { Negative($2) }
  | IVAR { Ivar }
  | INT { Int }
  ;

