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

%left AND OR
%left IFF PLUS MINUS TIMES DIVIDE




%type <Ast.t> constr
%type <Ast.exp> x3
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

x5:
  | INT { Int }
  | IVAR { Ivar }
  | MINUS x5 { Negative($2) }
  ;

e1:
  | x1 { $1 }
  | e1 OR e1 { Or($1, $3) }
  ;

x4:
  | x5 { $1 }
  | x4 PLUS x4 { Plus($1, $3) }
  | x4 MINUS x4 { Minus($1, $3) }
  | x4 DIVIDE x4 { Divide($1, $3) }
  ;

x3:
  | x4 POWER x3 { Power($1, $3) }
  | x4 { $1 }
  ;

x2:
  | x3 NE x3 { Ne($1, $3) }
  | x3 LTE x3 { Lte($1, $3) }
  | x3 LT x3 { Lt($1, $3) }
  | x3 GTE x3 { Gte($1, $3) }
  | x3 GT x3 { Gt($1, $3) }
  | x3 EQ x3 { Eq($1, $3) }
  | x2 IFF x2 { Iff($1, $3) }
  | x2 AND x2 { And($1, $3) }
  | LPAREN e1 RPAREN { Bparen($2) } 
  | BVAR { Bvar }
  ;

x1:
  | x2 { $1 }
  | NOT x1 { Not($2) }
  ;

