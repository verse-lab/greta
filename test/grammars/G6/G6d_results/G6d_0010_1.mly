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

%left AND
%left OR
%left PLUS MINUS TIMES DIVIDE POWER




%type <Ast.t> constr
%type <Ast.exp> x7
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

x7:
  | x7 PLUS x7 { Plus($1, $3) }
  | x7 MINUS x7 { Minus($1, $3) }
  | x7 TIMES x7 { Times($1, $3) }
  | x7 DIVIDE x7 { Divide($1, $3) }
  | x7 POWER x7 { Power($1, $3) }
  | MINUS x7 { Negative($2) }
  | IVAR { Ivar }
  | INT { Int }
  ;

e1:
  | x1 { $1 }
  | e1 OR e1 { Or($1, $3) }
  ;

x8:
  | LPAREN e1 RPAREN { Bparen($2) } 
  ;

x6:
  | x8 { $1 }
  | x7 NE x7 { Ne($1, $3) }
  | x7 LTE x7 { Lte($1, $3) }
  | x7 LT x7 { Lt($1, $3) }
  | x7 GTE x7 { Gte($1, $3) }
  | x7 GT x7 { Gt($1, $3) }
  | x7 EQ x7 { Eq($1, $3) }
  | NOT x6 { Not($2) }
  ;

x4:
  | x4 AND x4 { And($1, $3) }
  | x3 { $1 }
  ;

x2:
  | x2 { $1 }
  ;

x1:
  | x2 { $1 }
  | x1 IFF x2 { Iff($1, $3) }
  ;

