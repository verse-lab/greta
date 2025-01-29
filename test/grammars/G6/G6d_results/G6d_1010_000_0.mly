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
%type <Ast.exp> x5
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

x5:
  | x5 PLUS x5 { Plus($1, $3) }
  | x5 MINUS x5 { Minus($1, $3) }
  | x5 TIMES x5 { Times($1, $3) }
  | x5 DIVIDE x5 { Divide($1, $3) }
  | x5 POWER x5 { Power($1, $3) }
  | MINUS x5 { Negative($2) }
  | IVAR { Ivar }
  | INT { Int }
  ;

e1:
  | x1 { $1 }
  | e1 OR e1 { Or($1, $3) }
  ;

x6:
  | LPAREN e1 RPAREN { Bparen($2) } 
  ;

x4:
  | x6 { $1 }
  | x5 NE x5 { Ne($1, $3) }
  | x5 LTE x5 { Lte($1, $3) }
  | x5 LT x5 { Lt($1, $3) }
  | x5 GTE x5 { Gte($1, $3) }
  | x5 GT x5 { Gt($1, $3) }
  | x5 EQ x5 { Eq($1, $3) }
  | NOT x4 { Not($2) }
  ;

x3:
  | x4 { $1 }
  ;

x2:
  | x3 { $1 }
  | x2 AND x2 { And($1, $3) }
  ;

x1:
  | x2 IFF x1 { Iff($1, $3) }
  | x2 { $1 }
  ;

