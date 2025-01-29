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
%type <Ast.exp> x6
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

x6:
  | x6 PLUS x6 { Plus($1, $3) }
  | x6 MINUS x6 { Minus($1, $3) }
  | x6 TIMES x6 { Times($1, $3) }
  | x6 DIVIDE x6 { Divide($1, $3) }
  | x6 POWER x6 { Power($1, $3) }
  | MINUS x6 { Negative($2) }
  | IVAR { Ivar }
  | INT { Int }
  ;

x2:
  | x3 { $1 }
  ;

e1:
  | x1 { $1 }
  | e1 IFF x2 { Iff($1, $3) }
  ;

x7:
  | LPAREN e1 RPAREN { Bparen($2) } 
  ;

x5:
  | x7 { $1 }
  | x6 NE x6 { Ne($1, $3) }
  | x6 LTE x6 { Lte($1, $3) }
  | x6 LT x6 { Lt($1, $3) }
  | x6 GTE x6 { Gte($1, $3) }
  | x6 GT x6 { Gt($1, $3) }
  | x6 EQ x6 { Eq($1, $3) }
  | NOT x5 { Not($2) }
  ;

x3:
  | x3 AND x3 { And($1, $3) }
  | x2 { $1 }
  ;

x1:
  | x2 { $1 }
  | x1 OR x1 { Or($1, $3) }
  ;

