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
%type <Ast.exp> x4
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

x7:
  | INT { Int }
  | IVAR { Ivar }
  | MINUS x7 { Negative($2) }
  ;

e1:
  | x1 { $1 }
  | e1 OR e1 { Or($1, $3) }
  ;

x6:
  | x7 { $1 }
  | x6 TIMES x6 { Times($1, $3) }
  ;

x1:
  | x2 { $1 }
  | x1 IFF x1 { Iff($1, $3) }
  ;

x5:
  | x6 { $1 }
  | x5 PLUS x5 { Plus($1, $3) }
  ;

x4:
  | x5 POWER x4 { Power($1, $3) }
  | x5 { $1 }
  | x4 MINUS x4 { Minus($1, $3) }
  | x4 DIVIDE x4 { Divide($1, $3) }
  ;

x3:
  | x4 NE x4 { Ne($1, $3) }
  | x4 LTE x4 { Lte($1, $3) }
  | x4 LT x4 { Lt($1, $3) }
  | x4 GTE x4 { Gte($1, $3) }
  | x4 GT x4 { Gt($1, $3) }
  | x4 EQ x4 { Eq($1, $3) }
  | NOT x3 { Not($2) }
  | LPAREN e1 RPAREN { Bparen($2) } 
  | BVAR { Bvar }
  ;

x2:
  | x3 { $1 }
  | x2 AND x2 { And($1, $3) }
  ;

