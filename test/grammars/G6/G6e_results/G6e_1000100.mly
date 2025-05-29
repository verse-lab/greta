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
%type <Ast.exp> x2
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

x3:
  | INT { Int }
  | IVAR { Ivar }
  | MINUS x3 { Negative($2) }
  ;

x2:
  | x3 { $1 }
  | x2 POWER x2 { Power($1, $3) }
  | x2 PLUS x2 { Plus($1, $3) }
  | x2 MINUS x2 { Minus($1, $3) }
  | x2 DIVIDE x2 { Divide($1, $3) }
  ;

x1:
  | x2 NE x2 { Ne($1, $3) }
  | x2 LTE x2 { Lte($1, $3) }
  | x2 LT x2 { Lt($1, $3) }
  | x2 GTE x2 { Gte($1, $3) }
  | x2 GT x2 { Gt($1, $3) }
  | x2 EQ x2 { Eq($1, $3) }
  | x1 OR x1 { Or($1, $3) }
  | x1 IFF x1 { Iff($1, $3) }
  | x1 AND x1 { And($1, $3) }
  | LPAREN e1 RPAREN { Bparen($2) } 
  | BVAR { Bvar }
  ;

e1:
  | x1 { $1 }
  | NOT e1 { Not($2) }
  ;

