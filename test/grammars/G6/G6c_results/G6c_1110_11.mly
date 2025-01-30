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
%left PLUS MINUS
%left TIMES DIVIDE
%right POWER NOT




%type <Ast.t> constr
%type <Ast.exp> x4
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

x4:
  | x4 PLUS x4 { Plus($1, $3) }
  | x4 MINUS x4 { Minus($1, $3) }
  | x4 TIMES x4 { Times($1, $3) }
  | x4 DIVIDE x4 { Divide($1, $3) }
  | x4 POWER x4 { Power($1, $3) }
  | MINUS x4 { Negative($2) }
  | IVAR { Ivar }
  | INT { Int }
  ;

x3:
  | x4 NE x4 { Ne($1, $3) }
  | x4 LTE x4 { Lte($1, $3) }
  | x4 LT x4 { Lt($1, $3) }
  | x4 GTE x4 { Gte($1, $3) }
  | x4 GT x4 { Gt($1, $3) }
  | x4 EQ x4 { Eq($1, $3) }
  | x3 AND x3 { And($1, $3) }
  | LPAREN e1 RPAREN { Bparen($2) } 
  | BVAR { Bvar }
  ;

e1:
  | x1 { $1 }
  | e1 OR e1 { Or($1, $3) }
  ;

x2:
  | x3 { $1 }
  | x2 IFF x3 { Iff($1, $3) }
  ;

x1:
  | x2 { $1 }
  | NOT x1 { Not($2) }
  ;

