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

%left PLUS MINUS
%left TIMES DIVIDE
%right POWER



%type <Ast.t> constr
%type <Ast.exp> x1
%start constr
%%

constr: 
  | e1 EOF { $1 }
  ;

e1:
  | x1 LT x1 { Lt($1, $3) }
  | x1 GT x1 { Gt($1, $3) }
  | x1 EQ x1 { Eq($1, $3) }
  | x1 NE x1 { Ne($1, $3) }
  | x1 GTE x1 { Gte($1, $3) }
  | x1 LTE x1 { Lte($1, $3) }
  | e1 AND x2 { And($1, $3) }
  | e1 OR x2 { Or($1, $3) }
  | x2 { $1 }
  ;

x1:
  | x1 PLUS x1 { Plus($1, $3) }
  | x1 MINUS x1 { Minus($1, $3) }
  | x1 TIMES x1 { Times($1, $3) }
  | x1 DIVIDE x1 { Divide($1, $3) }
  | x1 POWER x1 { Power($1, $3) }
  | MINUS x1 { Negative($2) }
  | IVAR { Ivar }
  | INT { Int }
  ;

x4:
  | LPAREN e1 RPAREN { Bparen($2) } 
  ;

x3:
  | x4 { $1 }
  | x3 IFF x4 { Iff($1, $3) }
  ;

x2:
  | x3 { $1 }
  | NOT x2 { Not($2) }
  ;

