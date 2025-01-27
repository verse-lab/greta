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
%type <Ast.exp> int_expr

%start constr
%%

constr: 
    | bool_expr EOF { $1 }
    ;

bool_expr:
    | int_expr LT int_expr { Lt($1, $3) }
    | int_expr GT int_expr { Gt($1, $3) }
    | int_expr EQ int_expr { Eq($1, $3) }
    | int_expr NE int_expr { Ne($1, $3) }
    | int_expr GTE int_expr { Gte($1, $3) }
    | int_expr LTE int_expr { Lte($1, $3) }
    | bool_expr AND bool_expr2 { And($1, $3) }
    | bool_expr OR bool_expr2 { Or($1, $3) }
    ;

bool_expr2:
    | bool_expr2 IFF bool_expr2 { Iff($1, $3) }
    | NOT bool_expr2 { Not($2) }
    | BVAR { Bvar }
    | LPAREN bool_expr RPAREN { Bparen($2) } 
    ;

int_expr:
    | int_expr PLUS int_expr { Plus($1, $3) }
    | int_expr MINUS int_expr { Minus($1, $3) }
    | int_expr TIMES int_expr { Times($1, $3) }
    | int_expr DIVIDE int_expr { Divide($1, $3) }
    | int_expr POWER int_expr { Power($1, $3) }
    | MINUS int_expr { Negative($2) }
    | IVAR { Ivar }
    | INT { Int }
    | LPAREN int_expr RPAREN { Paren($2) }
    ;

