%{
    open Ast;;
%}

%token AND
%token OR 
%token NOT
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN

%token EOF

%type <Ast.t> prog1
%start prog1
%%

lexp2:
  | lexp3  { $1 }
  | lexp3 OR lexp2  { Or($1, $3) }
  ;

lexp3:
  | term1  { $1 }
  | NOT lexp3  { Not($2) }
  ;

lexp4:
  | lexp4 AND lexp2  { And($1, $3) }
  | lexp2  { $1 }
  ;

prog1:
  | lexp4 EOF  { $1 }
  ;

term1:
  | FALSE  { False }
  | TRUE  { True }
  | LPAREN lexp4 RPAREN  { Paren($2) }
  ;
