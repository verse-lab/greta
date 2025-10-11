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

lexp1:
  | lexp2 AND lexp1  { And($1, $3) }
  | lexp2  { $1 }
  ;

lexp2:
  | lexp3  { $1 }
  | lexp3 OR lexp2  { Or($1, $3) }
  ;

lexp3:
  | FALSE  { False }
  | NOT lexp3  { Not($2) }
  | TRUE  { True }
  | LPAREN lexp1 RPAREN  { Paren($2) }
  ;

prog1:
  | lexp1 EOF  { $1 }
  ;
