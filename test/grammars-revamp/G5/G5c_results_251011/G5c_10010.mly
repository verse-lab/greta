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
  | NOT lexp2  { Not($2) }
  ;

lexp3:
  | lexp3 OR lexp4  { Or($1, $3) }
  | lexp4  { $1 }
  ;

lexp4:
  | LPAREN lexp1 RPAREN  { Paren($2) }
  | TRUE  { True }
  | FALSE  { False }
  ;

prog1:
  | lexp1 EOF  { $1 }
  ;
