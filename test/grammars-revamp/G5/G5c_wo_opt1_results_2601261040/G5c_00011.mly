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
%token EOF%type <Ast.t> prog5
%start prog5
%%

lexp1:
  | lexp2 AND lexp1  {  And($1, $3)  }
  | lexp2  { $1 }
  ;

lexp2:
  | lexp3  { $1 }
  | lexp3 OR lexp2  {  Or($1, $3)  }
  ;

lexp3:
  | lexp4  { $1 }
  | NOT lexp3  {  Not($2)  }
  ;

lexp4:
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

prog5:
  | lexp1 EOF  {  $1  }
  ;
