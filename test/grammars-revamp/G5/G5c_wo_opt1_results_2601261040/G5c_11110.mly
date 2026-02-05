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
  | lexp2  { $1 }
  | NOT lexp1  {  Not($2)  }
  ;

lexp2:
  | lexp3  { $1 }
  | lexp2 OR lexp3  {  Or($1, $3)  }
  ;

lexp3:
  | lexp4  { $1 }
  | lexp4 AND lexp3  {  And($1, $3)  }
  ;

lexp4:
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

prog5:
  | lexp1 EOF  {  $1  }
  ;
