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

lexp2:
  | lexp3  { $1 }
  | NOT lexp2  {  Not($2)  }
  ;

lexp3:
  | lexp6 OR lexp3  {  Or($1, $3)  }
  | lexp6  { $1 }
  ;

lexp4:
  | lexp7 AND lexp4  {  And($1, $3)  }
  | lexp7  { $1 }
  ;

lexp6:
  | term1  {  $1  }
  ;

lexp7:
  | lexp2  {  $1  }
  ;

prog5:
  | lexp4 EOF  {  $1  }
  ;

term1:
  | FALSE  {  False  }
  | TRUE  {  True  }
  | LPAREN lexp4 RPAREN  {  Paren($2)  }
  ;
