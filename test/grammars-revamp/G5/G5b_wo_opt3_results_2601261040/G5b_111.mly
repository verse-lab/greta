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
%token EOF%type <Ast.t> prog1
%start prog1
%%

lexp2:
  | NOT lexp2  {  Not($2)  }
  | term1  {  $1  }
  | lexp5 OR lexp3  {  Or($1, $3)  }
  ;

lexp3:
  | lexp5 OR lexp3  {  Or($1, $3)  }
  | term1  {  $1  }
  ;

lexp4:
  | lexp6 AND lexp4  {  And($1, $3)  }
  | lexp2  {  $1  }
  ;

lexp5:
  | term1  {  $1  }
  ;

lexp6:
  | lexp2  {  $1  }
  ;

prog1:
  | lexp4 EOF  {  $1  }
  ;

term1:
  | FALSE  {  False  }
  | TRUE  {  True  }
  | LPAREN lexp4 RPAREN  {  Paren($2)  }
  ;
