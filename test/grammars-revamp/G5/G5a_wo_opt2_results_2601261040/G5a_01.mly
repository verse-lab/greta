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

lexp1:
  | lexp3  { $1 }
  ;

lexp3:
  | lexp4 OR lexp1  {  Or($1, $3)  }
  | lexp4  { $1 }
  ;

lexp4:
  | term2  {  $1  }
  | NOT lexp4  {  Not($2)  }
  ;

lexp5:
  | lexp6  { $1 }
  ;

lexp6:
  | lexp3  {  $1  }
  | lexp5 AND lexp3  {  And($1, $3)  }
  ;

prog1:
  | lexp6 EOF  {  $1  }
  ;

term2:
  | LPAREN lexp5 RPAREN  {  Paren($2)  }
  | FALSE  {  False  }
  | TRUE  {  True  }
  ;
