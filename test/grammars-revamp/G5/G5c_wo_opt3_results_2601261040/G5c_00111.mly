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
  | lexp3 AND lexp2  {  And($1, $3)  }
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  | NOT lexp3  {  Not($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | lexp2 OR lexp1  {  Or($1, $3)  }
  ;

lexp2:
  | TRUE  {  True  }
  | NOT lexp3  {  Not($2)  }
  | FALSE  {  False  }
  | lexp3 AND lexp2  {  And($1, $3)  }
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  ;

lexp3:
  | FALSE  {  False  }
  | NOT lexp3  {  Not($2)  }
  | TRUE  {  True  }
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  ;

prog1:
  | lexp1 EOF  {  $1  }
  ;
