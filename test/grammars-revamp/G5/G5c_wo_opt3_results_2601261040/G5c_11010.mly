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
  | NOT lexp1  {  Not($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | lexp3 OR lexp4  {  Or($1, $3)  }
  ;

lexp2:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | lexp3 AND lexp2  {  And($1, $3)  }
  | lexp3 OR lexp4  {  Or($1, $3)  }
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  ;

lexp3:
  | FALSE  {  False  }
  | lexp3 OR lexp4  {  Or($1, $3)  }
  | TRUE  {  True  }
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  ;

lexp4:
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

prog1:
  | lexp1 EOF  {  $1  }
  ;
