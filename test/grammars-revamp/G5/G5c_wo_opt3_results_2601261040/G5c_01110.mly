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
  | lexp4 AND lexp3  {  And($1, $3)  }
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  | NOT lexp2  {  Not($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | lexp1 OR lexp2  {  Or($1, $3)  }
  ;

lexp2:
  | TRUE  {  True  }
  | NOT lexp2  {  Not($2)  }
  | FALSE  {  False  }
  | lexp4 AND lexp3  {  And($1, $3)  }
  | LPAREN lexp1 RPAREN  {  Paren($2)  }
  ;

lexp3:
  | FALSE  {  False  }
  | lexp4 AND lexp3  {  And($1, $3)  }
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
