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
%token EOF%type <Ast.t> prog6
%start prog6
%%

lexp1:
  | lexp5 AND lexp4  {  And($1, $3)  }
  | LPAREN lexp2 RPAREN  {  Paren($2)  }
  | NOT lexp2  {  Not($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | lexp3 OR lexp4  {  Or($1, $3)  }
  ;

lexp2:
  | TRUE  {  True  }
  | NOT lexp2  {  Not($2)  }
  | FALSE  {  False  }
  | lexp5 AND lexp4  {  And($1, $3)  }
  | lexp3 OR lexp4  {  Or($1, $3)  }
  | LPAREN lexp2 RPAREN  {  Paren($2)  }
  ;

lexp3:
  | FALSE  {  False  }
  | lexp3 OR lexp4  {  Or($1, $3)  }
  | lexp5 AND lexp4  {  And($1, $3)  }
  | TRUE  {  True  }
  | LPAREN lexp2 RPAREN  {  Paren($2)  }
  ;

lexp4:
  | LPAREN lexp2 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | lexp5 AND lexp4  {  And($1, $3)  }
  | FALSE  {  False  }
  ;

lexp5:
  | LPAREN lexp2 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

prog6:
  | lexp1 EOF  {  $1  }
  ;
