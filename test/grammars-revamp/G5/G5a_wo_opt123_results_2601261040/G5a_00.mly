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
%token EOF%type <Ast.t> prog13
%start prog13
%%

lexp10:
  | NOT lexp10  {  Not($2)  }
  | term1  {  $1  }
  ;

lexp11:
  | lexp12 AND lexp9  {  And($1, $3)  }
  | lexp9  {  $1  }
  ;

lexp12:
  | lexp9  {  $1  }
  | lexp12 AND lexp9  {  And($1, $3)  }
  ;

lexp14:
  | term1  {  $1  }
  ;

lexp7:
  | lexp7 OR lexp10  {  Or($1, $3)  }
  | term1  {  $1  }
  | NOT lexp10  {  Not($2)  }
  ;

lexp8:
  | term1  {  $1  }
  | lexp7 OR lexp10  {  Or($1, $3)  }
  | NOT lexp10  {  Not($2)  }
  ;

lexp9:
  | term1  {  $1  }
  | lexp7 OR lexp10  {  Or($1, $3)  }
  | NOT lexp10  {  Not($2)  }
  ;

prog13:
  | lexp11 EOF  {  $1  }
  ;

term1:
  | FALSE  {  False  }
  | TRUE  {  True  }
  | LPAREN lexp12 RPAREN  {  Paren($2)  }
  ;

term2:
  | LPAREN lexp12 RPAREN  {  Paren($2)  }
  | FALSE  {  False  }
  | TRUE  {  True  }
  ;

term3:
  | FALSE  {  False  }
  | TRUE  {  True  }
  | LPAREN lexp12 RPAREN  {  Paren($2)  }
  ;

term4:
  | LPAREN lexp12 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

term5:
  | FALSE  {  False  }
  | LPAREN lexp12 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  ;

term6:
  | FALSE  {  False  }
  | LPAREN lexp12 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  ;
