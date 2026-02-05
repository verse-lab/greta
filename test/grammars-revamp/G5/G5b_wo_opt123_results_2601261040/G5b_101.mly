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
%token EOF%type <Ast.t> prog15
%start prog15
%%

lexp10:
  | NOT lexp9  {  Not($2)  }
  | term3  {  $1  }
  | lexp16 OR lexp12  {  Or($1, $3)  }
  ;

lexp11:
  | term3  {  $1  }
  | lexp16 OR lexp12  {  Or($1, $3)  }
  | NOT lexp9  {  Not($2)  }
  ;

lexp12:
  | lexp16 OR lexp12  {  Or($1, $3)  }
  | term3  {  $1  }
  ;

lexp13:
  | lexp14 AND lexp17  {  And($1, $3)  }
  | lexp11  {  $1  }
  ;

lexp14:
  | lexp11  {  $1  }
  | lexp14 AND lexp17  {  And($1, $3)  }
  ;

lexp16:
  | term3  {  $1  }
  ;

lexp17:
  | lexp11  {  $1  }
  ;

lexp8:
  | term3  {  $1  }
  | lexp16 OR lexp12  {  Or($1, $3)  }
  | NOT lexp9  {  Not($2)  }
  ;

lexp9:
  | term3  {  $1  }
  | lexp16 OR lexp12  {  Or($1, $3)  }
  | NOT lexp9  {  Not($2)  }
  ;

prog15:
  | lexp13 EOF  {  $1  }
  ;

term1:
  | FALSE  {  False  }
  | TRUE  {  True  }
  | LPAREN lexp14 RPAREN  {  Paren($2)  }
  ;

term2:
  | LPAREN lexp14 RPAREN  {  Paren($2)  }
  | FALSE  {  False  }
  | TRUE  {  True  }
  ;

term3:
  | FALSE  {  False  }
  | TRUE  {  True  }
  | LPAREN lexp14 RPAREN  {  Paren($2)  }
  ;

term4:
  | LPAREN lexp14 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

term5:
  | FALSE  {  False  }
  | LPAREN lexp14 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  ;

term6:
  | FALSE  {  False  }
  | LPAREN lexp14 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  ;

term7:
  | LPAREN lexp14 RPAREN  {  Paren($2)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;
