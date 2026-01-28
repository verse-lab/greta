%{

open Ast

%}
%token <string> IDENTIFIER
%token <float> FLOAT
%token <int> NEGATIVE_DIGIT
%token <int> POSITIVE_DIGIT
%token <string> STRING_LITERAL
%token <string> COMPARISION_OPERATOR
%token UPDATE SET WHERE ORDER BY ASC DESC LIMIT AND OR DEFAULT BETWEEN
%token LPAREN RPAREN EQ COMMA SEMICOLON COND_ASSIGN
%token PLUS MINUS STAR DIV
%token EOF%start <Ast.t> prog21
%start prog21
%%

assi11:
  | assi19  { $1 }
  | assi19 COMMA assi11  {  $1 @ $3  }
  ;

assi19:
  | assi27  {  [ $1 ]  }
  | LPAREN assi27 RPAREN  {  [$2]  }
  ;

assi27:
  | col_26 EQ valu12  {  Assign ($1, $3)  }
  ;

col_26:
  | IDENTIFIER  {  Col $1  }
  ;

cond18:
  | cond25  { $1 }
  | cond25 SEMICOLON cond18  {  $1 @ $3  }
  ;

cond25:
  | cond3  {  [$1]  }
  ;

cond3:
  | cond5  { $1 }
  | cond5 AND cond3  {  And  ($1, $3)  }
  ;

cond5:
  | cond7 OR cond5  {  Or  ($1, $3)  }
  | cond7  { $1 }
  ;

cond7:
  | col_26 BETWEEN real8 AND real8  {  Between ($1, $3, $5)  }
  | LPAREN cond3 RPAREN  {  $2  }
  | COND_ASSIGN assi27  {  CondAssign $2  }
  | col_26 COMPARISION_OPERATOR real8  {  Compare ($2, $1, $3)  }
  ;

expr1:
  | expr2 MINUS expr1  {  Minus ($1, $3)  }
  | expr2  { $1 }
  ;

expr10:
  | iden17  {  $1  }
  | real8  {  $1  }
  | LPAREN expr1 RPAREN  {  $2  }
  ;

expr2:
  | expr4  { $1 }
  | expr4 PLUS expr2  {  Plus ($1, $3)  }
  ;

expr4:
  | expr6 DIV expr4  {  Div ($1, $3)  }
  | expr6  { $1 }
  ;

expr6:
  | expr10  { $1 }
  | expr10 STAR expr6  {  Star ($1, $3)  }
  ;

iden17:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

opt_16:
  | LIMIT POSITIVE_DIGIT  {  Some (Limit $2)  }
  |   {  None  }
  ;

opt_23:
  | WHERE cond18  {  Some (Where $2)  }
  ;

opt_24:
  | ORDER BY orde15  {  Some (OrderBy $3)  }
  ;

opti9:
  | opt_23 opt_16  {  ($1, None, $2)  }
  | opt_24 opt_16  {  (None, $1, $2)  }
  | opt_23 opt_24 opt_16  {  ($1, $2, $3)  }
  ;

orde14:
  | col_26 DESC  {  Order ($1, `Desc)  }
  | col_26 ASC  {  Order ($1, `Asc)  }
  ;

orde15:
  | orde22 COMMA orde15  {  $1 @ $3  }
  | orde22  { $1 }
  ;

orde22:
  | orde14  {  [ $1 ]  }
  ;

prog21:
  | quer13 EOF  {  $1  }
  ;

quer13:
  | UPDATE tabl20 SET assi11 opti9  {  let (ow, oo, ol) = $5 in Update ($2, $4, ow, oo, ol)   }
  | UPDATE tabl20 SET assi11  {  Update ($2, $4, None, None, None)  }
  ;

real8:
  | POSITIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  ;

tabl20:
  | IDENTIFIER  {  Table $1  }
  ;

valu12:
  | expr1  {  ValueExpr $1  }
  | DEFAULT  {  Default  }
  ;
