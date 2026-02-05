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
%token EOF%start <Ast.t> prog1
%start prog1
%%

assi13:
  | assi15  { $1 }
  ;

assi15:
  | assi24 COMMA assi13  {  $1 @ $3  }
  | assi24  { $1 }
  ;

assi24:
  | assi33  {  [ $1 ]  }
  | LPAREN assi33 RPAREN  {  [$2]  }
  ;

assi26:
  | assi33  { $1 }
  ;

assi33:
  | col_30 EQ valu22  {  Assign ($1, $3)  }
  ;

col_27:
  | col_30  { $1 }
  ;

col_30:
  | IDENTIFIER  {  Col $1  }
  ;

cond19:
  | cond21  { $1 }
  ;

cond21:
  | cond29 SEMICOLON cond19  {  $1 @ $3  }
  | cond29  { $1 }
  ;

cond29:
  | cond5  {  [$1]  }
  ;

cond3:
  | cond5  { $1 }
  ;

cond5:
  | cond6 AND cond3  {  And  ($1, $3)  }
  | cond6  { $1 }
  ;

cond6:
  | cond8 OR cond6  {  Or  ($1, $3)  }
  | cond8  { $1 }
  ;

cond8:
  | col_27 COMPARISION_OPERATOR real10  {  Compare ($2, $1, $3)  }
  | col_27 BETWEEN real10 AND real10  {  Between ($1, $3, $5)  }
  | COND_ASSIGN assi26  {  CondAssign $2  }
  | LPAREN cond3 RPAREN  {  $2  }
  ;

expr1:
  | expr2  { $1 }
  ;

expr12:
  | LPAREN expr1 RPAREN  {  $2  }
  | real11  {  $1  }
  | iden16  {  $1  }
  ;

expr2:
  | expr4  { $1 }
  | expr4 PLUS expr1  {  Plus ($1, $3)  }
  ;

expr4:
  | expr7 DIV expr4  {  Div ($1, $3)  }
  | expr7  { $1 }
  ;

expr7:
  | expr9  { $1 }
  | expr9 MINUS expr7  {  Minus ($1, $3)  }
  ;

expr9:
  | expr12 STAR expr9  {  Star ($1, $3)  }
  | expr12  { $1 }
  ;

iden16:
  | STRING_LITERAL  {  Str $1  }
  | IDENTIFIER  {  Var $1  }
  ;

opt_23:
  | LIMIT POSITIVE_DIGIT  {  Some (Limit $2)  }
  |   {  None  }
  ;

opt_31:
  | ORDER BY orde20  {  Some (OrderBy $3)  }
  ;

opt_32:
  | WHERE cond21  {  Some (Where $2)  }
  ;

opti14:
  | opt_32 opt_23  {  ($1, None, $2)  }
  | opt_31 opt_23  {  (None, $1, $2)  }
  | opt_32 opt_31 opt_23  {  ($1, $2, $3)  }
  ;

orde17:
  | col_27 ASC  {  Order ($1, `Asc)  }
  | col_27 DESC  {  Order ($1, `Desc)  }
  ;

orde18:
  | orde20  { $1 }
  ;

orde20:
  | orde28 COMMA orde18  {  $1 @ $3  }
  | orde28  { $1 }
  ;

orde28:
  | orde17  {  [ $1 ]  }
  ;

prog1:
  | quer25 EOF  {  $1  }
  ;

quer25:
  | UPDATE tabl34 SET assi15  {  Update ($2, $4, None, None, None)  }
  | UPDATE tabl34 SET assi15 opti14  {  let (ow, oo, ol) = $5 in Update ($2, $4, ow, oo, ol)   }
  ;

real10:
  | real11  { $1 }
  ;

real11:
  | FLOAT  {  Float $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  ;

tabl34:
  | IDENTIFIER  {  Table $1  }
  ;

valu22:
  | expr2  {  ValueExpr $1  }
  | DEFAULT  {  Default  }
  ;
