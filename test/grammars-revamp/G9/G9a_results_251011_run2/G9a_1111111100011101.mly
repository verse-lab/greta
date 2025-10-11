// SQL language

%{
open Ast
%}

%token <string> IDENTIFIER
%token <float>  FLOAT
%token <int> NEGATIVE_DIGIT 
%token <int> POSITIVE_DIGIT
%token <string> STRING_LITERAL
%token <string> COMPARISION_OPERATOR
%token UPDATE SET WHERE ORDER BY ASC DESC LIMIT AND OR DEFAULT BETWEEN
%token LPAREN RPAREN EQ COMMA SEMICOLON COND_ASSIGN
%token PLUS MINUS STAR DIV

%token EOF


%start <Ast.t> prog1
%start prog1
%%

assi11:
  | assi18  { $1 }
  | assi18 COMMA assi11  { $1 @ $3 }
  ;

assi18:
  | LPAREN assi25 RPAREN  { [$2] }
  | assi25  { [ $1 ] }
  ;

assi25:
  | col_22 EQ valu16  { Assign ($1, $3) }
  ;

col_22:
  | IDENTIFIER  { Col $1 }
  ;

cond15:
  | cond21  { $1 }
  | cond21 SEMICOLON cond15  { $1 @ $3 }
  ;

cond21:
  | cond3  { [$1] }
  ;

cond3:
  | cond4  { $1 }
  | cond4 AND cond3  { And  ($1, $3) }
  ;

cond4:
  | cond6 OR cond4  { Or  ($1, $3) }
  | cond6  { $1 }
  ;

cond6:
  | col_22 BETWEEN real9 AND real9  { Between ($1, $3, $5) }
  | col_22 COMPARISION_OPERATOR real9  { Compare ($2, $1, $3) }
  | COND_ASSIGN assi25  { CondAssign $2 }
  | LPAREN cond3 RPAREN  { $2 }
  ;

expr1:
  | expr2 MINUS expr1  { Minus ($1, $3) }
  | expr2  { $1 }
  ;

expr2:
  | expr5  { $1 }
  | expr2 DIV expr5  { Div ($1, $3) }
  ;

expr5:
  | expr7 STAR expr5  { Star ($1, $3) }
  | expr7  { $1 }
  ;

expr7:
  | expr8  { $1 }
  | expr7 PLUS expr8  { Plus ($1, $3) }
  ;

expr8:
  | real9  { $1 }
  | iden12  { $1 }
  | LPAREN expr1 RPAREN  { $2 }
  ;

iden12:
  | IDENTIFIER  { Var $1 }
  | STRING_LITERAL  { Str $1 }
  ;

opt_17:
  | LIMIT POSITIVE_DIGIT  { Some (Limit $2) }
  |   { None }
  ;

opt_23:
  | ORDER BY orde14  { Some (OrderBy $3) }
  ;

opt_24:
  | WHERE cond15  { Some (Where $2) }
  ;

opti10:
  | opt_24 opt_23 opt_17  { ($1, $2, $3) }
  | opt_23 opt_17  { (None, $1, $2) }
  | opt_24 opt_17  { ($1, None, $2) }
  ;

orde13:
  | col_22 ASC  { Order ($1, `Asc) }
  | col_22 DESC  { Order ($1, `Desc) }
  ;

orde14:
  | orde20  { $1 }
  | orde20 COMMA orde14  { $1 @ $3 }
  ;

orde20:
  | orde13  { [ $1 ] }
  ;

prog1:
  | quer19 EOF  { $1 }
  ;

quer19:
  | UPDATE tabl26 SET assi11  { Update ($2, $4, None, None, None) }
  | UPDATE tabl26 SET assi11 opti10  { let (ow, oo, ol) = $5 in Update ($2, $4, ow, oo, ol)  }
  ;

real9:
  | POSITIVE_DIGIT  { Int $1 }
  | FLOAT  { Float $1 }
  | NEGATIVE_DIGIT  { Int $1 }
  ;

tabl26:
  | IDENTIFIER  { Table $1 }
  ;

valu16:
  | DEFAULT  { Default }
  | expr1  { ValueExpr $1 }
  ;
