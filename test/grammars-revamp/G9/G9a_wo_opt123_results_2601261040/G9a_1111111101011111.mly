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
%token EOF%start <Ast.t> prog101
%start prog101
%%

assi121:
  | col_117 EQ valu48  {  Assign ($1, $3)  }
  ;

assi122:
  | col_117 EQ valu48  {  Assign ($1, $3)  }
  ;

assi123:
  | col_117 EQ valu48  {  Assign ($1, $3)  }
  ;

assi124:
  | col_117 EQ valu48  {  Assign ($1, $3)  }
  ;

assi125:
  | col_117 EQ valu48  {  Assign ($1, $3)  }
  ;

assi42:
  | assi96 COMMA assi42  {  $1 @ $3  }
  | LPAREN assi121 RPAREN  {  [$2]  }
  | assi121  {  [ $1 ]  }
  ;

assi43:
  | assi121  {  [ $1 ]  }
  | assi96 COMMA assi42  {  $1 @ $3  }
  | LPAREN assi121 RPAREN  {  [$2]  }
  ;

assi44:
  | LPAREN assi121 RPAREN  {  [$2]  }
  | assi96 COMMA assi42  {  $1 @ $3  }
  | assi121  {  [ $1 ]  }
  ;

assi96:
  | LPAREN assi121 RPAREN  {  [$2]  }
  | assi121  {  [ $1 ]  }
  ;

col_114:
  | IDENTIFIER  {  Col $1  }
  ;

col_115:
  | IDENTIFIER  {  Col $1  }
  ;

col_116:
  | IDENTIFIER  {  Col $1  }
  ;

col_117:
  | IDENTIFIER  {  Col $1  }
  ;

col_118:
  | IDENTIFIER  {  Col $1  }
  ;

col_119:
  | IDENTIFIER  {  Col $1  }
  ;

col_120:
  | IDENTIFIER  {  Col $1  }
  ;

cond10:
  | cond21 OR cond19  {  Or  ($1, $3)  }
  | cond19 AND cond13  {  And  ($1, $3)  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  ;

cond11:
  | cond19 AND cond13  {  And  ($1, $3)  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | cond21 OR cond19  {  Or  ($1, $3)  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  ;

cond113:
  | cond15  {  [$1]  }
  ;

cond12:
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | cond21 OR cond19  {  Or  ($1, $3)  }
  | cond19 AND cond13  {  And  ($1, $3)  }
  ;

cond13:
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | cond19 AND cond13  {  And  ($1, $3)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | cond21 OR cond19  {  Or  ($1, $3)  }
  ;

cond14:
  | cond21 OR cond19  {  Or  ($1, $3)  }
  | cond19 AND cond13  {  And  ($1, $3)  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  ;

cond15:
  | cond19 AND cond13  {  And  ($1, $3)  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | cond21 OR cond19  {  Or  ($1, $3)  }
  ;

cond16:
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | cond21 OR cond19  {  Or  ($1, $3)  }
  | cond19 AND cond13  {  And  ($1, $3)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  ;

cond17:
  | cond21 OR cond19  {  Or  ($1, $3)  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | cond19 AND cond13  {  And  ($1, $3)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  ;

cond19:
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | cond21 OR cond19  {  Or  ($1, $3)  }
  ;

cond21:
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | LPAREN cond13 RPAREN  {  $2  }
  ;

cond22:
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  | LPAREN cond13 RPAREN  {  $2  }
  | COND_ASSIGN assi123  {  CondAssign $2  }
  ;

cond23:
  | COND_ASSIGN assi123  {  CondAssign $2  }
  | LPAREN cond13 RPAREN  {  $2  }
  | col_115 BETWEEN real25 AND real25  {  Between ($1, $3, $5)  }
  | col_115 COMPARISION_OPERATOR real25  {  Compare ($2, $1, $3)  }
  ;

cond90:
  | cond113 SEMICOLON cond93  {  $1 @ $3  }
  | cond15  {  [$1]  }
  ;

cond91:
  | cond113 SEMICOLON cond93  {  $1 @ $3  }
  | cond15  {  [$1]  }
  ;

cond92:
  | cond15  {  [$1]  }
  | cond113 SEMICOLON cond93  {  $1 @ $3  }
  ;

cond93:
  | cond113 SEMICOLON cond93  {  $1 @ $3  }
  | cond15  {  [$1]  }
  ;

cond94:
  | cond113 SEMICOLON cond93  {  $1 @ $3  }
  | cond15  {  [$1]  }
  ;

cond95:
  | cond15  {  [$1]  }
  | cond113 SEMICOLON cond93  {  $1 @ $3  }
  ;

expr1:
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | LPAREN expr4 RPAREN  {  $2  }
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | real28  {  $1  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | iden81  {  $1  }
  ;

expr18:
  | iden81  {  $1  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | real28  {  $1  }
  | LPAREN expr4 RPAREN  {  $2  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  ;

expr2:
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | real28  {  $1  }
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | LPAREN expr4 RPAREN  {  $2  }
  | iden81  {  $1  }
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  ;

expr20:
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | iden81  {  $1  }
  | real28  {  $1  }
  | LPAREN expr4 RPAREN  {  $2  }
  ;

expr3:
  | real28  {  $1  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | LPAREN expr4 RPAREN  {  $2  }
  | iden81  {  $1  }
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | expr18 DIV expr9  {  Div ($1, $3)  }
  ;

expr4:
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | real28  {  $1  }
  | iden81  {  $1  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | LPAREN expr4 RPAREN  {  $2  }
  ;

expr41:
  | real28  {  $1  }
  | iden81  {  $1  }
  | LPAREN expr4 RPAREN  {  $2  }
  ;

expr5:
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | iden81  {  $1  }
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | real28  {  $1  }
  | LPAREN expr4 RPAREN  {  $2  }
  ;

expr6:
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | LPAREN expr4 RPAREN  {  $2  }
  | iden81  {  $1  }
  | real28  {  $1  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  ;

expr7:
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | real28  {  $1  }
  | iden81  {  $1  }
  | LPAREN expr4 RPAREN  {  $2  }
  ;

expr8:
  | real28  {  $1  }
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | LPAREN expr4 RPAREN  {  $2  }
  | expr9 MINUS expr4  {  Minus ($1, $3)  }
  | iden81  {  $1  }
  ;

expr9:
  | iden81  {  $1  }
  | real28  {  $1  }
  | expr20 STAR expr18  {  Star ($1, $3)  }
  | LPAREN expr4 RPAREN  {  $2  }
  | expr18 DIV expr9  {  Div ($1, $3)  }
  | expr41 PLUS expr20  {  Plus ($1, $3)  }
  ;

iden77:
  | STRING_LITERAL  {  Str $1  }
  | IDENTIFIER  {  Var $1  }
  ;

iden78:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

iden79:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

iden80:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

iden81:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

iden82:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

iden83:
  | STRING_LITERAL  {  Str $1  }
  | IDENTIFIER  {  Var $1  }
  ;

iden84:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

iden85:
  | STRING_LITERAL  {  Str $1  }
  | IDENTIFIER  {  Var $1  }
  ;

iden86:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

iden87:
  | STRING_LITERAL  {  Str $1  }
  | IDENTIFIER  {  Var $1  }
  ;

iden88:
  | STRING_LITERAL  {  Str $1  }
  | IDENTIFIER  {  Var $1  }
  ;

iden89:
  | IDENTIFIER  {  Var $1  }
  | STRING_LITERAL  {  Str $1  }
  ;

opt_103:
  | WHERE cond92  {  Some (Where $2)  }
  ;

opt_104:
  | WHERE cond92  {  Some (Where $2)  }
  ;

opt_105:
  | WHERE cond92  {  Some (Where $2)  }
  ;

opt_106:
  | WHERE cond92  {  Some (Where $2)  }
  ;

opt_107:
  | WHERE cond92  {  Some (Where $2)  }
  ;

opt_108:
  | ORDER BY orde68  {  Some (OrderBy $3)  }
  ;

opt_109:
  | ORDER BY orde68  {  Some (OrderBy $3)  }
  ;

opt_110:
  | ORDER BY orde68  {  Some (OrderBy $3)  }
  ;

opt_111:
  | ORDER BY orde68  {  Some (OrderBy $3)  }
  ;

opt_112:
  | ORDER BY orde68  {  Some (OrderBy $3)  }
  ;

opt_72:
  | LIMIT POSITIVE_DIGIT  {  Some (Limit $2)  }
  |   {  None  }
  ;

opt_73:
  | LIMIT POSITIVE_DIGIT  {  Some (Limit $2)  }
  |   {  None  }
  ;

opt_74:
  | LIMIT POSITIVE_DIGIT  {  Some (Limit $2)  }
  |   {  None  }
  ;

opt_75:
  | LIMIT POSITIVE_DIGIT  {  Some (Limit $2)  }
  |   {  None  }
  ;

opt_76:
  |   {  None  }
  | LIMIT POSITIVE_DIGIT  {  Some (Limit $2)  }
  ;

opti37:
  | opt_103 opt_72  {  ($1, None, $2)  }
  | opt_108 opt_72  {  (None, $1, $2)  }
  | opt_103 opt_108 opt_72  {  ($1, $2, $3)  }
  ;

opti38:
  | opt_108 opt_72  {  (None, $1, $2)  }
  | opt_103 opt_72  {  ($1, None, $2)  }
  | opt_103 opt_108 opt_72  {  ($1, $2, $3)  }
  ;

opti39:
  | opt_103 opt_72  {  ($1, None, $2)  }
  | opt_103 opt_108 opt_72  {  ($1, $2, $3)  }
  | opt_108 opt_72  {  (None, $1, $2)  }
  ;

opti40:
  | opt_103 opt_108 opt_72  {  ($1, $2, $3)  }
  | opt_103 opt_72  {  ($1, None, $2)  }
  | opt_108 opt_72  {  (None, $1, $2)  }
  ;

orde102:
  | orde60  {  [ $1 ]  }
  ;

orde54:
  | col_115 ASC  {  Order ($1, `Asc)  }
  | col_115 DESC  {  Order ($1, `Desc)  }
  ;

orde55:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde56:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde57:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde58:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde59:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde60:
  | col_115 ASC  {  Order ($1, `Asc)  }
  | col_115 DESC  {  Order ($1, `Desc)  }
  ;

orde61:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde62:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde63:
  | col_115 ASC  {  Order ($1, `Asc)  }
  | col_115 DESC  {  Order ($1, `Desc)  }
  ;

orde64:
  | col_115 ASC  {  Order ($1, `Asc)  }
  | col_115 DESC  {  Order ($1, `Desc)  }
  ;

orde65:
  | col_115 DESC  {  Order ($1, `Desc)  }
  | col_115 ASC  {  Order ($1, `Asc)  }
  ;

orde66:
  | orde60  {  [ $1 ]  }
  | orde102 COMMA orde69  {  $1 @ $3  }
  ;

orde67:
  | orde102 COMMA orde69  {  $1 @ $3  }
  | orde60  {  [ $1 ]  }
  ;

orde68:
  | orde102 COMMA orde69  {  $1 @ $3  }
  | orde60  {  [ $1 ]  }
  ;

orde69:
  | orde60  {  [ $1 ]  }
  | orde102 COMMA orde69  {  $1 @ $3  }
  ;

orde70:
  | orde60  {  [ $1 ]  }
  | orde102 COMMA orde69  {  $1 @ $3  }
  ;

orde71:
  | orde102 COMMA orde69  {  $1 @ $3  }
  | orde60  {  [ $1 ]  }
  ;

prog101:
  | quer52 EOF  {  $1  }
  ;

quer52:
  | UPDATE tabl100 SET assi44 opti40  {  let (ow, oo, ol) = $5 in Update ($2, $4, ow, oo, ol)   }
  | UPDATE tabl100 SET assi44  {  Update ($2, $4, None, None, None)  }
  ;

quer53:
  | UPDATE tabl100 SET assi44 opti40  {  let (ow, oo, ol) = $5 in Update ($2, $4, ow, oo, ol)   }
  | UPDATE tabl100 SET assi44  {  Update ($2, $4, None, None, None)  }
  ;

real24:
  | NEGATIVE_DIGIT  {  Int $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  ;

real25:
  | NEGATIVE_DIGIT  {  Int $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  ;

real26:
  | POSITIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  ;

real27:
  | NEGATIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  ;

real28:
  | NEGATIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  ;

real29:
  | POSITIVE_DIGIT  {  Int $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  ;

real30:
  | FLOAT  {  Float $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  ;

real31:
  | POSITIVE_DIGIT  {  Int $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  ;

real32:
  | NEGATIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  ;

real33:
  | NEGATIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  ;

real34:
  | POSITIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  ;

real35:
  | POSITIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  | NEGATIVE_DIGIT  {  Int $1  }
  ;

real36:
  | NEGATIVE_DIGIT  {  Int $1  }
  | POSITIVE_DIGIT  {  Int $1  }
  | FLOAT  {  Float $1  }
  ;

tabl100:
  | IDENTIFIER  {  Table $1  }
  ;

tabl97:
  | IDENTIFIER  {  Table $1  }
  ;

tabl98:
  | IDENTIFIER  {  Table $1  }
  ;

tabl99:
  | IDENTIFIER  {  Table $1  }
  ;

valu45:
  | DEFAULT  {  Default  }
  | expr6  {  ValueExpr $1  }
  ;

valu46:
  | expr6  {  ValueExpr $1  }
  | DEFAULT  {  Default  }
  ;

valu47:
  | expr6  {  ValueExpr $1  }
  | DEFAULT  {  Default  }
  ;

valu48:
  | expr6  {  ValueExpr $1  }
  | DEFAULT  {  Default  }
  ;

valu49:
  | DEFAULT  {  Default  }
  | expr6  {  ValueExpr $1  }
  ;

valu50:
  | DEFAULT  {  Default  }
  | expr6  {  ValueExpr $1  }
  ;

valu51:
  | expr6  {  ValueExpr $1  }
  | DEFAULT  {  Default  }
  ;
