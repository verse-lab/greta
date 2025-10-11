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


%start <Ast.t> program

%start program
%%

program:
  | query EOF { $1 }
  ;

query:
  | UPDATE table_name SET assignment_list { Update ($2, $4, None, None, None) }
  | UPDATE table_name SET assignment_list option_list { let (ow, oo, ol) = $5 in Update ($2, $4, ow, oo, ol)  }
  ;

table_name:	
  | IDENTIFIER { Table $1 }
  ;

col_name:
  | IDENTIFIER { Col $1 }
  ;

value:  
  | expr { ValueExpr $1 }
  | DEFAULT { Default }
  ;

assignment_list:
  | assignment_list COMMA assignment_list { $1 @ $3 }
  | LPAREN assignment RPAREN { [$2] }
  | assignment { [ $1 ] }
  ;

assignment: 
  | col_name EQ value { Assign ($1, $3) }
  ;

option_list:
  | opt_where opt_order opt_limit  { ($1, $2, $3) }
  | opt_order opt_limit  { (None, $1, $2) }
  | opt_where opt_limit  { ($1, None, $2) }
  ;

opt_where:
  | WHERE condition_list  { Some (Where $2) }
  ;

opt_order:
  | ORDER BY order_by_list  { Some (OrderBy $3) }
  ;

opt_limit:
  |    { None }
  | LIMIT POSITIVE_DIGIT { Some (Limit $2) }
  ;

condition_list: 
  | condition_list SEMICOLON condition_list { $1 @ $3 }
  | condition { [$1] }
  ;

condition:
  | condition OR condition { Or  ($1, $3) }
  | condition AND condition  { And  ($1, $3) }
  | COND_ASSIGN assignment { CondAssign $2 }
  |	col_name COMPARISION_OPERATOR real_number { Compare ($2, $1, $3) }
  | col_name BETWEEN real_number AND real_number { Between ($1, $3, $5) }
  | LPAREN condition RPAREN { $2 }
  ;

order_by_list:
  | order_by_list COMMA order_by_list { $1 @ $3 }
  | order_term { [ $1 ] }

order_term: 
  | col_name ASC { Order ($1, `Asc) }
  | col_name DESC  { Order ($1, `Desc) }
  ;

expr:
  | expr PLUS expr { Plus ($1, $3) }
  | expr MINUS expr { Minus ($1, $3) }
  | expr STAR expr { Star ($1, $3) }
  | expr DIV expr { Div ($1, $3) }
  | real_number { $1 }
  | identifiers_strings { $1 }
  | LPAREN expr RPAREN  { $2 }
  ;

identifiers_strings:
  | IDENTIFIER { Var $1 }
  |	STRING_LITERAL { Str $1 }
  ;

real_number:
  | POSITIVE_DIGIT { Int $1 }
  | NEGATIVE_DIGIT  { Int $1 }
  | FLOAT { Float $1 }
  ;

