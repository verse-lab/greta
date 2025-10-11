// SQL language

%{
open Ast
%}

%token UPDATE  SET  WHERE ORDER BY ASC DESC LIMIT AND OR DEFAULT BETWEEN
%token KEYWORD
%token IDENTIFIER STRING_LITERAL NEGATIVE_DIGIT POSITIVE_DIGIT FLOAT DATE COMPARISION_OPERATOR
%token LPAREN RPAREN EQ COMMA

%token EOF


%type <Ast.t> constr
%type <Ast.exp> int_expr

%start program
%%

program:
  | query EOF { $1 }
  ;

query:
  | UPDATE table_name SET assignment_list
  | UPDATE table_name SET assignment_list option_list
  ;

assignment_list:
  | assignment_list COMMA assignment_list
  | LPAREN assignment RPAREN
  | assignment
  ;

assignment: 
  | col_name EQ value
  ;

table_name:	
  | IDENTIFIER
  ;

col_name:
  | IDENTIFIER
  ;

value:  
  | expr
  | DEFAULT
  ;

expr:
  | expr PLUS expr
  | expr MINUS expr
  | expr STAR expr
  | expr DIV expr
  | real_number
  | identifiers_strings
  ;

option_list:
  | WHERE condition_list
  | ORDER BY order_by_list
  | LIMIT POSITIVE_DIGIT
  | WHERE condition_list ORDER BY order_by_list
  | WHERE condition_list LIMIT POSITIVE_DIGIT
  | ORDER BY order_by_list LIMIT POSITIVE_DIGIT
  | WHERE condition_list ORDER BY order_by_list LIMIT POSITIVE_DIGIT
  ;

condition_list:	
  | condition_list OR condition_list
  | condition_list AND condition_list
  | LPAREN condition RPAREN
  |	condition
  ;

condition:
  | assignment
  |	col_name COMPARISION_OPERATOR real_number
  | col_name BETWEEN real_number AND real_number
  ;

identifiers_strings:
  | IDENTIFIER
  |	STRING_LITERAL
  ;

real_number:
  | POSITIVE_DIGIT
  | MINUS real_number UMINUS
  | FLOAT
  ;

order_by_list:
  | order_by_list COMMA order_by_list
  | col_name
  | col_name ASC
  | col_name DESC
  ;
