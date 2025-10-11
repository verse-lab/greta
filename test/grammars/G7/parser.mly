/* SQL language */

%{
open Ast
%}

/* --- Tokens -------------------------------------------------------------- */

%token UPDATE SET WHERE ORDER BY ASC DESC LIMIT AND OR DEFAULT BETWEEN
%token KEYWORD
%token IDENTIFIER STRING_LITERAL NEGATIVE_DIGIT POSITIVE_DIGIT FLOAT DATE COMPARISION_OPERATOR
%token LPAREN RPAREN EQ COMMA
%token PLUS MINUS STAR DIV
%token EOF

/* --- Precedence & associativity ----------------------------------------- */
/* Adjust/extend if you add more operators. */

%left OR
%left AND
%nonassoc COMPARISION_OPERATOR BETWEEN
%left PLUS MINUS
%left STAR DIV

/* --- Start symbol & types ------------------------------------------------ */

%start <Ast.t> program

%%

/* --- Top level ----------------------------------------------------------- */

program:
  | query EOF                                { $1 }
  ;

/* UPDATE ... SET ... [WHERE ...] [ORDER BY ...] [LIMIT ...] */

query:
  | UPDATE table_name SET assignment_list                         { Ast.Update ($2, $4, None, None, None) }
  | UPDATE table_name SET assignment_list option_list             { 
      let (ow, oo, ol) = $4 in Ast.Update ($2, $4, ow, oo, ol) 
    }
  ;

/* --- Tables, columns, values -------------------------------------------- */

table_name:
  | IDENTIFIER                              { Ast.Table $1 }
  ;

col_name:
  | IDENTIFIER                              { Ast.Col $1 }
  ;

value:
  | expr                                    { Ast.ValueExpr $1 }
  | DEFAULT                                 { Ast.Default }
  ;

/* --- Assignments: col = value, comma-separated, optionally parenthesized -- */

assignment_list:
  | assignment                               { [ $1 ] }
  | assignment_list COMMA assignment         { $1 @ [ $3 ] }
  | LPAREN assignment_list RPAREN            { $2 }
  ;

assignment:
  | col_name EQ value                        { Ast.Assign ($1, $3) }
  ;

/* --- Optional clauses packaged so all combinations are accepted ----------- */

option_list:
  | opt_where opt_order opt_limit           { ($1, $2, $3) }
  ;

opt_where:
  |                                         { None }
  | WHERE condition_list                    { Some (Ast.Where $2) }
  ;

opt_order:
  |                                         { None }
  | ORDER BY order_by_list                  { Some (Ast.OrderBy $3) }
  ;

opt_limit:
  |                                         { None }
  | LIMIT POSITIVE_DIGIT                    { Some (Ast.Limit $2) }
  ;

/* --- Conditions ----------------------------------------------------------- */

condition_list:
  | condition                               { $1 }                       /* start with a single condition */
  | condition_list OR condition             { Ast.Or  ($1, $3) }
  | condition_list AND condition            { Ast.And ($1, $3) }
  ;

condition:
  | assignment                               { Ast.CondAssign $1 }       /* e.g., col = expr usable in WHERE */
  | col_name COMPARISION_OPERATOR real_number { Ast.Compare ($2, $1, $3) }
  | col_name BETWEEN real_number AND real_number { Ast.Between ($1, $3, $5) }
  | LPAREN condition_list RPAREN            { $2 }
  ;

/* --- Order By ------------------------------------------------------------- */

order_by_list:
  | order_term                              { [ $1 ] }
  | order_by_list COMMA order_term          { $1 @ [ $3 ] }
  ;

order_term:
  | col_name                                { Ast.Order ($1, `Asc) }
  | col_name ASC                            { Ast.Order ($1, `Asc) }
  | col_name DESC                           { Ast.Order ($1, `Desc) }
  ;

/* --- Expressions ---------------------------------------------------------- */

expr:
  | expr PLUS expr                          { Ast.Plus  ($1, $3) }
  | expr MINUS expr                         { Ast.Minus ($1, $3) }
  | expr STAR expr                          { Ast.Star  ($1, $3) }
  | expr DIV  expr                          { Ast.Div   ($1, $3) }
  | real_number                             { $1 }
  | identifiers_strings                     { $1 }
  | LPAREN expr RPAREN                      { $2 }
  ;

identifiers_strings:
  | IDENTIFIER                              { Ast.Var   $1 }
  | STRING_LITERAL                          { Ast.Str   $1 }
  ;

real_number:
  | POSITIVE_DIGIT                          { Ast.Int   $1 }
  | NEGATIVE_DIGIT                          { Ast.Int   $1 }  /* assume lexer already parsed the sign */
  | FLOAT                                   { Ast.Float $1 }
  ;
