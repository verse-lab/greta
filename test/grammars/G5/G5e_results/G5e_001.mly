%{
    open Ast;;
%}

%token <Range.t * string> VAR
%token <Range.t> AND
%token <Range.t> OR 
%token <Range.t> NOT
%token <Range.t> LPAREN
%token <Range.t> RPAREN

%token EOF



%type <Ast.t> program
%start program 
%%

program : e1 EOF { $1 };

x2:
  | LPAREN e1 RPAREN { Paren($2) }
  | NOT x2 { Not($2) }
  ;

x1:
  | x2 { $1 }
  | x1 OR x2 { Or($1, $3) }
  ;

e1:
  | x1 { $1 }
  | e1 AND e1 { And($1, $3) }
  ;

