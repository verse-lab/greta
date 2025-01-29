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

x1:
  | LPAREN e1 RPAREN { Paren($2) }
  | x1 AND x1 { And($1, $3) }
  ;

e1:
  | x1 { $1 }
  | e1 OR x1 { Or($1, $3) }
  | NOT e1 { Not($2) }
  ;

