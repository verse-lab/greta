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

x3:
  | LPAREN e1 RPAREN { Paren($2) }
  | NOT x3 { Not($2) }
  | VAR { Var }
  ;

x2:
  | x3 OR x2 { Or($1, $3) }
  | x3 { $1 }
  ;

x1:
  | x2 { $1 }
  ;

e1:
  | x1 AND e1 { And($1, $3) }
  | x1 { $1 }
  ;

