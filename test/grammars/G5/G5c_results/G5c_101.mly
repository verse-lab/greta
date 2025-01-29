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

x4:
    | LPAREN e1 RPAREN { Paren($2) }
  ;

x3:
    | x4 { $1 }
    | x3 OR x4 { Or($1, $3) }
  ;

x2:
    | x3 { $1 }
  ;

x1:
    | x2 { $1 }
    | NOT x1 { Not($2) }
  ;

e1:
    | x2 AND e1 { And($1, $3) }
    | x1 { $1 }
  ;

