%{
    open Ast;;
%}

%token AND
%token OR 
%token NOT
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN

%token EOF




%type <Ast.t> program
%start program
%%

program : e1 EOF { $1 };

x2:
  | FALSE { False }
  | LPAREN e1 RPAREN { Paren($2) }
  | NOT x2 { Not($2) }
  | TRUE { True }
  ;

x1:
  | x2 { $1 }
  | x1 OR x2 { Or($1, $3) }
  ;

e1:
  | x1 { $1 }
  | x1 AND e1 { And($1, $3) }
  ;

