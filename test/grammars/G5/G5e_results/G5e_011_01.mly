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

e1:
  | x1 { $1 }
  | e1 OR x1 { Or($1, $3) }
  ;

x3:
  | FALSE { False }
  | LPAREN e1 RPAREN { Paren($2) }
  | TRUE { True }
  ;

x2:
  | x3 { $1 }
  | x2 AND x3 { And($1, $3) }
  ;

x1:
  | x2 { $1 }
  | NOT x1 { Not($2) }
  ;

