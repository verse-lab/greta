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
  | NOT x2 { Not($2) }
  | TRUE { True }
  | FALSE { False }
  | LPAREN e1 RPAREN { Paren($2) }
  ;

x1:
  | x2 OR x1 { Or($1, $3) }
  | x2 { $1 }
  ;

e1:
  | x1 { $1 }
  | e1 AND e1 { And($1, $3) }
  ;

