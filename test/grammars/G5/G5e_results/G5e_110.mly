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

x1:
  | FALSE { False }
  | LPAREN e1 RPAREN { Paren($2) }
  | TRUE { True }
  | x1 AND x1 { And($1, $3) }
  ;

e1:
  | x1 OR e1 { Or($1, $3) }
  | x1 { $1 }
  | NOT e1 { Not($2) }
  ;

