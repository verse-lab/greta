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
  | e1 AND x1 { And($1, $3) }
  | x1 { $1 }
  ;

x3: 
  | TRUE { True }
  | FALSE { False }
  | LPAREN e1 RPAREN { Paren($2) }
  ;

x2:
  | x3 { $1 }
  | x2 OR x3 { Or($1, $3) }
  ;

x1:
  | x2 { $1 }
  | NOT x1 { Not($2) }
  ;

