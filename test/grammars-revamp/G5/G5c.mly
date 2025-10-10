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

program : lexpr EOF { $1 };

lexpr:     
  | lexpr AND lexpr { And($1, $3) }
  | lexpr OR lexpr { Or($1, $3) }
  | NOT lexpr { Not($2) }
  | TRUE { True }
  | FALSE { False }
  | LPAREN lexpr RPAREN { Paren($2) }
  ;

