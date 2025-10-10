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

program : lexpr1 EOF { $1 };

lexpr1:     
  | lexpr1 AND lexpr2 { And($1, $3) }
  | lexpr2 { $1 }
  ;

lexpr2:
  | lexpr2 OR lexpr2 { Or($1, $3) }
  | NOT lexpr2 { Not($2) }
  | term { $1 }
  ;

term: 
  | TRUE { True }
  | FALSE { False }
  | LPAREN lexpr1 RPAREN { Paren($2) }
  ;