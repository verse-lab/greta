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

program : lexpr EOF { $1 };

lexpr:     
  | lexpr AND lexpr { And($1, $3) }
  | lexpr OR lexpr { Or($1, $3) }
  | NOT lexpr { Not($2) }
  | VAR { Var }
  | LPAREN lexpr RPAREN { Paren($2) }
  ;

