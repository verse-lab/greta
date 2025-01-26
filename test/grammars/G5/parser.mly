%{
    open Ast
%}

%token EOF
%token AND
%token OR 
%token NOT


%token <Ast.t> program 
%start program 
%% 

program : lexpr EOF { $1 };

lexpr :     
    | lexpr AND lexpr { And($1, $3) }
    | lexpr OR lexpr { Or($1, $3) }
    | NOT lexpr { Not($2) }
    | term

term : 
    | VAR { Var("asd") }
    | LPAREN lexpr RPAREN { Paren($2) }