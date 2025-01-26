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
    | term { $1 }
    ;

term: 
    | NOT term { Not($2) }
    | x=VAR { Var (snd x)}
    | LPAREN lexpr RPAREN { Paren($2) }
    ;