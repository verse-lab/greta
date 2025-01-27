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
    | x=VAR { Var (snd x)}
    | LPAREN lexpr1 RPAREN { Paren($2) }
    ;