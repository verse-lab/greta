%{
open Ast
%}

%token EOF
%token LBRACKET
%token RBRACKET
%token QUESTION
%token ID
%token NUM
%token NUMID
%token WEB
%token SLASH
%token ASSIGNOP
%token QUOTE

%type <Ast.t> toplevel

%start toplevel
%%

toplevel: program EOF { $1 };

program: 
    | LBRACKET QUESTION ID attribute_list QUESTION RBRACKET { Bracket("?", $4, []) }
    | root { $1 }
    ;

root:
    | LBRACKET ID attribute_list RBRACKET node_list LBRACKET SLASH ID RBRACKET { Bracket("id", $3, $5) }
    ; 


node_list:
    | node_s { $1 }
    | ns=node_list n=node_s { ns@n }
    ; 

node_s:
    | node { $1 }
    | u_node { $1 }
    | ID { Id::[] }
    ;

node:
    | LBRACKET ID attribute_list SLASH RBRACKET { Bracket("id", $3, [])::[] }
    ;

u_node:
    | LBRACKET ID attribute_list RBRACKET node_list LBRACKET SLASH ID RBRACKET { Bracket("id", $3, $5)::[] }
    | LBRACKET ID attribute_list RBRACKET LBRACKET SLASH ID RBRACKET { Bracket("id", $3, [])::[] }
    ;

attribute_list:
    | attribute { $1 }
    | xs=attribute_list x=attribute { xs@x }
    ;

attribute:
    | ID ASSIGNOP QUOTE ID QUOTE { [Assign(Id, Id)] }
    | ID ASSIGNOP QUOTE NUM QUOTE { [Assign(Id, Num)] }
    | ID ASSIGNOP QUOTE NUMID QUOTE { [Assign(Id, Numid)] }
    | ID ASSIGNOP QUOTE WEB QUOTE { [Assign(Id, Web)] }
    ;
