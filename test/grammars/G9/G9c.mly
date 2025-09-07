// this is to try out Review c's question (#5)
// reg (op (op _ _) _) vs (op _ (op _ _)) 
// and (op1 (op2 _) _) vs (op2 _ (op1 _))

%{
    open Ast;;
%}

%token <string> VAR
%token ARROW
%token LPAREN
%token RPAREN

%token EOF

%type <Ast.expr> program

%start program
%%

program : expr EOF { $1 };

expr:
  | VAR { EVar $1 } 
  | expr ARROW expr { EArrow ($1, $3) }
  | expr ARROW expr ARROW expr ARROW expr ARROW expr { EChain ($1, $3, $5, $7, $9) } 
  | LPAREN expr RPAREN { $2 }
  ;

