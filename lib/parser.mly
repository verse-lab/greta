// subset of C++ grammar to address review C's question
// Q: Can it be so one cannot disambiguate between the two parse trees 
// as the right disambiguation depends on the surrounding context?


%{
    open Ast;;
%}

%token <string> DECL (* literal word "decl" *)
%token <string> IDENT
%token LPAREN RPAREN EOF

%type <Ast.t> program
%start program
%%

program : decl EOF { $1 };

decl:
  | DECL ty id LPAREN expr RPAREN  { Decl1 ($2, $3, $5) }
  | DECL ty id LPAREN param RPAREN { Decl2 ($2, $3, $5) }
  ;

expr: 
  | id LPAREN RPAREN { Call $1 }
  ;

param:
  | ty LPAREN RPAREN { Param $1 }
  ; 

ty: 
  | IDENT { TyString $1 }
  ;

id: 
  | IDENT { IdString $1 }
  ;
