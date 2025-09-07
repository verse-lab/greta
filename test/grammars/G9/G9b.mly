// subset of C++ grammar to address review C's question
// Q: Can it be so one cannot disambiguate between the two parse trees 
// as the right disambiguation depends on the surrounding context?


%{
    open Ast;;
%}

%token <string> TYPE (* "Foo" or "Bar" *)
%token <string> ID (* any identifier, eg, "f" or "Bar" *)
%token DECL
%token LPAREN
%token RPAREN

%token EOF

%type <Ast.t> program
%start program
%%

program : decl EOF { $1 };

decl:
  | DECL ty id LPAREN expr RPAREN  { Decl1 ($2, $3, $5) }
  | DECL ty id LPAREN param RPAREN { Decl2 ($2, $3, $5) }
  ;

expr: 
  | id LPAREN RPAREN { Call($1) }
  ;

param:
  | ty LPAREN RPAREN { Param($1) }
  ; 

ty: 
  | TYPE { TyString($1) }
  ;

id: 
  | ID { IdString($1) }
  ;
