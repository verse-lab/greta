%{
  open Ast
%}

%token B
%token A

%type <Ast.cs3231> s

%start s
%%

s : 
  | B a { B1 ($2) }
  | A b { A1 ($2) }
  ;

a :
  | A { Atom "a" }
  | A s { A1 ($2) }
  | B a a { B2 ($2, $3) }
  ;

b :
  | B { Atom "b" }
  | B s { B1 ($2) }
  | A b b { A2 ($2, $3) }
  ;

