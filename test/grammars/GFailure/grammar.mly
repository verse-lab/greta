%{
  open Ast
%}

%token B
%token A

%type <Ast.ast> s

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
  | c { $1 }
  | B s { B1 ($2) }
  | A c b { A2 ($2, $3) }
  ;

c :
  | B { Atom "b" }
  | b { $1 }
  ;

