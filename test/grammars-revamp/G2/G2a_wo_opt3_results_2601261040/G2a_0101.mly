%{

open Ast;;

%}
%token EOF
%token <Range.t * string> VAR
%token <Range.t> ARR
%token <Range.t> BAR
%token <Range.t> AMPER
%token <Range.t> LPAREN
%token <Range.t> RPAREN
%token <Range.t> TILDE
%token <Range.t> TRUE
%token <Range.t> FALSE%start topl1
%type <Ast.bexp> topl1
%type <Ast.bexp> bexp4
%type <Ast.bexp> bexp2
%%

bexp1:
  | TILDE b=bexp2  {  Not(b)  }
  | TRUE  {  True  }
  | l=bexp2 ARR r=bexp1  {  Imp(l, r)  }
  | LPAREN b=bexp3 RPAREN  {  b  }
  | FALSE  {  False  }
  ;

bexp2:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | TILDE b=bexp2  {  Not(b)  }
  | LPAREN b=bexp3 RPAREN  {  b  }
  ;

bexp3:
  | x=VAR  {  Var (snd x)  }
  | l=bexp4 BAR r=bexp1  {  Or(l, r)  }
  | bexp1  {  $1  }
  | l=bexp3 AMPER r=bexp4  {  And(l, r)  }
  ;

bexp4:
  | x=VAR  {  Var (snd x)  }
  | bexp1  {  $1  }
  | l=bexp4 BAR r=bexp1  {  Or(l, r)  }
  ;

topl1:
  | b=bexp3 EOF  {  b  }
  ;
