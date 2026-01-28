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
%%

bexp1:
  | l=bexp1 AMPER r=bexp2  {  And(l, r)  }
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  | l=bexp2 ARR r=bexp3  {  Imp(l, r)  }
  ;

bexp3:
  | bexp4  { $1 }
  | l=bexp4 BAR r=bexp3  {  Or(l, r)  }
  ;

bexp4:
  | x=VAR  {  Var (snd x)  }
  | TILDE b=bexp4  {  Not(b)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  ;

topl1:
  | b=bexp1 EOF  {  b  }
  ;
