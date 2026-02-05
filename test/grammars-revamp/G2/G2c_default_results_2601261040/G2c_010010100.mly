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
%type <Ast.bexp> bexp5
%%

bexp1:
  | l=bexp1 BAR r=bexp2  {  Or(l, r)  }
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  | l=bexp2 AMPER r=bexp3  {  And(l, r)  }
  ;

bexp3:
  | TILDE b=bexp3  {  Not(b)  }
  | bexp4  { $1 }
  ;

bexp4:
  | bexp5  { $1 }
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp5:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  ;

topl1:
  | b=bexp1 EOF  {  b  }
  ;
