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
  | bexp2  { $1 }
  | TILDE b=bexp1  {  Not(b)  }
  ;

bexp2:
  | bexp3  { $1 }
  | l=bexp3 ARR r=bexp2  {  Imp(l, r)  }
  ;

bexp3:
  | bexp4  { $1 }
  | l=bexp3 AMPER r=bexp4  {  And(l, r)  }
  ;

bexp4:
  | bexp5  { $1 }
  | l=bexp5 BAR r=bexp4  {  Or(l, r)  }
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
