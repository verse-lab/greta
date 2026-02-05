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
%type <Ast.bexp> bexp6
%%

bexp1:
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  | l=bexp1 AMPER r=bexp3  {  And(l, r)  }
  ;

bexp3:
  | TILDE b=bexp3  {  Not(b)  }
  | bexp4  { $1 }
  ;

bexp4:
  | bexp5  { $1 }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  ;

bexp5:
  | l=bexp6 ARR r=bexp5  {  Imp(l, r)  }
  | bexp6  { $1 }
  ;

bexp6:
  | TRUE  {  True  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  | FALSE  {  False  }
  ;

topl1:
  | b=bexp2 EOF  {  b  }
  ;
