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
  | TILDE b=bexp1  {  Not(b)  }
  ;

bexp3:
  | bexp4  { $1 }
  | l=bexp4 BAR r=bexp3  {  Or(l, r)  }
  ;

bexp4:
  | bexp5  { $1 }
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp5:
  | l=bexp6 AMPER r=bexp5  {  And(l, r)  }
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
