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
%token <Range.t> FALSE%start topl6
%type <Ast.bexp> topl6
%type <Ast.bexp> bexp5
%%

bexp1:
  | l=bexp1 AMPER r=bexp2  {  And(l, r)  }
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  | l=bexp2 BAR r=bexp3  {  Or(l, r)  }
  ;

bexp3:
  | l=bexp3 ARR r=bexp4  {  Imp(l, r)  }
  | bexp4  { $1 }
  ;

bexp4:
  | TILDE b=bexp4  {  Not(b)  }
  | bexp5  { $1 }
  ;

bexp5:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  ;

topl6:
  | b=bexp1 EOF  {  b  }
  ;
