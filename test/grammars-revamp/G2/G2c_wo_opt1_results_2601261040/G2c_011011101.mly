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
  | l=bexp2 BAR r=bexp1  {  Or(l, r)  }
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  | TILDE b=bexp2  {  Not(b)  }
  ;

bexp3:
  | bexp4  { $1 }
  | l=bexp4 AMPER r=bexp3  {  And(l, r)  }
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

topl6:
  | b=bexp1 EOF  {  b  }
  ;
