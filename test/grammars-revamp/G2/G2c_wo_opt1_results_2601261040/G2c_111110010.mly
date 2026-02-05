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
  | bexp2  { $1 }
  | TILDE b=bexp1  {  Not(b)  }
  ;

bexp2:
  | bexp3  { $1 }
  | l=bexp2 AMPER r=bexp3  {  And(l, r)  }
  ;

bexp3:
  | l=bexp4 ARR r=bexp3  {  Imp(l, r)  }
  | bexp4  { $1 }
  ;

bexp4:
  | bexp5  { $1 }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
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
