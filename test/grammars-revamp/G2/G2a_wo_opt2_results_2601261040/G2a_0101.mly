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
%type <Ast.bexp> bexp7
%type <Ast.bexp> bexp4
%%

bexp1:
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  ;

bexp3:
  | bexp4  { $1 }
  | l=bexp4 ARR r=bexp1  {  Imp(l, r)  }
  ;

bexp4:
  | LPAREN b=bexp5 RPAREN  {  b  }
  | FALSE  {  False  }
  | TILDE b=bexp4  {  Not(b)  }
  | TRUE  {  True  }
  ;

bexp5:
  | bexp6  { $1 }
  ;

bexp6:
  | l=bexp5 AMPER r=bexp7  {  And(l, r)  }
  | bexp7  { $1 }
  ;

bexp7:
  | x=VAR  {  Var (snd x)  }
  | bexp3  {  $1  }
  | l=bexp7 BAR r=bexp2  {  Or(l, r)  }
  ;

topl1:
  | b=bexp6 EOF  {  b  }
  ;
