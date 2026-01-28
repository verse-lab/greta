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
%type <Ast.bexp> bexp3
%%

bexp1:
  | bexp2  { $1 }
  ;

bexp2:
  | l=bexp3 ARR r=bexp1  {  Imp(l, r)  }
  | bexp3  { $1 }
  ;

bexp3:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp4 RPAREN  {  b  }
  | TILDE b=bexp3  {  Not(b)  }
  ;

bexp4:
  | bexp5  { $1 }
  ;

bexp5:
  | bexp6  { $1 }
  | l=bexp4 BAR r=bexp2  {  Or(l, r)  }
  ;

bexp6:
  | l=bexp7 AMPER r=bexp6  {  And(l, r)  }
  | bexp7  { $1 }
  ;

bexp7:
  | x=VAR  {  Var (snd x)  }
  | bexp2  {  $1  }
  ;

topl1:
  | b=bexp5 EOF  {  b  }
  ;
