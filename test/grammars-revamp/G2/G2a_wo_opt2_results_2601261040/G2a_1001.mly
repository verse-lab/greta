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
%type <Ast.bexp> bexp8
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
  | bexp6  { $1 }
  | l=bexp6 ARR r=bexp3  {  Imp(l, r)  }
  ;

bexp4:
  | bexp5  { $1 }
  ;

bexp5:
  | bexp7  { $1 }
  | l=bexp4 BAR r=bexp2  {  Or(l, r)  }
  ;

bexp6:
  | LPAREN b=bexp4 RPAREN  {  b  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

bexp7:
  | bexp8  { $1 }
  | l=bexp7 AMPER r=bexp8  {  And(l, r)  }
  ;

bexp8:
  | x=VAR  {  Var (snd x)  }
  | bexp2  {  $1  }
  ;

topl1:
  | b=bexp5 EOF  {  b  }
  ;
