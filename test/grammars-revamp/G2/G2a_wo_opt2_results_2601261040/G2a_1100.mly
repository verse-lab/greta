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
%type <Ast.bexp> bexp7
%%

bexp1:
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  ;

bexp3:
  | bexp4  { $1 }
  | TILDE b=bexp1  {  Not(b)  }
  ;

bexp4:
  | bexp7  { $1 }
  | l=bexp4 ARR r=bexp7  {  Imp(l, r)  }
  ;

bexp5:
  | bexp6  { $1 }
  ;

bexp6:
  | l=bexp5 AMPER r=bexp8  {  And(l, r)  }
  | bexp8  { $1 }
  ;

bexp7:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp5 RPAREN  {  b  }
  ;

bexp8:
  | x=VAR  {  Var (snd x)  }
  | bexp3  {  $1  }
  | l=bexp8 BAR r=bexp2  {  Or(l, r)  }
  ;

topl1:
  | b=bexp6 EOF  {  b  }
  ;
