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
  | bexp4  { $1 }
  | l=bexp4 BAR r=bexp1  {  Or(l, r)  }
  ;

bexp3:
  | bexp5  { $1 }
  ;

bexp4:
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  | bexp7  { $1 }
  ;

bexp5:
  | bexp6  { $1 }
  ;

bexp6:
  | TILDE b=bexp3  {  Not(b)  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

bexp7:
  | bexp8  { $1 }
  | l=bexp8 AMPER r=bexp7  {  And(l, r)  }
  ;

bexp8:
  | x=VAR  {  Var (snd x)  }
  | bexp6  {  $1  }
  ;

topl1:
  | b=bexp2 EOF  {  b  }
  ;
