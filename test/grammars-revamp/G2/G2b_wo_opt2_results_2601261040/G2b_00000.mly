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
%type <Ast.bexp> bexp5
%%

bexp1:
  | bexp2  { $1 }
  ;

bexp2:
  | l=bexp1 ARR r=bexp5  {  Imp(l, r)  }
  | bexp4  { $1 }
  ;

bexp3:
  | bexp5  { $1 }
  ;

bexp4:
  | bexp6  { $1 }
  | l=bexp4 BAR r=bexp6  {  Or(l, r)  }
  ;

bexp5:
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TRUE  {  True  }
  | TILDE b=bexp3  {  Not(b)  }
  ;

bexp6:
  | l=bexp6 AMPER r=bexp7  {  And(l, r)  }
  | bexp7  { $1 }
  ;

bexp7:
  | x=VAR  {  Var (snd x)  }
  | bexp5  {  $1  }
  ;

topl1:
  | b=bexp2 EOF  {  b  }
  ;
