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
%token <Range.t> FALSE%start topl7
%type <Ast.bexp> topl7
%type <Ast.bexp> bexp6
%type <Ast.bexp> bexp4
%%

bexp1:
  | TILDE b=bexp1  {  Not(b)  }
  | bexp2  { $1 }
  ;

bexp2:
  | l=bexp2 ARR r=bexp4  {  Imp(l, r)  }
  | bexp4  { $1 }
  ;

bexp3:
  | l=bexp3 BAR r=bexp1  {  Or(l, r)  }
  | bexp5  { $1 }
  ;

bexp4:
  | LPAREN b=bexp3 RPAREN  {  b  }
  | FALSE  {  False  }
  | TRUE  {  True  }
  ;

bexp5:
  | bexp6  { $1 }
  | l=bexp5 AMPER r=bexp6  {  And(l, r)  }
  ;

bexp6:
  | bexp1  {  $1  }
  | x=VAR  {  Var (snd x)  }
  ;

topl7:
  | b=bexp3 EOF  {  b  }
  ;
