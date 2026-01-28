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
%type <Ast.bexp> bexp5
%type <Ast.bexp> bexp2
%%

bexp1:
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  | bexp2  {  $1  }
  | l=bexp1 ARR r=bexp2  {  Imp(l, r)  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 AMPER r=bexp4  {  And(l, r)  }
  ;

bexp2:
  | TRUE  {  True  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TILDE b=bexp2  {  Not(b)  }
  | FALSE  {  False  }
  ;

bexp3:
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  | bexp2  {  $1  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 AMPER r=bexp4  {  And(l, r)  }
  ;

bexp4:
  | bexp2  {  $1  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  ;

bexp5:
  | x=VAR  {  Var (snd x)  }
  | bexp2  {  $1  }
  ;

topl1:
  | b=bexp1 EOF  {  b  }
  ;
