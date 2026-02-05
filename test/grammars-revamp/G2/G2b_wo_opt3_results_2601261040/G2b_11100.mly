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
%type <Ast.bexp> bexp3
%%

bexp1:
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  | bexp3  {  $1  }
  | l=bexp2 ARR r=bexp3  {  Imp(l, r)  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp2 AMPER r=bexp1  {  And(l, r)  }
  ;

bexp2:
  | x=VAR  {  Var (snd x)  }
  | l=bexp2 ARR r=bexp3  {  Imp(l, r)  }
  | bexp3  {  $1  }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  ;

bexp3:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TILDE b=bexp3  {  Not(b)  }
  ;

bexp4:
  | bexp3  {  $1  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  ;

bexp5:
  | x=VAR  {  Var (snd x)  }
  | bexp3  {  $1  }
  ;

topl1:
  | b=bexp1 EOF  {  b  }
  ;
