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
%type <Ast.bexp> bexp4
%%

bexp1:
  | TRUE  {  True  }
  | l=bexp4 AMPER r=bexp3  {  And(l, r)  }
  | l=bexp2 ARR r=bexp3  {  Imp(l, r)  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp1 BAR r=bexp2  {  Or(l, r)  }
  | TILDE b=bexp4  {  Not(b)  }
  ;

bexp2:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | x=VAR  {  Var (snd x)  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | l=bexp4 AMPER r=bexp3  {  And(l, r)  }
  | TILDE b=bexp4  {  Not(b)  }
  | l=bexp2 ARR r=bexp3  {  Imp(l, r)  }
  ;

bexp3:
  | TILDE b=bexp4  {  Not(b)  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TRUE  {  True  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp4 AMPER r=bexp3  {  And(l, r)  }
  ;

bexp4:
  | x=VAR  {  Var (snd x)  }
  | TILDE b=bexp4  {  Not(b)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  ;

topl1:
  | b=bexp1 EOF  {  b  }
  ;
