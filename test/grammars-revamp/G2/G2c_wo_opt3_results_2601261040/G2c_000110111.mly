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
  | l=bexp1 AMPER r=bexp2  {  And(l, r)  }
  | l=bexp4 ARR r=bexp3  {  Imp(l, r)  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 BAR r=bexp2  {  Or(l, r)  }
  | TILDE b=bexp4  {  Not(b)  }
  ;

bexp2:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 BAR r=bexp2  {  Or(l, r)  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TILDE b=bexp4  {  Not(b)  }
  | l=bexp4 ARR r=bexp3  {  Imp(l, r)  }
  ;

bexp3:
  | TILDE b=bexp4  {  Not(b)  }
  | l=bexp4 ARR r=bexp3  {  Imp(l, r)  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TRUE  {  True  }
  | x=VAR  {  Var (snd x)  }
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
