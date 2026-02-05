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
%%

bexp1:
  | TRUE  {  True  }
  | l=bexp3 AMPER r=bexp4  {  And(l, r)  }
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 BAR r=bexp2  {  Or(l, r)  }
  | TILDE b=bexp1  {  Not(b)  }
  ;

bexp2:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 BAR r=bexp2  {  Or(l, r)  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | l=bexp3 AMPER r=bexp4  {  And(l, r)  }
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp3:
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | TRUE  {  True  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 AMPER r=bexp4  {  And(l, r)  }
  ;

bexp4:
  | x=VAR  {  Var (snd x)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp5:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp1 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  ;

topl1:
  | b=bexp1 EOF  {  b  }
  ;
