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
%%

bexp1:
  | TRUE  {  True  }
  | l=bexp6 ARR r=bexp5  {  Imp(l, r)  }
  | FALSE  {  False  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  | TILDE b=bexp2  {  Not(b)  }
  | l=bexp4 AMPER r=bexp3  {  And(l, r)  }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  ;

bexp2:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | TILDE b=bexp2  {  Not(b)  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | l=bexp4 AMPER r=bexp3  {  And(l, r)  }
  | l=bexp6 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp3:
  | l=bexp6 ARR r=bexp5  {  Imp(l, r)  }
  | FALSE  {  False  }
  | l=bexp4 AMPER r=bexp3  {  And(l, r)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | TRUE  {  True  }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  | x=VAR  {  Var (snd x)  }
  ;

bexp4:
  | x=VAR  {  Var (snd x)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | l=bexp4 BAR r=bexp5  {  Or(l, r)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | l=bexp6 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp5:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp6 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp6:
  | x=VAR  {  Var (snd x)  }
  | TRUE  {  True  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | FALSE  {  False  }
  ;

topl7:
  | b=bexp1 EOF  {  b  }
  ;
