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
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  | FALSE  {  False  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
  | TILDE b=bexp5  {  Not(b)  }
  | l=bexp2 AMPER r=bexp3  {  And(l, r)  }
  | l=bexp3 BAR r=bexp4  {  Or(l, r)  }
  ;

bexp2:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | TILDE b=bexp5  {  Not(b)  }
  | x=VAR  {  Var (snd x)  }
  | l=bexp3 BAR r=bexp4  {  Or(l, r)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | l=bexp2 AMPER r=bexp3  {  And(l, r)  }
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp3:
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  | FALSE  {  False  }
  | TILDE b=bexp5  {  Not(b)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | TRUE  {  True  }
  | l=bexp3 BAR r=bexp4  {  Or(l, r)  }
  | x=VAR  {  Var (snd x)  }
  ;

bexp4:
  | x=VAR  {  Var (snd x)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | TILDE b=bexp5  {  Not(b)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | l=bexp4 ARR r=bexp5  {  Imp(l, r)  }
  ;

bexp5:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | TILDE b=bexp5  {  Not(b)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | x=VAR  {  Var (snd x)  }
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
