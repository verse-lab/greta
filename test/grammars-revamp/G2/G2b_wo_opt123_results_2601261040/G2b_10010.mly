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
%token <Range.t> FALSE%start topl12
%type <Ast.bexp> topl12
%type <Ast.bexp> bexp9
%type <Ast.bexp> bexp8
%%

bexp1:
  | bexp8  {  $1  }
  | l=bexp2 ARR r=bexp8  {  Imp(l, r)  }
  | l=bexp9 AMPER r=bexp10  {  And(l, r)  }
  | l=bexp11 BAR r=bexp10  {  Or(l, r)  }
  | x=VAR  {  Var (snd x)  }
  ;

bexp10:
  | x=VAR  {  Var (snd x)  }
  | l=bexp11 BAR r=bexp10  {  Or(l, r)  }
  | bexp8  {  $1  }
  ;

bexp11:
  | x=VAR  {  Var (snd x)  }
  | bexp8  {  $1  }
  ;

bexp2:
  | x=VAR  {  Var (snd x)  }
  | l=bexp2 ARR r=bexp8  {  Imp(l, r)  }
  | bexp8  {  $1  }
  | l=bexp11 BAR r=bexp10  {  Or(l, r)  }
  | l=bexp9 AMPER r=bexp10  {  And(l, r)  }
  ;

bexp3:
  | TILDE b=bexp6  {  Not(b)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  ;

bexp4:
  | TILDE b=bexp6  {  Not(b)  }
  | FALSE  {  False  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | TRUE  {  True  }
  ;

bexp5:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | TILDE b=bexp6  {  Not(b)  }
  ;

bexp6:
  | TRUE  {  True  }
  | TILDE b=bexp6  {  Not(b)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | FALSE  {  False  }
  ;

bexp7:
  | LPAREN b=bexp2 RPAREN  {  b  }
  | TRUE  {  True  }
  | TILDE b=bexp6  {  Not(b)  }
  | FALSE  {  False  }
  ;

bexp8:
  | TILDE b=bexp6  {  Not(b)  }
  | LPAREN b=bexp2 RPAREN  {  b  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

bexp9:
  | x=VAR  {  Var (snd x)  }
  | l=bexp9 AMPER r=bexp10  {  And(l, r)  }
  | l=bexp11 BAR r=bexp10  {  Or(l, r)  }
  | bexp8  {  $1  }
  ;

topl12:
  | b=bexp1 EOF  {  b  }
  ;
