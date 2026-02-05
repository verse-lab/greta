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
%type <Ast.bexp> bexp8
%type <Ast.bexp> bexp9
%%

bexp1:
  | TRUE  {  True  }
  | FALSE  {  False  }
  | TILDE b=bexp6  {  Not(b)  }
  | l=bexp3 ARR r=bexp6  {  Imp(l, r)  }
  | LPAREN b=bexp8 RPAREN  {  b  }
  ;

bexp10:
  | l=bexp10 BAR r=bexp1  {  Or(l, r)  }
  | x=VAR  {  Var (snd x)  }
  | bexp5  {  $1  }
  ;

bexp11:
  | x=VAR  {  Var (snd x)  }
  | bexp5  {  $1  }
  ;

bexp2:
  | TRUE  {  True  }
  | l=bexp3 ARR r=bexp6  {  Imp(l, r)  }
  | FALSE  {  False  }
  | TILDE b=bexp6  {  Not(b)  }
  | LPAREN b=bexp8 RPAREN  {  b  }
  ;

bexp3:
  | LPAREN b=bexp8 RPAREN  {  b  }
  | l=bexp3 ARR r=bexp6  {  Imp(l, r)  }
  | TILDE b=bexp6  {  Not(b)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

bexp4:
  | l=bexp3 ARR r=bexp6  {  Imp(l, r)  }
  | TILDE b=bexp6  {  Not(b)  }
  | LPAREN b=bexp8 RPAREN  {  b  }
  | FALSE  {  False  }
  | TRUE  {  True  }
  ;

bexp5:
  | l=bexp3 ARR r=bexp6  {  Imp(l, r)  }
  | TILDE b=bexp6  {  Not(b)  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  | LPAREN b=bexp8 RPAREN  {  b  }
  ;

bexp6:
  | TRUE  {  True  }
  | TILDE b=bexp6  {  Not(b)  }
  | LPAREN b=bexp8 RPAREN  {  b  }
  | FALSE  {  False  }
  ;

bexp7:
  | l=bexp10 AMPER r=bexp8  {  And(l, r)  }
  | x=VAR  {  Var (snd x)  }
  | bexp5  {  $1  }
  | l=bexp10 BAR r=bexp1  {  Or(l, r)  }
  ;

bexp8:
  | l=bexp10 BAR r=bexp1  {  Or(l, r)  }
  | l=bexp10 AMPER r=bexp8  {  And(l, r)  }
  | x=VAR  {  Var (snd x)  }
  | bexp5  {  $1  }
  ;

bexp9:
  | LPAREN b=bexp8 RPAREN  {  b  }
  | TRUE  {  True  }
  | FALSE  {  False  }
  ;

topl12:
  | b=bexp7 EOF  {  b  }
  ;
