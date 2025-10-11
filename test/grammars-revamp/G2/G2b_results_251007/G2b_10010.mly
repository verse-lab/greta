/* *** G1c *** */
// 4 po's 2 assoc's
// & vs. |    // | vs. &
// | assoc    // | vs. ->
/* ---------- */ 
// & vs. ->   
/* ---------- */ 
// & assoc


%{
open Ast;;
%}

%token EOF
%token <Range.t * string> VAR
%token <Range.t> ARR      /* -> */
%token <Range.t> BAR      /* | */
%token <Range.t> AMPER    /* & */
%token <Range.t> LPAREN   /* ( */
%token <Range.t> RPAREN   /* ) */
%token <Range.t> TILDE    /* ~ */
%token <Range.t> TRUE     /* true */
%token <Range.t> FALSE    /* false */

%start topl1
%type <Ast.bexp> topl1
%type <Ast.bexp> bexp5
%type <Ast.bexp> bexp2
%%

bexp1:
  | l=bexp1 ARR r=bexp2  { Imp(l, r) }
  | bexp3  { $1 }
  ;

bexp2:
  | TRUE  { True }
  | LPAREN b=bexp1 RPAREN  { b }
  | TILDE b=bexp2  { Not(b) }
  | FALSE  { False }
  ;

bexp3:
  | bexp4  { $1 }
  | l=bexp3 AMPER r=bexp4  { And(l, r) }
  ;

bexp4:
  | bexp5  { $1 }
  | l=bexp5 BAR r=bexp4  { Or(l, r) }
  ;

bexp5:
  | x=VAR  { Var (snd x) }
  | bexp2  { $1 }
  ;

topl1:
  | b=bexp1 EOF  { b }
  ;
