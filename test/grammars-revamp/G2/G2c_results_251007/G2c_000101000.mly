/* *** G1e *** */
// 9 po's 3 assoc's
// ~ vs. |
// & vs. |    // | vs. &
// -> vs. |
// | assoc
/* ---------- */ 
// ~ vs. &
// -> assoc
// & assoc
/* ---------- */ 
// ~ vs. ->
// & vs. ->
// -> vs. &
// | vs. ->


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

/* ---------------------------------------------------------------------- */
%start topl1
%type <Ast.bexp> topl1
%type <Ast.bexp> bexp4
%%

bexp1:
  | l=bexp1 ARR r=bexp2  { Imp(l, r) }
  | bexp2  { $1 }
  ;

bexp2:
  | bexp3  { $1 }
  | l=bexp3 AMPER r=bexp2  { And(l, r) }
  ;

bexp3:
  | bexp4  { $1 }
  | l=bexp3 BAR r=bexp4  { Or(l, r) }
  ;

bexp4:
  | x=VAR  { Var (snd x) }
  | TILDE b=bexp4  { Not(b) }
  | TRUE  { True }
  | FALSE  { False }
  | LPAREN b=bexp1 RPAREN  { b }
  ;

topl1:
  | b=bexp1 EOF  { b }
  ;
