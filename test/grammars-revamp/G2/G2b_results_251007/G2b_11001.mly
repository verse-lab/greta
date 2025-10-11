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
%type <Ast.bexp> bexp4
%type <Ast.bexp> bexp3
%%

bexp1:
  | l=bexp1 AMPER r=bexp2  { And(l, r) }
  | bexp2  { $1 }
  ;

bexp2:
  | bexp4  { $1 }
  | l=bexp2 BAR r=bexp4  { Or(l, r) }
  ;

bexp3:
  | TRUE  { True }
  | FALSE  { False }
  | LPAREN b=bexp1 RPAREN  { b }
  | TILDE b=bexp3  { Not(b) }
  ;

bexp4:
  | l=bexp4 ARR r=bexp3  { Imp(l, r) }
  | bexp3  { $1 }
  | x=VAR  { Var (snd x) }
  ;

topl1:
  | b=bexp1 EOF  { b }
  ;
