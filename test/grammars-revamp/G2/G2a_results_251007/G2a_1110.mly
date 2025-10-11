/* *** G1b *** */
// 2 po's 2 assoc's
// ~ vs. ->
// & vs. |
// & assoc
// -> assoc

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
%type <Ast.bexp> bexp4
%%

bexp1:
  | TILDE b=bexp1  { Not(b) }
  | bexp2  { $1 }
  ;

bexp2:
  | l=bexp2 ARR r=bexp4  { Imp(l, r) }
  | bexp4  { $1 }
  ;

bexp3:
  | bexp5  { $1 }
  | l=bexp5 AMPER r=bexp3  { And(l, r) }
  ;

bexp4:
  | LPAREN b=bexp3 RPAREN  { b }
  | FALSE  { False }
  | TRUE  { True }
  ;

bexp5:
  | bexp1  { $1 }
  | x=VAR  { Var (snd x) }
  | l=bexp5 BAR r=bexp1  { Or(l, r) }
  ;

topl1:
  | b=bexp3 EOF  { b }
  ;
