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


%start toplevel           
%type <Ast.bexp> toplevel
%type <Ast.bexp> e1
%%

toplevel:
  | b=e1 EOF { b }        

x2:
  | FALSE               { False }
  | LPAREN b=e1 RPAREN { b }
  | TILDE b=x2        { Not(b) }
  | TRUE                { True }
  | x=VAR               { Var (snd x) }
  ;

x1:
  | x2 { $1 }
  | l=x1 BAR r=x2   { Or(l, r) }
  ;

e1:
  | x1 { $1 }
  | l=e1 ARR r=e1   { Imp(l, r) }
  | l=e1 AMPER r=e1 { And(l, r) }
  ;

