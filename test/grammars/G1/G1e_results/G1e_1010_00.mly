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

e1:
  | l=x1 BAR r=e1   { Or(l, r) }
  | x1 { $1 }
  | TILDE b=e1        { Not(b) }
  ;

x3:
  | FALSE               { False }
  | LPAREN b=e1 RPAREN { b }
  | TRUE                { True }
  | x=VAR               { Var (snd x) }
  ;

x2:
  | l=x3 ARR r=x2   { Imp(l, r) }
  | x3 { $1 }
  ;

x1:
  | x2 { $1 }
  | l=x1 AMPER r=x1 { And(l, r) }
  ;

