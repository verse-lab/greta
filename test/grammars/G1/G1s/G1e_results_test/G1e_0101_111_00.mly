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

x3:
  | FALSE               { False }
  | LPAREN b=e1 RPAREN { b }
  | TILDE b=x3        { Not(b) }
  | TRUE                { True }
  | x=VAR               { Var (snd x) }
  ;

x2:
  | x3 { $1 }
  | l=x3 AMPER r=x2 { And(l, r) }
  ;

x1:
  | l=x3 BAR r=x1   { Or(l, r) }
  | x2 { $1 }
  ;

e1:
  | x1 { $1 }
  | l=e1 ARR r=x2   { Imp(l, r) }
  ;

