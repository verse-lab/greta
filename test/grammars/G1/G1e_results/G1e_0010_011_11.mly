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
  ;

x4:
  | FALSE               { False }
  | LPAREN b=e1 RPAREN { b }
  | TRUE                { True }
  | x=VAR               { Var (snd x) }
  ;

x3:
  | x4 { $1 }
  | l=x3 AMPER r=x4 { And(l, r) }
  ;

x2:
  | x3 { $1 }
  | l=x2 ARR r=x3   { Imp(l, r) }
  ;

x1:
  | x2 { $1 }
  | TILDE b=x1        { Not(b) }
  ;

