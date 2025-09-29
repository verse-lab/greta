/* *** G1d *** */
// 6 po's 3 assoc's
// & vs. |    // | vs. &
// -> vs. |   
// | assoc
/* ---------- */ 
// & assoc
// -> assoc
// | vs. ->    // & vs. ->   // -> vs. &
/* ---------- */ 

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
%type <Ast.bexp> x3
%%

toplevel:
  | b=e1 EOF { b }        

x3:
  | TRUE                  { True }
  | FALSE                 { False }
  | TILDE b=x3         { Not(b) }
  | LPAREN b=e1 RPAREN { b }

x2:
  | x3                 { $1 }
  | l=x2 ARR r=x3   { Imp(l, r) }
  | x=VAR                 { Var (snd x) }
  ;

x1:
  | l=x2 AMPER r=x1 { And(l, r) }  
  | x2                 { $1 }
  ;

e1:
  | l=x2 BAR r=e1   { Or(l, r) }
  | x1                 { $1 }
  ;

