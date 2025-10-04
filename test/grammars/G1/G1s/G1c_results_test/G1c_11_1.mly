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

e1:
  | x1                 { $1 }
  | l=e1 BAR r=x1   { Or(l, r) }
  ;

x2:
  | x3                 { $1 }
  | l=x2 AMPER r=x2 { And(l, r) }  
  | x=VAR                 { Var (snd x) }
  ;

x1:
  | x2                 { $1 }
  | l=x1 ARR r=x3   { Imp(l, r) }
  ;

