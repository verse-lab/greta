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


%start toplevel           
%type <Ast.bexp> toplevel
%type <Ast.bexp> e1
%type <Ast.bexp> x2
%%

toplevel:
  | b=e1 EOF { b }        

x3:
  | FALSE                 { False }
  | LPAREN b=e1 RPAREN { b }
  | TILDE b=x3         { Not(b) }
  | TRUE                  { True }
  ;

x2:
  | x3                 { $1 }
  | l=x2 ARR r=x3   { Imp(l, r) }
  ;

x1:
  | x2                 { $1 }
  | l=x2 AMPER r=x1 { And(l, r) }  
  ;

e1:
  | x1                 { $1 }
  | l=e1 BAR r=x2   { Or(l, r) }
  | x=VAR                 { Var (snd x) }
  ;

