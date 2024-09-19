%{
open Ast;;
%}

/* Declare your tokens here. */

/* menhir uses this declaration to automatically generate
 * a token datatype.
 * Each token carries a Range.t value 
 */

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

/* Mark 'toplevel' as a starting nonterminal of the grammar */
%start toplevel           

/* Define type annotations for toplevel and bexp */
%type <Ast.bexp> toplevel  
%type <Ast.bexp> bexp
%%

toplevel:
  | b=bexp EOF { b }        

bexp:
  | TRUE                { True }
  | FALSE               { False }
  | x=VAR               { Var (snd x) }
  | l=bexp ARR r=bexp   { Imp(l, r) }
  | l=bexp BAR r=bexp   { Or(l, r) }
  | l=bexp AMPER r=bexp { And(l, r) }
  | TILDE b=bexp        { Not(b) }
  | LPAREN b=bexp RPAREN { b }