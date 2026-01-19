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
%type <Ast.bexp> bexp1
%type <Ast.bexp> bexp2
%%

toplevel:
  | b=bexp1 EOF { b }        

bexp1:
  | bexp2                 { $1 }
  | x=VAR                 { Var (snd x) }
  | l=bexp1 BAR r=bexp2   { Or(l, r) }
  | l=bexp1 AMPER r=bexp1 { And(l, r) }  

bexp2:
  | TRUE                  { True }
  | FALSE                 { False }
  | l=bexp2 ARR r=bexp2   { Imp(l, r) }
  | TILDE b=bexp2         { Not(b) }
  | LPAREN b=bexp1 RPAREN { b }
