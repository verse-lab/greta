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
  | b=e1 EOF { b }        

e1:
  | l=e1 BAR r=x2   { Or(l, r) }
  | x1                 { $1 }
  ;

x1:
  | x=VAR                 { Var (snd x) }
  | l=x1 AMPER r=x1       { And(l, r) }  
  | x2                    { $1 }
  ;

x2: 
  | l=x3 ARR r=x2         { Imp(l, r) }
  | x3                    { $1 }
  ;

x3:
  | TRUE                  { True }
  | FALSE                 { False }
  | TILDE b=x3            { Not(b) }
  | LPAREN b=e1 RPAREN    { b }
  ;
