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

x2:
  | TRUE                  { True }
  | FALSE                 { False }
  | TILDE b=x2         { Not(b) }
  | LPAREN b=e1 RPAREN { b }

x1:
  | x2                 { $1 }
  | l=x1 BAR r=x2   { Or(l, r) }
  | l=x1 ARR r=x2   { Imp(l, r) }
  | x=VAR                 { Var (snd x) }
  ;

e1:
  | x1                 { $1 }
  | l=e1 AMPER r=e1 { And(l, r) }  
  ;

