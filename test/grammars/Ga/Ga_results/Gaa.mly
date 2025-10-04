%{
    open Syntax
    open Ast_helper
    open MySupport
%}

%token PARAM 
%token STORAGE 
%token CODE 
%token LPAREN 
%token RPAREN 
%token LBRACE 
%token RBRACE 
%token SEMI 
%token EOF
%token UNIT
%token PAIR
%token LPAREN_PAIR 
%token LPAREN_LEFT 
%token LPAREN_RIGHT 
%token SOME 
%token NONE
%token ELT 
%token TYS

%token <string> INTV
%token <bool> BOOL
%token <string> STR
%token <string> MNEMONIC
%token <string> MNEMONIC_OTY
%token <string> MNEMONIC_TTY
%token <string> MNEMONIC_TYL

%token <string> LCID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel:
  | sc=e1 EOF { sc }

e1:
  | CODE LBRACE is=x3 RBRACE { Code (None, is) }
  | PARAM pty=x1 SEMI STORAGE stty=x1 SEMI CODE LBRACE is=x3 RBRACE { Code (Some (pty, stty), is) }

x2:
  | /* empty */ { [] }
  | TYS ty=x3 tyds=x2 { ty::tyds }

x1:
  | x3 { $1 }
  | LPAREN ty=LCID tail=x2 RPAREN { let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail }
  ;

