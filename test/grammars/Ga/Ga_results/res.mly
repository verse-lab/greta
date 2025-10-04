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
%token LIT
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
  | PARAM pty=x7 SEMI STORAGE stty=x7 SEMI CODE LBRACE is=x3 RBRACE { Code (Some (pty, stty), is) }

x7 :
  | LPAREN ty=LCID tail=tys RPAREN { let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail }
tys :
  | /* empty */ { [] }
  | ty=x7 tyds=tys { ty::tyds }

literal:
  | const { $1 }
  | UNIT { Exp.tuple [] }
  | LBRACE kvs=kvlists RBRACE { Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs] }
  | NONE { Exp.construct (Location.mknoloc (Longident.Lident "None")) None }
  | SOME l=literal { Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l) }
  | LPAREN_LEFT l=literal RPAREN { Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l) }
  | LPAREN_RIGHT l=literal RPAREN { Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l) }
  | l1=literal ELT  l2=literal { Exp.tuple [l1; l2] }
  | LPAREN PAIR a=literal b=literal RPAREN { Exp.tuple [a; b] }
  | PAIR a=literal b=literal { Exp.tuple [a; b] }

const:
  | s=STR { Exp.constant (Const.string s) }
  | i=INTV { Exp.constant (Const.int (int_of_string i)) }
  | b=BOOL { Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None }
  | LPAREN x=const RPAREN { x }

kvlists:
  | ELT l1=literal l2=literal SEMI { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None])) }
  | ELT l1=literal l2=literal SEMI ls=kvlists { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls])) }

singleinst:
  | m=MNEMONIC { Simple m }
  | m=MNEMONIC_OTY x7 { Simple m }
  | m=MNEMONIC_TTY x7 x7 { Simple m }
  | m=MNEMONIC_TYL x7 l=literal { SimpleArgCon (m, l) }

x3:
  | i=singleinst { [ i ] }
