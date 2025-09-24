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
%token LEFT 
%token RIGHT 
%token SOME 
%token NONE 
%token ELT

%token <string> INTV
%token <bool> BOOL
%token <string> STR
%token <string> MNEMONIC
%token <string> LCID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
  | sc=script EOF { sc }

script :
  | CODE LBRACE is=instlist RBRACE { Code (None, is) }
  | PARAM pty=tyy SEMI STORAGE stty=tyy SEMI CODE LBRACE is=instlist RBRACE { Code (Some (pty, stty), is) }

tyy :
  | ty=LCID { Typ.constr (Location.mknoloc (Longident.Lident ty)) [] }
  | LPAREN ty=LCID tyds=tys RPAREN { let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tyds }
tys :
  | /* empty */ { [] }
  | ty=tyy tyds=tys { ty::tyds }

literal :
  | s=STR { Exp.constant (Const.string s) }
  | i=INTV { Exp.constant (Const.int (int_of_string i)) }
  | b=BOOL { Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None }
  | UNIT { Exp.tuple [] }
  | LBRACE l=semilits RBRACE { l }  /* list/set literal; pair could be of the same form but we ignore */
  | LBRACE kvs=kvlists RBRACE { Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs] }
  | NONE { Exp.construct (Location.mknoloc (Longident.Lident "None")) None }
  | SOME l=literal { Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l) }
  | LPAREN LEFT l=literal RPAREN { Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l) }
  | LPAREN RIGHT l=literal RPAREN { Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l) }
  | LPAREN PAIR ls=lits RPAREN { ls }

semilits :
  | /* empty */ { Exp.construct (Location.mknoloc (Longident.Lident "[]")) None }
  | l=literal { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None])) }
  | l=literal SEMI ls=semilits { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls])) }

optsemi : 
  | /* empty */ { () } 
  | SEMI { () }

kvlists : /* Cannot be empty to distinguish from the empty list */
  | ELT l1=literal l2=literal optsemi { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None])) }
  | ELT l1=literal l2=literal SEMI ls=kvlists { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls])) }

lits :
  | l1=literal l2=literal { Exp.tuple [l1; l2] }
  | l=literal ls=lits { Exp.tuple [l; ls] }

singleinst :
  | m=MNEMONIC { Simple m }
  | m=MNEMONIC tyy { Simple m }
  | m=MNEMONIC tyy tyy { Simple m }
  | m=MNEMONIC tyy l=literal { SimpleArgCon (m, l) }
  | m=MNEMONIC i=INTV { SimpleWithNum (m, int_of_string i) }
  | m=MNEMONIC LBRACE is=instlist RBRACE { OneBlock (m, is) }
  | m=MNEMONIC ty1=tyy ty2=tyy LBRACE is=instlist RBRACE { OneBlockWithTwoTys (m, ty1, ty2, is) }
  | m=MNEMONIC i=INTV LBRACE is=instlist RBRACE { OneBlockWithNum (m, int_of_string i, is) }
  | m=MNEMONIC LBRACE is1=instlist RBRACE LBRACE is2=instlist RBRACE { TwoBlocks (m, is1, is2) }
  | m=MNEMONIC LBRACE sc=script RBRACE { CreateContract (m, sc) }
  | LBRACE is=instlist RBRACE { Block is }
  | m=MNEMONIC error { prerr_string m; exit 1 }

instlist :
  | /* empty */ { [] }
  | i=singleinst { [ i ] }
  | i=singleinst SEMI is=instlist { i :: is }