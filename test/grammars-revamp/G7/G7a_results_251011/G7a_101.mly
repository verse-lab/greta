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
%token IF
%token IF_LEFT 
%token IF_RIGHT 
%token IF_NONE
%token AT
%token PCT 
%token COLON

%token LOOP
%token LOOP_LEFT
%token ITER 
%token MAP
%token LAMBDA
%token EXEC
%token DIP

%token ADD 
%token SUB 
%token MUL 
%token EDIV 
%token ABS 
%token NEG 
%token LSL
%token LSR
%token AND_ 
%token OR_ 
%token XOR 
%token NOT 
%token COMPARE 
%token EQ 
%token NEQ 
%token LT 
%token LE 
%token GT 
%token GE

%token <string> INTV
%token <bool> BOOL
%token <string> STR
%token <string> MNEMONIC
%token <string> LCID
%start topl1
%type <Syntax.program> topl1
%%

bloc13:
  | LBRACE is=inst6 RBRACE  { is }
  ;

inst6:
  |   { [] }
  | i=sing1 SEMI is=inst6  { i :: is }
  | i=sing1  { [ i ] }
  ;

kvli8:
  | ELT l1=lite2 l2=lite2 SEMI ls=kvli8  { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls])) }
  | ELT l1=lite2 l2=lite2 opts7  { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None])) }
  ;

lite2:
  | l1=lite3 ELT l2=lite2  { Exp.tuple [l1; l2] }
  | lite3  { $1 }
  ;

lite3:
  | lite4  { $1 }
  | SOME l=lite3  { Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l) }
  ;

lite4:
  | i=INTV  { Exp.constant (Const.int (int_of_string i)) }
  | UNIT  { Exp.tuple [] }
  | LPAREN_PAIR ls=lits9 RPAREN  { ls }
  | NONE  { Exp.construct (Location.mknoloc (Longident.Lident "None")) None }
  | LPAREN_RIGHT l=lite2 RPAREN  { Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l) }
  | PAIR a=lite4 b=lite4  { Exp.tuple [a; b] }
  | b=BOOL  { Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None }
  | LBRACE kvs=kvli8 RBRACE  { Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs] }
  | LBRACE LIT l=semi5 RBRACE  { l }
  | LPAREN PAIR a=lite2 b=lite2 RPAREN  { Exp.tuple [a; b] }
  | LPAREN_LEFT l=lite2 RPAREN  { Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l) }
  | s=STR  { Exp.constant (Const.string s) }
  ;

lits9:
  | l1=lite2 l2=lite2  { Exp.tuple [l1; l2] }
  | l=lite2 ls=lits9  { Exp.tuple [l; ls] }
  ;

opts7:
  | SEMI  { () }
  |   { () }
  ;

scri12:
  | PARAM pty=ty11 SEMI STORAGE stty=ty11 SEMI CODE LBRACE is=inst6 RBRACE  { Code (Some (pty, stty), is) }
  | CODE LBRACE is=inst6 RBRACE  { Code (None, is) }
  ;

semi5:
  | l=lite2  { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None])) }
  |   { Exp.construct (Location.mknoloc (Longident.Lident "[]")) None }
  | l=lite2 SEMI ls=semi5  { Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls])) }
  ;

sing1:
  | LOOP_LEFT b=bloc13  { LoopLeft (b) }
  | DIP b=bloc13  { OneBlock ("DIP", b) }
  | LBRACE is=inst6 RBRACE  { Block (is) }
  | LAMBDA ty1=ty11 ty2=ty11 b=bloc13  { OneBlockWithTwoTys ("Lambda", ty1, ty2, b) }
  | EXEC  { Simple "Exec" }
  | EDIV  { Simple "EDIV" }
  | m=MNEMONIC LBRACE is1=inst6 RBRACE LBRACE is2=inst6 RBRACE  { TwoBlocks (m, is1, is2) }
  | IF b1=bloc13  { IfThen b1 }
  | GT  { Simple "GT" }
  | ADD  { Simple "ADD" }
  | LSR  { Simple "LSR" }
  | m=MNEMONIC LBRACE is=inst6 RBRACE  { OneBlock (m, is) }
  | ABS  { Simple "ABS" }
  | IF b1=bloc13 b2=bloc13  { IfThenElse (b1, b2) }
  | SUB  { Simple "SUB" }
  | m=MNEMONIC i=INTV LBRACE is=inst6 RBRACE  { OneBlockWithNum (m, int_of_string i, is) }
  | MAP b=bloc13  { Map (b) }
  | LE  { Simple "LE" }
  | MUL  { Simple "MUL" }
  | NOT  { Simple "NOT" }
  | EQ  { Simple "EQ" }
  | OR_  { Simple "OR" }
  | GE  { Simple "GE" }
  | NEG  { Simple "NEG" }
  | IF_NONE b1=bloc13 b2=bloc13  { IfNone  (b1, b2) }
  | NEQ  { Simple "NEQ" }
  | m=MNEMONIC ty11 l=lite2  { SimpleArgCon (m, l) }
  | IF_LEFT b1=bloc13 b2=bloc13  { IfLeft  (b1, b2) }
  | ITER b=bloc13  { Iter (b) }
  | IF_RIGHT b1=bloc13 b2=bloc13  { IfRight (b1, b2) }
  | m=MNEMONIC LBRACE sc=scri12 RBRACE  { CreateContract (m, sc) }
  | m=MNEMONIC ty1=ty11 ty2=ty11 LBRACE is=inst6 RBRACE  { OneBlockWithTwoTys (m, ty1, ty2, is) }
  | LT  { Simple "LT" }
  | m=MNEMONIC ty11 ty11  { Simple m }
  | m=MNEMONIC ty11  { Simple m }
  | m=MNEMONIC i=INTV  { SimpleWithNum (m, int_of_string i) }
  | LOOP b=bloc13  { Loop (b) }
  | XOR  { Simple "XOR" }
  | DIP i=INTV b=bloc13  { OneBlockWithNum ("DIP", int_of_string i, b) }
  | LSL  { Simple "LSL" }
  | AND_  { Simple "AND" }
  | m=MNEMONIC  { Simple m }
  | COMPARE  { Simple "COMPARE" }
  ;

topl1:
  | sc=scri12 EOF  { sc }
  ;

ty10:
  | ty=ty11 tyds=ty10  { ty::tyds }
  |   { [] }
  ;

ty11:
  | LPAREN ty=LCID tail=ty10 RPAREN  { let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail }
  | ty=LCID  { Typ.constr (Location.mknoloc (Longident.Lident ty)) [] }
  ;
