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
%token <string> LCID%start topl1
%type <Syntax.program> topl1
%%

bloc18:
  | bloc19  { $1 }
  ;

bloc19:
  | LBRACE is=inst8 RBRACE  {  is  }
  ;

inst8:
  | inst9  { $1 }
  ;

inst9:
  |   {  []  }
  | i=sing1  {  [ i ]  }
  | i=sing1 SEMI is=inst8  {  i :: is  }
  ;

kvli11:
  | ELT l1=lite2 l2=lite2 opts10  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | ELT l1=lite2 l2=lite2 SEMI ls=kvli11  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  ;

lite2:
  | lite3  { $1 }
  ;

lite3:
  | lite4  { $1 }
  | SOME l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  ;

lite4:
  | PAIR a=lite4 b=lite4  {  Exp.tuple [a; b]  }
  | lite5  { $1 }
  ;

lite5:
  | l1=lite5 ELT l2=lite6  {  Exp.tuple [l1; l2]  }
  | lite6  { $1 }
  ;

lite6:
  | UNIT  {  Exp.tuple []  }
  | LPAREN_PAIR ls=lits12 RPAREN  {  ls  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | LBRACE kvs=kvli11 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | LPAREN PAIR a=lite2 b=lite2 RPAREN  {  Exp.tuple [a; b]  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LPAREN_RIGHT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LBRACE LIT l=semi7 RBRACE  {  l  }
  | LPAREN_LEFT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  ;

lits12:
  | l1=lite2 l2=lite2  {  Exp.tuple [l1; l2]  }
  | l=lite2 ls=lits12  {  Exp.tuple [l; ls]  }
  ;

opts10:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

scri13:
  | scri17  { $1 }
  ;

scri17:
  | PARAM pty=ty16 SEMI STORAGE stty=ty16 SEMI CODE LBRACE is=inst9 RBRACE  {  Code (Some (pty, stty), is)  }
  | CODE LBRACE is=inst9 RBRACE  {  Code (None, is)  }
  ;

semi7:
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | l=lite2 SEMI ls=semi7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  ;

sing1:
  | LOOP_LEFT b=bloc18  {  LoopLeft (b)  }
  | DIP b=bloc18  {  OneBlock ("DIP", b)  }
  | LBRACE is=inst8 RBRACE  {  Block (is)  }
  | LAMBDA ty1=ty14 ty2=ty14 b=bloc19  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | EXEC  {  Simple "Exec"  }
  | EDIV  {  Simple "EDIV"  }
  | m=MNEMONIC LBRACE is1=inst8 RBRACE LBRACE is2=inst8 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | IF b1=bloc18  {  IfThen b1  }
  | GT  {  Simple "GT"  }
  | ADD  {  Simple "ADD"  }
  | LSR  {  Simple "LSR"  }
  | m=MNEMONIC LBRACE is=inst8 RBRACE  {  OneBlock (m, is)  }
  | ABS  {  Simple "ABS"  }
  | IF b1=bloc18 b2=bloc18  {  IfThenElse (b1, b2)  }
  | SUB  {  Simple "SUB"  }
  | m=MNEMONIC i=INTV LBRACE is=inst8 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | MAP b=bloc18  {  Map (b)  }
  | LE  {  Simple "LE"  }
  | MUL  {  Simple "MUL"  }
  | NOT  {  Simple "NOT"  }
  | EQ  {  Simple "EQ"  }
  | OR_  {  Simple "OR"  }
  | GE  {  Simple "GE"  }
  | NEG  {  Simple "NEG"  }
  | IF_NONE b1=bloc18 b2=bloc18  {  IfNone  (b1, b2)  }
  | NEQ  {  Simple "NEQ"  }
  | m=MNEMONIC ty14 l=lite3  {  SimpleArgCon (m, l)  }
  | IF_LEFT b1=bloc18 b2=bloc18  {  IfLeft  (b1, b2)  }
  | ITER b=bloc18  {  Iter (b)  }
  | IF_RIGHT b1=bloc18 b2=bloc18  {  IfRight (b1, b2)  }
  | m=MNEMONIC LBRACE sc=scri13 RBRACE  {  CreateContract (m, sc)  }
  | m=MNEMONIC ty1=ty14 ty2=ty14 LBRACE is=inst8 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | LT  {  Simple "LT"  }
  | m=MNEMONIC ty14 ty14  {  Simple m  }
  | m=MNEMONIC ty14  {  Simple m  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | LOOP b=bloc18  {  Loop (b)  }
  | XOR  {  Simple "XOR"  }
  | DIP i=INTV b=bloc18  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | LSL  {  Simple "LSL"  }
  | AND_  {  Simple "AND"  }
  | m=MNEMONIC  {  Simple m  }
  | COMPARE  {  Simple "COMPARE"  }
  ;

topl1:
  | sc=scri17 EOF  {  sc  }
  ;

ty14:
  | ty16  { $1 }
  ;

ty15:
  | ty=ty14 tyds=ty15  {  ty::tyds  }
  |   {  []  }
  ;

ty16:
  | LPAREN ty=LCID tail=ty15 RPAREN  {  let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail  }
  | ty=LCID  {  Typ.constr (Location.mknoloc (Longident.Lident ty)) []  }
  ;
