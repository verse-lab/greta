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

bloc17:
  | bloc18  { $1 }
  ;

bloc18:
  | LBRACE is=inst7 RBRACE  {  is  }
  ;

inst7:
  | inst8  { $1 }
  ;

inst8:
  | i=sing1  {  [ i ]  }
  | i=sing1 SEMI is=inst7  {  i :: is  }
  |   {  []  }
  ;

kvli10:
  | ELT l1=lite2 l2=lite2 opts9  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | ELT l1=lite2 l2=lite2 SEMI ls=kvli10  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  ;

lite2:
  | lite3  { $1 }
  ;

lite3:
  | lite4  { $1 }
  | SOME l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  ;

lite4:
  | l1=lite5 ELT l2=lite4  {  Exp.tuple [l1; l2]  }
  | lite5  { $1 }
  ;

lite5:
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LPAREN_LEFT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | LBRACE LIT l=semi6 RBRACE  {  l  }
  | UNIT  {  Exp.tuple []  }
  | LPAREN_RIGHT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LPAREN PAIR a=lite2 b=lite2 RPAREN  {  Exp.tuple [a; b]  }
  | PAIR a=lite5 b=lite5  {  Exp.tuple [a; b]  }
  | LBRACE kvs=kvli10 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | LPAREN_PAIR ls=lits11 RPAREN  {  ls  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  ;

lits11:
  | l=lite2 ls=lits11  {  Exp.tuple [l; ls]  }
  | l1=lite2 l2=lite2  {  Exp.tuple [l1; l2]  }
  ;

opts9:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

scri12:
  | scri16  { $1 }
  ;

scri16:
  | CODE LBRACE is=inst8 RBRACE  {  Code (None, is)  }
  | PARAM pty=ty15 SEMI STORAGE stty=ty15 SEMI CODE LBRACE is=inst8 RBRACE  {  Code (Some (pty, stty), is)  }
  ;

semi6:
  | l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | l=lite2 SEMI ls=semi6  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  ;

sing1:
  | LOOP_LEFT b=bloc17  {  LoopLeft (b)  }
  | DIP b=bloc17  {  OneBlock ("DIP", b)  }
  | LBRACE is=inst7 RBRACE  {  Block (is)  }
  | LAMBDA ty1=ty13 ty2=ty13 b=bloc18  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | EXEC  {  Simple "Exec"  }
  | EDIV  {  Simple "EDIV"  }
  | m=MNEMONIC LBRACE is1=inst7 RBRACE LBRACE is2=inst7 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | IF b1=bloc17  {  IfThen b1  }
  | GT  {  Simple "GT"  }
  | ADD  {  Simple "ADD"  }
  | LSR  {  Simple "LSR"  }
  | m=MNEMONIC LBRACE is=inst7 RBRACE  {  OneBlock (m, is)  }
  | ABS  {  Simple "ABS"  }
  | IF b1=bloc17 b2=bloc17  {  IfThenElse (b1, b2)  }
  | SUB  {  Simple "SUB"  }
  | m=MNEMONIC i=INTV LBRACE is=inst7 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | MAP b=bloc17  {  Map (b)  }
  | LE  {  Simple "LE"  }
  | MUL  {  Simple "MUL"  }
  | NOT  {  Simple "NOT"  }
  | EQ  {  Simple "EQ"  }
  | OR_  {  Simple "OR"  }
  | GE  {  Simple "GE"  }
  | NEG  {  Simple "NEG"  }
  | IF_NONE b1=bloc17 b2=bloc17  {  IfNone  (b1, b2)  }
  | NEQ  {  Simple "NEQ"  }
  | m=MNEMONIC ty13 l=lite3  {  SimpleArgCon (m, l)  }
  | IF_LEFT b1=bloc17 b2=bloc17  {  IfLeft  (b1, b2)  }
  | ITER b=bloc17  {  Iter (b)  }
  | IF_RIGHT b1=bloc17 b2=bloc17  {  IfRight (b1, b2)  }
  | m=MNEMONIC LBRACE sc=scri12 RBRACE  {  CreateContract (m, sc)  }
  | m=MNEMONIC ty1=ty13 ty2=ty13 LBRACE is=inst7 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | LT  {  Simple "LT"  }
  | m=MNEMONIC ty13 ty13  {  Simple m  }
  | m=MNEMONIC ty13  {  Simple m  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | LOOP b=bloc17  {  Loop (b)  }
  | XOR  {  Simple "XOR"  }
  | DIP i=INTV b=bloc17  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | LSL  {  Simple "LSL"  }
  | AND_  {  Simple "AND"  }
  | m=MNEMONIC  {  Simple m  }
  | COMPARE  {  Simple "COMPARE"  }
  ;

topl1:
  | sc=scri16 EOF  {  sc  }
  ;

ty13:
  | ty15  { $1 }
  ;

ty14:
  | ty=ty13 tyds=ty14  {  ty::tyds  }
  |   {  []  }
  ;

ty15:
  | LPAREN ty=LCID tail=ty14 RPAREN  {  let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail  }
  | ty=LCID  {  Typ.constr (Location.mknoloc (Longident.Lident ty)) []  }
  ;
