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
%token <string> LCID%start topl14
%type <Syntax.program> topl14
%%

bloc15:
  | LBRACE is=inst7 RBRACE  {  is  }
  ;

inst7:
  | i=sing1  {  [ i ]  }
  |   {  []  }
  | i=sing1 SEMI is=inst7  {  i :: is  }
  ;

kvli13:
  | ELT l1=lite2 l2=lite2 opts11  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | ELT l1=lite2 l2=lite2 SEMI ls=kvli13  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  ;

lite2:
  | lite3  { $1 }
  | SOME l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  ;

lite3:
  | lite4  { $1 }
  | PAIR a=lite3 b=lite3  {  Exp.tuple [a; b]  }
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
  | LBRACE kvs=kvli13 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | LPAREN_PAIR ls=lits12 RPAREN  {  ls  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  ;

lits12:
  | l1=lite2 l2=lite2  {  Exp.tuple [l1; l2]  }
  | l=lite2 ls=lits12  {  Exp.tuple [l; ls]  }
  ;

opts11:
  | SEMI  {  ()  }
  |   {  ()  }
  ;

scri10:
  | CODE LBRACE is=inst7 RBRACE  {  Code (None, is)  }
  | PARAM pty=ty8 SEMI STORAGE stty=ty8 SEMI CODE LBRACE is=inst7 RBRACE  {  Code (Some (pty, stty), is)  }
  ;

semi6:
  | l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | l=lite2 SEMI ls=semi6  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  ;

sing1:
  | LOOP_LEFT b=bloc15  {  LoopLeft (b)  }
  | DIP b=bloc15  {  OneBlock ("DIP", b)  }
  | LBRACE is=inst7 RBRACE  {  Block (is)  }
  | LAMBDA ty1=ty8 ty2=ty8 b=bloc15  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | EXEC  {  Simple "Exec"  }
  | EDIV  {  Simple "EDIV"  }
  | m=MNEMONIC LBRACE is1=inst7 RBRACE LBRACE is2=inst7 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | IF b1=bloc15  {  IfThen b1  }
  | GT  {  Simple "GT"  }
  | ADD  {  Simple "ADD"  }
  | LSR  {  Simple "LSR"  }
  | m=MNEMONIC LBRACE is=inst7 RBRACE  {  OneBlock (m, is)  }
  | ABS  {  Simple "ABS"  }
  | IF b1=bloc15 b2=bloc15  {  IfThenElse (b1, b2)  }
  | SUB  {  Simple "SUB"  }
  | m=MNEMONIC i=INTV LBRACE is=inst7 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | MAP b=bloc15  {  Map (b)  }
  | LE  {  Simple "LE"  }
  | MUL  {  Simple "MUL"  }
  | NOT  {  Simple "NOT"  }
  | EQ  {  Simple "EQ"  }
  | OR_  {  Simple "OR"  }
  | GE  {  Simple "GE"  }
  | NEG  {  Simple "NEG"  }
  | IF_NONE b1=bloc15 b2=bloc15  {  IfNone  (b1, b2)  }
  | NEQ  {  Simple "NEQ"  }
  | m=MNEMONIC ty8 l=lite2  {  SimpleArgCon (m, l)  }
  | IF_LEFT b1=bloc15 b2=bloc15  {  IfLeft  (b1, b2)  }
  | ITER b=bloc15  {  Iter (b)  }
  | IF_RIGHT b1=bloc15 b2=bloc15  {  IfRight (b1, b2)  }
  | m=MNEMONIC LBRACE sc=scri10 RBRACE  {  CreateContract (m, sc)  }
  | m=MNEMONIC ty1=ty8 ty2=ty8 LBRACE is=inst7 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | LT  {  Simple "LT"  }
  | m=MNEMONIC ty8 ty8  {  Simple m  }
  | m=MNEMONIC ty8  {  Simple m  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | LOOP b=bloc15  {  Loop (b)  }
  | XOR  {  Simple "XOR"  }
  | DIP i=INTV b=bloc15  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | LSL  {  Simple "LSL"  }
  | AND_  {  Simple "AND"  }
  | m=MNEMONIC  {  Simple m  }
  | COMPARE  {  Simple "COMPARE"  }
  ;

topl14:
  | sc=scri10 EOF  {  sc  }
  ;

ty8:
  | LPAREN ty=LCID tail=ty9 RPAREN  {  let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail  }
  | ty=LCID  {  Typ.constr (Location.mknoloc (Longident.Lident ty)) []  }
  ;

ty9:
  |   {  []  }
  | ty=ty8 tyds=ty9  {  ty::tyds  }
  ;
