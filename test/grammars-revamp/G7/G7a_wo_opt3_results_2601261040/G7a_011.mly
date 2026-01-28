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

bloc14:
  | LBRACE is=inst7 RBRACE  {  is  }
  ;

inst7:
  | i=sing1  {  [ i ]  }
  |   {  []  }
  | i=sing1 SEMI is=inst7  {  i :: is  }
  ;

kvli9:
  | ELT l1=lite2 l2=lite2 SEMI ls=kvli9  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  | ELT l1=lite2 l2=lite2 opts8  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

lite2:
  | PAIR a=lite3 b=lite3  {  Exp.tuple [a; b]  }
  | LBRACE kvs=kvli9 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | l1=lite5 ELT l2=lite4  {  Exp.tuple [l1; l2]  }
  | LPAREN_RIGHT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LPAREN_PAIR ls=lits10 RPAREN  {  ls  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | LBRACE LIT l=semi6 RBRACE  {  l  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LPAREN_LEFT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | SOME l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  | LPAREN PAIR a=lite2 b=lite2 RPAREN  {  Exp.tuple [a; b]  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | UNIT  {  Exp.tuple []  }
  ;

lite3:
  | s=STR  {  Exp.constant (Const.string s)  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | l1=lite5 ELT l2=lite4  {  Exp.tuple [l1; l2]  }
  | LPAREN_RIGHT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LBRACE LIT l=semi6 RBRACE  {  l  }
  | LPAREN PAIR a=lite2 b=lite2 RPAREN  {  Exp.tuple [a; b]  }
  | LPAREN_LEFT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | LPAREN_PAIR ls=lits10 RPAREN  {  ls  }
  | PAIR a=lite3 b=lite3  {  Exp.tuple [a; b]  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | UNIT  {  Exp.tuple []  }
  | LBRACE kvs=kvli9 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  ;

lite4:
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | UNIT  {  Exp.tuple []  }
  | LPAREN_PAIR ls=lits10 RPAREN  {  ls  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LPAREN_RIGHT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | LBRACE kvs=kvli9 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | l1=lite5 ELT l2=lite4  {  Exp.tuple [l1; l2]  }
  | LBRACE LIT l=semi6 RBRACE  {  l  }
  | LPAREN PAIR a=lite2 b=lite2 RPAREN  {  Exp.tuple [a; b]  }
  | LPAREN_LEFT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | s=STR  {  Exp.constant (Const.string s)  }
  ;

lite5:
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LPAREN_LEFT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | LBRACE LIT l=semi6 RBRACE  {  l  }
  | UNIT  {  Exp.tuple []  }
  | LPAREN_RIGHT l=lite2 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LPAREN PAIR a=lite2 b=lite2 RPAREN  {  Exp.tuple [a; b]  }
  | LBRACE kvs=kvli9 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | LPAREN_PAIR ls=lits10 RPAREN  {  ls  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  ;

lits10:
  | l=lite2 ls=lits10  {  Exp.tuple [l; ls]  }
  | l1=lite2 l2=lite2  {  Exp.tuple [l1; l2]  }
  ;

opts8:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

scri13:
  | CODE LBRACE is=inst7 RBRACE  {  Code (None, is)  }
  | PARAM pty=ty12 SEMI STORAGE stty=ty12 SEMI CODE LBRACE is=inst7 RBRACE  {  Code (Some (pty, stty), is)  }
  ;

semi6:
  | l=lite2  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | l=lite2 SEMI ls=semi6  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  ;

sing1:
  | LOOP_LEFT b=bloc14  {  LoopLeft (b)  }
  | DIP b=bloc14  {  OneBlock ("DIP", b)  }
  | LBRACE is=inst7 RBRACE  {  Block (is)  }
  | LAMBDA ty1=ty12 ty2=ty12 b=bloc14  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | EXEC  {  Simple "Exec"  }
  | EDIV  {  Simple "EDIV"  }
  | m=MNEMONIC LBRACE is1=inst7 RBRACE LBRACE is2=inst7 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | IF b1=bloc14  {  IfThen b1  }
  | GT  {  Simple "GT"  }
  | ADD  {  Simple "ADD"  }
  | LSR  {  Simple "LSR"  }
  | m=MNEMONIC LBRACE is=inst7 RBRACE  {  OneBlock (m, is)  }
  | ABS  {  Simple "ABS"  }
  | IF b1=bloc14 b2=bloc14  {  IfThenElse (b1, b2)  }
  | SUB  {  Simple "SUB"  }
  | m=MNEMONIC i=INTV LBRACE is=inst7 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | MAP b=bloc14  {  Map (b)  }
  | LE  {  Simple "LE"  }
  | MUL  {  Simple "MUL"  }
  | NOT  {  Simple "NOT"  }
  | EQ  {  Simple "EQ"  }
  | OR_  {  Simple "OR"  }
  | GE  {  Simple "GE"  }
  | NEG  {  Simple "NEG"  }
  | IF_NONE b1=bloc14 b2=bloc14  {  IfNone  (b1, b2)  }
  | NEQ  {  Simple "NEQ"  }
  | m=MNEMONIC ty12 l=lite2  {  SimpleArgCon (m, l)  }
  | IF_LEFT b1=bloc14 b2=bloc14  {  IfLeft  (b1, b2)  }
  | ITER b=bloc14  {  Iter (b)  }
  | IF_RIGHT b1=bloc14 b2=bloc14  {  IfRight (b1, b2)  }
  | m=MNEMONIC LBRACE sc=scri13 RBRACE  {  CreateContract (m, sc)  }
  | m=MNEMONIC ty1=ty12 ty2=ty12 LBRACE is=inst7 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | LT  {  Simple "LT"  }
  | m=MNEMONIC ty12 ty12  {  Simple m  }
  | m=MNEMONIC ty12  {  Simple m  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | LOOP b=bloc14  {  Loop (b)  }
  | XOR  {  Simple "XOR"  }
  | DIP i=INTV b=bloc14  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | LSL  {  Simple "LSL"  }
  | AND_  {  Simple "AND"  }
  | m=MNEMONIC  {  Simple m  }
  | COMPARE  {  Simple "COMPARE"  }
  ;

topl1:
  | sc=scri13 EOF  {  sc  }
  ;

ty11:
  |   {  []  }
  | ty=ty12 tyds=ty11  {  ty::tyds  }
  ;

ty12:
  | ty=LCID  {  Typ.constr (Location.mknoloc (Longident.Lident ty)) []  }
  | LPAREN ty=LCID tail=ty11 RPAREN  {  let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail  }
  ;
