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
%token <string> LCID%start topl62
%type <Syntax.program> topl62
%%

bloc63:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

bloc64:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

bloc65:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

bloc66:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

bloc67:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

bloc68:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

bloc69:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

bloc70:
  | LBRACE is=inst22 RBRACE  {  is  }
  ;

inst22:
  |   {  []  }
  | i=sing1 SEMI is=inst22  {  i :: is  }
  | i=sing1  {  [ i ]  }
  ;

inst23:
  | i=sing1 SEMI is=inst22  {  i :: is  }
  |   {  []  }
  | i=sing1  {  [ i ]  }
  ;

inst24:
  | i=sing1 SEMI is=inst22  {  i :: is  }
  |   {  []  }
  | i=sing1  {  [ i ]  }
  ;

kvli53:
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  ;

kvli54:
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

kvli55:
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

kvli56:
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

kvli57:
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

kvli58:
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

kvli59:
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  ;

kvli60:
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

kvli61:
  | ELT l1=lite7 l2=lite7 opts40  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | ELT l1=lite7 l2=lite7 SEMI ls=kvli59  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))  }
  ;

lite10:
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | PAIR a=lite11 b=lite11  {  Exp.tuple [a; b]  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | UNIT  {  Exp.tuple []  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | l1=lite11 ELT l2=lite10  {  Exp.tuple [l1; l2]  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  ;

lite11:
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | PAIR a=lite11 b=lite11  {  Exp.tuple [a; b]  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | UNIT  {  Exp.tuple []  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  ;

lite12:
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | UNIT  {  Exp.tuple []  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  ;

lite5:
  | l1=lite11 ELT l2=lite10  {  Exp.tuple [l1; l2]  }
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | UNIT  {  Exp.tuple []  }
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | PAIR a=lite11 b=lite11  {  Exp.tuple [a; b]  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | SOME l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  ;

lite6:
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | l1=lite11 ELT l2=lite10  {  Exp.tuple [l1; l2]  }
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | UNIT  {  Exp.tuple []  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | SOME l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | PAIR a=lite11 b=lite11  {  Exp.tuple [a; b]  }
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  ;

lite7:
  | l1=lite11 ELT l2=lite10  {  Exp.tuple [l1; l2]  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | PAIR a=lite11 b=lite11  {  Exp.tuple [a; b]  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | UNIT  {  Exp.tuple []  }
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | SOME l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  ;

lite8:
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | l1=lite11 ELT l2=lite10  {  Exp.tuple [l1; l2]  }
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | SOME l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  | UNIT  {  Exp.tuple []  }
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | PAIR a=lite11 b=lite11  {  Exp.tuple [a; b]  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  ;

lite9:
  | LPAREN PAIR a=lite7 b=lite7 RPAREN  {  Exp.tuple [a; b]  }
  | LPAREN_LEFT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l)  }
  | b=BOOL  {  Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None  }
  | LBRACE LIT l=semi19 RBRACE  {  l  }
  | i=INTV  {  Exp.constant (Const.int (int_of_string i))  }
  | UNIT  {  Exp.tuple []  }
  | LBRACE kvs=kvli59 RBRACE  {  Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]  }
  | l1=lite11 ELT l2=lite10  {  Exp.tuple [l1; l2]  }
  | PAIR a=lite11 b=lite11  {  Exp.tuple [a; b]  }
  | LPAREN_RIGHT l=lite7 RPAREN  {  Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l)  }
  | NONE  {  Exp.construct (Location.mknoloc (Longident.Lident "None")) None  }
  | LPAREN_PAIR ls=lits50 RPAREN  {  ls  }
  | s=STR  {  Exp.constant (Const.string s)  }
  | SOME l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l)  }
  ;

lits44:
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  ;

lits45:
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  ;

lits46:
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  ;

lits47:
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  ;

lits48:
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  ;

lits49:
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  ;

lits50:
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  ;

lits51:
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  ;

lits52:
  | l=lite7 ls=lits50  {  Exp.tuple [l; ls]  }
  | l1=lite7 l2=lite7  {  Exp.tuple [l1; l2]  }
  ;

opts34:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

opts35:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

opts36:
  | SEMI  {  ()  }
  |   {  ()  }
  ;

opts37:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

opts38:
  | SEMI  {  ()  }
  |   {  ()  }
  ;

opts39:
  | SEMI  {  ()  }
  |   {  ()  }
  ;

opts40:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

opts41:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

opts42:
  |   {  ()  }
  | SEMI  {  ()  }
  ;

opts43:
  | SEMI  {  ()  }
  |   {  ()  }
  ;

scri32:
  | CODE LBRACE is=inst24 RBRACE  {  Code (None, is)  }
  | PARAM pty=ty27 SEMI STORAGE stty=ty27 SEMI CODE LBRACE is=inst24 RBRACE  {  Code (Some (pty, stty), is)  }
  ;

scri33:
  | CODE LBRACE is=inst24 RBRACE  {  Code (None, is)  }
  | PARAM pty=ty27 SEMI STORAGE stty=ty27 SEMI CODE LBRACE is=inst24 RBRACE  {  Code (Some (pty, stty), is)  }
  ;

semi13:
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  ;

semi14:
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  ;

semi15:
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

semi16:
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

semi17:
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  ;

semi18:
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

semi19:
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

semi20:
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  ;

semi21:
  |   {  Exp.construct (Location.mknoloc (Longident.Lident "[]")) None  }
  | l=lite7  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))  }
  | l=lite7 SEMI ls=semi19  {  Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))  }
  ;

sing1:
  | EXEC  {  Simple "Exec"  }
  | m=MNEMONIC LBRACE is1=inst22 RBRACE LBRACE is2=inst22 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | IF b1=bloc66  {  IfThen b1  }
  | GT  {  Simple "GT"  }
  | LSR  {  Simple "LSR"  }
  | IF b1=bloc66 b2=bloc66  {  IfThenElse (b1, b2)  }
  | SUB  {  Simple "SUB"  }
  | MUL  {  Simple "MUL"  }
  | NOT  {  Simple "NOT"  }
  | EQ  {  Simple "EQ"  }
  | OR_  {  Simple "OR"  }
  | NEG  {  Simple "NEG"  }
  | NEQ  {  Simple "NEQ"  }
  | m=MNEMONIC ty25 l=lite5  {  SimpleArgCon (m, l)  }
  | ITER b=bloc66  {  Iter (b)  }
  | IF_RIGHT b1=bloc66 b2=bloc66  {  IfRight (b1, b2)  }
  | m=MNEMONIC LBRACE sc=scri33 RBRACE  {  CreateContract (m, sc)  }
  | m=MNEMONIC ty1=ty25 ty2=ty25 LBRACE is=inst22 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | m=MNEMONIC ty25 ty25  {  Simple m  }
  | m=MNEMONIC ty25  {  Simple m  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | LOOP b=bloc66  {  Loop (b)  }
  | XOR  {  Simple "XOR"  }
  | DIP i=INTV b=bloc66  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | m=MNEMONIC  {  Simple m  }
  | LOOP_LEFT b=bloc66  {  LoopLeft (b)  }
  | DIP b=bloc66  {  OneBlock ("DIP", b)  }
  | LBRACE is=inst22 RBRACE  {  Block (is)  }
  | LAMBDA ty1=ty25 ty2=ty25 b=bloc64  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | EDIV  {  Simple "EDIV"  }
  | ADD  {  Simple "ADD"  }
  | m=MNEMONIC LBRACE is=inst22 RBRACE  {  OneBlock (m, is)  }
  | ABS  {  Simple "ABS"  }
  | m=MNEMONIC i=INTV LBRACE is=inst22 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | MAP b=bloc66  {  Map (b)  }
  | LE  {  Simple "LE"  }
  | GE  {  Simple "GE"  }
  | IF_NONE b1=bloc66 b2=bloc66  {  IfNone  (b1, b2)  }
  | IF_LEFT b1=bloc66 b2=bloc66  {  IfLeft  (b1, b2)  }
  | LT  {  Simple "LT"  }
  | LSL  {  Simple "LSL"  }
  | AND_  {  Simple "AND"  }
  | COMPARE  {  Simple "COMPARE"  }
  ;

sing2:
  | DIP b=bloc66  {  OneBlock ("DIP", b)  }
  | EXEC  {  Simple "Exec"  }
  | XOR  {  Simple "XOR"  }
  | MUL  {  Simple "MUL"  }
  | m=MNEMONIC LBRACE is1=inst22 RBRACE LBRACE is2=inst22 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | GE  {  Simple "GE"  }
  | NEQ  {  Simple "NEQ"  }
  | LE  {  Simple "LE"  }
  | LT  {  Simple "LT"  }
  | IF b1=bloc66  {  IfThen b1  }
  | NOT  {  Simple "NOT"  }
  | IF_RIGHT b1=bloc66 b2=bloc66  {  IfRight (b1, b2)  }
  | m=MNEMONIC ty25  {  Simple m  }
  | IF_LEFT b1=bloc66 b2=bloc66  {  IfLeft  (b1, b2)  }
  | LSL  {  Simple "LSL"  }
  | m=MNEMONIC  {  Simple m  }
  | IF b1=bloc66 b2=bloc66  {  IfThenElse (b1, b2)  }
  | EDIV  {  Simple "EDIV"  }
  | LAMBDA ty1=ty25 ty2=ty25 b=bloc64  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | MAP b=bloc66  {  Map (b)  }
  | EQ  {  Simple "EQ"  }
  | LSR  {  Simple "LSR"  }
  | OR_  {  Simple "OR"  }
  | NEG  {  Simple "NEG"  }
  | m=MNEMONIC LBRACE sc=scri33 RBRACE  {  CreateContract (m, sc)  }
  | m=MNEMONIC LBRACE is=inst22 RBRACE  {  OneBlock (m, is)  }
  | SUB  {  Simple "SUB"  }
  | ABS  {  Simple "ABS"  }
  | LOOP_LEFT b=bloc66  {  LoopLeft (b)  }
  | ITER b=bloc66  {  Iter (b)  }
  | COMPARE  {  Simple "COMPARE"  }
  | IF_NONE b1=bloc66 b2=bloc66  {  IfNone  (b1, b2)  }
  | DIP i=INTV b=bloc66  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | AND_  {  Simple "AND"  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | GT  {  Simple "GT"  }
  | m=MNEMONIC i=INTV LBRACE is=inst22 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | ADD  {  Simple "ADD"  }
  | LOOP b=bloc66  {  Loop (b)  }
  | LBRACE is=inst22 RBRACE  {  Block (is)  }
  | m=MNEMONIC ty1=ty25 ty2=ty25 LBRACE is=inst22 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | m=MNEMONIC ty25 ty25  {  Simple m  }
  | m=MNEMONIC ty25 l=lite5  {  SimpleArgCon (m, l)  }
  ;

sing3:
  | LSL  {  Simple "LSL"  }
  | DIP b=bloc66  {  OneBlock ("DIP", b)  }
  | LT  {  Simple "LT"  }
  | XOR  {  Simple "XOR"  }
  | MUL  {  Simple "MUL"  }
  | ITER b=bloc66  {  Iter (b)  }
  | IF b1=bloc66 b2=bloc66  {  IfThenElse (b1, b2)  }
  | OR_  {  Simple "OR"  }
  | GE  {  Simple "GE"  }
  | NEG  {  Simple "NEG"  }
  | IF_LEFT b1=bloc66 b2=bloc66  {  IfLeft  (b1, b2)  }
  | m=MNEMONIC ty25  {  Simple m  }
  | LOOP b=bloc66  {  Loop (b)  }
  | m=MNEMONIC LBRACE is1=inst22 RBRACE LBRACE is2=inst22 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | m=MNEMONIC ty25 l=lite5  {  SimpleArgCon (m, l)  }
  | LBRACE is=inst22 RBRACE  {  Block (is)  }
  | m=MNEMONIC  {  Simple m  }
  | NEQ  {  Simple "NEQ"  }
  | DIP i=INTV b=bloc66  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | LOOP_LEFT b=bloc66  {  LoopLeft (b)  }
  | m=MNEMONIC ty1=ty25 ty2=ty25 LBRACE is=inst22 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | m=MNEMONIC ty25 ty25  {  Simple m  }
  | COMPARE  {  Simple "COMPARE"  }
  | m=MNEMONIC LBRACE sc=scri33 RBRACE  {  CreateContract (m, sc)  }
  | GT  {  Simple "GT"  }
  | SUB  {  Simple "SUB"  }
  | LSR  {  Simple "LSR"  }
  | ADD  {  Simple "ADD"  }
  | MAP b=bloc66  {  Map (b)  }
  | ABS  {  Simple "ABS"  }
  | NOT  {  Simple "NOT"  }
  | EXEC  {  Simple "Exec"  }
  | IF_RIGHT b1=bloc66 b2=bloc66  {  IfRight (b1, b2)  }
  | m=MNEMONIC i=INTV LBRACE is=inst22 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | AND_  {  Simple "AND"  }
  | EDIV  {  Simple "EDIV"  }
  | EQ  {  Simple "EQ"  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | LE  {  Simple "LE"  }
  | m=MNEMONIC LBRACE is=inst22 RBRACE  {  OneBlock (m, is)  }
  | IF_NONE b1=bloc66 b2=bloc66  {  IfNone  (b1, b2)  }
  | LAMBDA ty1=ty25 ty2=ty25 b=bloc64  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | IF b1=bloc66  {  IfThen b1  }
  ;

sing4:
  | OR_  {  Simple "OR"  }
  | LOOP_LEFT b=bloc66  {  LoopLeft (b)  }
  | LBRACE is=inst22 RBRACE  {  Block (is)  }
  | NEG  {  Simple "NEG"  }
  | GT  {  Simple "GT"  }
  | EDIV  {  Simple "EDIV"  }
  | LAMBDA ty1=ty25 ty2=ty25 b=bloc64  {  OneBlockWithTwoTys ("Lambda", ty1, ty2, b)  }
  | IF_RIGHT b1=bloc66 b2=bloc66  {  IfRight (b1, b2)  }
  | m=MNEMONIC LBRACE is1=inst22 RBRACE LBRACE is2=inst22 RBRACE  {  TwoBlocks (m, is1, is2)  }
  | LE  {  Simple "LE"  }
  | IF b1=bloc66  {  IfThen b1  }
  | m=MNEMONIC i=INTV LBRACE is=inst22 RBRACE  {  OneBlockWithNum (m, int_of_string i, is)  }
  | DIP b=bloc66  {  OneBlock ("DIP", b)  }
  | m=MNEMONIC ty1=ty25 ty2=ty25 LBRACE is=inst22 RBRACE  {  OneBlockWithTwoTys (m, ty1, ty2, is)  }
  | EXEC  {  Simple "Exec"  }
  | AND_  {  Simple "AND"  }
  | GE  {  Simple "GE"  }
  | IF_LEFT b1=bloc66 b2=bloc66  {  IfLeft  (b1, b2)  }
  | ADD  {  Simple "ADD"  }
  | m=MNEMONIC ty25 ty25  {  Simple m  }
  | ITER b=bloc66  {  Iter (b)  }
  | LOOP b=bloc66  {  Loop (b)  }
  | m=MNEMONIC LBRACE is=inst22 RBRACE  {  OneBlock (m, is)  }
  | LSR  {  Simple "LSR"  }
  | MUL  {  Simple "MUL"  }
  | IF_NONE b1=bloc66 b2=bloc66  {  IfNone  (b1, b2)  }
  | NOT  {  Simple "NOT"  }
  | LSL  {  Simple "LSL"  }
  | m=MNEMONIC LBRACE sc=scri33 RBRACE  {  CreateContract (m, sc)  }
  | m=MNEMONIC i=INTV  {  SimpleWithNum (m, int_of_string i)  }
  | m=MNEMONIC  {  Simple m  }
  | MAP b=bloc66  {  Map (b)  }
  | LT  {  Simple "LT"  }
  | m=MNEMONIC ty25  {  Simple m  }
  | XOR  {  Simple "XOR"  }
  | ABS  {  Simple "ABS"  }
  | DIP i=INTV b=bloc66  {  OneBlockWithNum ("DIP", int_of_string i, b)  }
  | SUB  {  Simple "SUB"  }
  | IF b1=bloc66 b2=bloc66  {  IfThenElse (b1, b2)  }
  | COMPARE  {  Simple "COMPARE"  }
  | NEQ  {  Simple "NEQ"  }
  | EQ  {  Simple "EQ"  }
  | m=MNEMONIC ty25 l=lite5  {  SimpleArgCon (m, l)  }
  ;

topl62:
  | sc=scri32 EOF  {  sc  }
  ;

ty25:
  | ty=LCID  {  Typ.constr (Location.mknoloc (Longident.Lident ty)) []  }
  | LPAREN ty=LCID tail=ty28 RPAREN  {  let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail  }
  ;

ty26:
  | ty=LCID  {  Typ.constr (Location.mknoloc (Longident.Lident ty)) []  }
  | LPAREN ty=LCID tail=ty28 RPAREN  {  let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail  }
  ;

ty27:
  | ty=LCID  {  Typ.constr (Location.mknoloc (Longident.Lident ty)) []  }
  | LPAREN ty=LCID tail=ty28 RPAREN  {  let ty = if ty = "or" then "or_" else ty in Typ.constr (Location.mknoloc (Longident.Lident ty)) tail  }
  ;

ty28:
  |   {  []  }
  | ty=ty25 tyds=ty28  {  ty::tyds  }
  ;

ty29:
  | ty=ty25 tyds=ty28  {  ty::tyds  }
  |   {  []  }
  ;

ty30:
  |   {  []  }
  | ty=ty25 tyds=ty28  {  ty::tyds  }
  ;

ty31:
  |   {  []  }
  | ty=ty25 tyds=ty28  {  ty::tyds  }
  ;
