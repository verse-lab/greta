%{

(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Original implementation at
https://github.com/kaitai-io/kaitai_struct_compiler/blob/master/shared/src/main/scala/io/kaitai/struct/exprlang/Expressions.scala *)

open Types


%}
%token AND
%token OR
%token NOT
%token AS
%token SIZEOF
%token BITSIZEOF
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> IDENT
%token COLON2
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token QUESTION
%token COLON
%token LSHIFT
%token RSHIFT
%token LT
%token GT
%token EQ
%token GTE
%token LTE
%token NOTEQ
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token BITOR
%token BITAND
%token BITXOR
%token COMMA
%token TILDE
%token DOT
%token EOF%start <Ast.t> expr1
%%

and_32:
  | chai17  {  $1  }
  ;

arit13:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit28:
  | chai14  {  $1  }
  ;

atom1:
  | LPAREN test5 RPAREN  {  $2  }
  | s=STRING  {  Ast.Str s  }
  | name25  {  $1  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa10 RBRACKET  {  Ast.List $2  }
  | SIZEOF LT typeName=type24 GT  {  Ast.ByteSizeOfType{typeName}  }
  | BITSIZEOF LT typeName=type24 GT  {  Ast.BitSizeOfType{typeName}  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  ;

bita30:
  | BITAND  {  Ast.BitAnd  }
  ;

bito34:
  | BITOR  {  Ast.BitOr  }
  ;

bitx31:
  | BITXOR  {  Ast.BitXor  }
  ;

chai12:
  | fact4  {  $1  }
  | fact4 term6 chai12  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai14:
  | term27  {  $1  }
  | term27 arit13 chai14  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai16:
  | arit28  {  $1  }
  | arit28 shif15 chai16  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai17:
  | shif29  {  $1  }
  | shif29 bita30 chai17  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai18:
  | and_32  {  $1  }
  | and_32 bitx31 chai18  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai19:
  | xor_33 bito34 chai19  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_33  {  $1  }
  ;

comp2:
  | LT  {  Ast.Lt  }
  | EQ  {  Ast.Eq  }
  | LTE  {  Ast.LtE  }
  | GTE  {  Ast.GtE  }
  | GT  {  Ast.Gt  }
  | NOTEQ  {  Ast.NotEq  }
  ;

comp20:
  | expr35  {  $1  }
  | left=expr35 ops=comp2 right=expr35  {  Ast.Compare {left;ops;right}  }
  ;

expr1:
  | test5 EOF  {  $1  }
  ;

expr35:
  | chai19  {  $1  }
  ;

fact4:
  | TILDE fact4  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | SUB fact4  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | powe26  {  $1  }
  | ADD fact4  {  $2  }
  ;

list11:
  |   {  []  }
  | trai3 list11  {  $1 :: $2  }
  ;

name25:
  | COLON2 sepa9  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

not_21:
  | comp20  {  $1  }
  | NOT not_21  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  ;

pair23:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

powe26:
  | atom1 list11  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

sepa10:
  | test5  {  [$1]  }
  | test5 COMMA sepa10  {  $1 :: $3  }
  ;

sepa8:
  |   {  []  }
  | sepa10  {  $1  }
  ;

sepa9:
  | IDENT COLON2 sepa9  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

shif15:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif29:
  | chai16  {  $1  }
  ;

term27:
  | chai12  {  $1  }
  ;

term6:
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  ;

test22:
  | test36  { $1 }
  | condition=test36 QUESTION ifTrue=test22 COLON ifFalse=test22  {  Ast.(IfExp {condition; ifTrue; ifFalse})  }
  ;

test36:
  | not_21  {  $1  }
  ;

test5:
  | test7  { $1 }
  | test5 AND test7  {  Ast.(BoolOp { op = And; values = [$1; $3] })  }
  ;

test7:
  | test22 OR test7  {  Ast.(BoolOp { op = Or; values = [$1; $3] })  }
  | test22  { $1 }
  ;

trai3:
  | LPAREN args=sepa8 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type24 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test5 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

type24:
  | COLON2 sepa9 pair23  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

xor_33:
  | chai18  {  $1  }
  ;
