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
%token EOF%start <Ast.t> expr31
%%

and_37:
  | chai19  {  $1  }
  ;

arit22:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit36:
  | chai17  {  $1  }
  ;

atom1:
  | LPAREN test4 RPAREN  {  $2  }
  | s=STRING  {  Ast.Str s  }
  | name30  {  $1  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa10 RBRACKET  {  Ast.List $2  }
  | SIZEOF LT typeName=type24 GT  {  Ast.ByteSizeOfType{typeName}  }
  | BITSIZEOF LT typeName=type24 GT  {  Ast.BitSizeOfType{typeName}  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  ;

bita35:
  | BITAND  {  Ast.BitAnd  }
  ;

bito34:
  | BITOR  {  Ast.BitOr  }
  ;

bitx33:
  | BITXOR  {  Ast.BitXor  }
  ;

chai16:
  | fact5  {  $1  }
  | fact5 term7 chai16  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai17:
  | term26  {  $1  }
  | term26 arit22 chai17  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai18:
  | arit36 shif9 chai18  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | arit36  {  $1  }
  ;

chai19:
  | shif27  {  $1  }
  | shif27 bita35 chai19  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai20:
  | and_37 bitx33 chai20  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_37  {  $1  }
  ;

chai21:
  | xor_23 bito34 chai21  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_23  {  $1  }
  ;

comp15:
  | left=expr32 ops=comp2 right=expr32  {  Ast.Compare {left;ops;right}  }
  | expr32  {  $1  }
  ;

comp2:
  | LT  {  Ast.Lt  }
  | EQ  {  Ast.Eq  }
  | LTE  {  Ast.LtE  }
  | GTE  {  Ast.GtE  }
  | GT  {  Ast.Gt  }
  | NOTEQ  {  Ast.NotEq  }
  ;

expr31:
  | test4 EOF  {  $1  }
  ;

expr32:
  | chai21  {  $1  }
  ;

fact5:
  | ADD fact5  {  $2  }
  | powe28  {  $1  }
  | TILDE fact5  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | SUB fact5  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  ;

list14:
  |   {  []  }
  | trai3 list14  {  $1 :: $2  }
  ;

name30:
  | COLON2 sepa11  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

not_13:
  | NOT not_13  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  | comp15  {  $1  }
  ;

pair29:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

powe28:
  | atom1 list14  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

sepa10:
  | test4  {  [$1]  }
  | test4 COMMA sepa10  {  $1 :: $3  }
  ;

sepa11:
  | IDENT COLON2 sepa11  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa12:
  |   {  []  }
  | sepa10  {  $1  }
  ;

shif27:
  | chai18  {  $1  }
  ;

shif9:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

term26:
  | chai16  {  $1  }
  ;

term7:
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  ;

test25:
  | not_13  {  $1  }
  ;

test4:
  | condition=test4 QUESTION ifTrue=test4 COLON ifFalse=test6  {  Ast.(IfExp {condition; ifTrue; ifFalse})  }
  | test6  { $1 }
  ;

test6:
  | test8  { $1 }
  | test6 AND test8  {  Ast.(BoolOp { op = And; values = [$1; $3] })  }
  ;

test8:
  | test25 OR test8  {  Ast.(BoolOp { op = Or; values = [$1; $3] })  }
  | test25  { $1 }
  ;

trai3:
  | LPAREN args=sepa12 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type24 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test4 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

type24:
  | COLON2 sepa11 pair29  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

xor_23:
  | chai20  {  $1  }
  ;
