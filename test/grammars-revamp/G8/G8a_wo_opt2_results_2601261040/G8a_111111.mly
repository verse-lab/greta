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

and_46:
  | chai27  {  $1  }
  ;

arit21:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit42:
  | chai22  {  $1  }
  ;

atom1:
  | LPAREN test6 RPAREN  {  $2  }
  | s=STRING  {  Ast.Str s  }
  | name39  {  $1  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa16 RBRACKET  {  Ast.List $2  }
  | SIZEOF LT typeName=type38 GT  {  Ast.ByteSizeOfType{typeName}  }
  | BITSIZEOF LT typeName=type38 GT  {  Ast.BitSizeOfType{typeName}  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  ;

bita44:
  | BITAND  {  Ast.BitAnd  }
  ;

bito48:
  | BITOR  {  Ast.BitOr  }
  ;

bitx45:
  | BITXOR  {  Ast.BitXor  }
  ;

chai18:
  | chai19  { $1 }
  ;

chai19:
  | fact5 term8 chai18  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact5  {  $1  }
  ;

chai20:
  | chai22  { $1 }
  ;

chai22:
  | term41 arit21 chai20  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term41  {  $1  }
  ;

chai23:
  | chai25  { $1 }
  ;

chai25:
  | arit42 shif24 chai23  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | arit42  {  $1  }
  ;

chai26:
  | chai27  { $1 }
  ;

chai27:
  | shif43 bita44 chai26  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif43  {  $1  }
  ;

chai28:
  | chai29  { $1 }
  ;

chai29:
  | and_46  {  $1  }
  | and_46 bitx45 chai28  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai30:
  | chai31  { $1 }
  ;

chai31:
  | xor_47 bito48 chai30  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_47  {  $1  }
  ;

comp2:
  | LT  {  Ast.Lt  }
  | EQ  {  Ast.Eq  }
  | LTE  {  Ast.LtE  }
  | GTE  {  Ast.GtE  }
  | GT  {  Ast.Gt  }
  | NOTEQ  {  Ast.NotEq  }
  ;

comp32:
  | left=expr49 ops=comp2 right=expr49  {  Ast.Compare {left;ops;right}  }
  | expr49  {  $1  }
  ;

expr1:
  | test7 EOF  {  $1  }
  ;

expr49:
  | chai31  {  $1  }
  ;

fact4:
  | fact5  { $1 }
  ;

fact5:
  | ADD fact4  {  $2  }
  | powe40  {  $1  }
  | TILDE fact4  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | SUB fact4  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  ;

list15:
  | list17  { $1 }
  ;

list17:
  |   {  []  }
  | trai3 list15  {  $1 :: $2  }
  ;

name39:
  | COLON2 sepa14  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

not_33:
  | not_35  { $1 }
  ;

not_35:
  | comp32  {  $1  }
  | NOT not_33  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  ;

pair37:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

powe40:
  | atom1 list17  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

sepa10:
  | sepa13  { $1 }
  ;

sepa11:
  | sepa14  { $1 }
  ;

sepa12:
  |   {  []  }
  | sepa10  {  $1  }
  ;

sepa13:
  | sepa16  { $1 }
  ;

sepa14:
  | IDENT COLON2 sepa11  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa16:
  | test6 COMMA sepa13  {  $1 :: $3  }
  | test6  {  [$1]  }
  ;

shif24:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif43:
  | chai25  {  $1  }
  ;

term41:
  | chai19  {  $1  }
  ;

term8:
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  | DIV  {  Ast.Div  }
  ;

test34:
  | test50  { $1 }
  | condition=test50 QUESTION ifTrue=test34 COLON ifFalse=test34  {  Ast.(IfExp {condition; ifTrue; ifFalse})  }
  ;

test50:
  | not_35  {  $1  }
  ;

test6:
  | test7  { $1 }
  ;

test7:
  | test9 OR test6  {  Ast.(BoolOp { op = Or; values = [$1; $3] })  }
  | test9  { $1 }
  ;

test9:
  | test34  { $1 }
  | test34 AND test9  {  Ast.(BoolOp { op = And; values = [$1; $3] })  }
  ;

trai3:
  | LPAREN args=sepa12 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type36 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test6 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

type36:
  | type38  { $1 }
  ;

type38:
  | COLON2 sepa14 pair37  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

xor_47:
  | chai29  {  $1  }
  ;
