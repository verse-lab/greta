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
%token EOF%start <Ast.t> expr456
%%

and_517:
  | chai276  {  $1  }
  ;

and_518:
  | chai276  {  $1  }
  ;

and_519:
  | chai276  {  $1  }
  ;

and_520:
  | chai276  {  $1  }
  ;

and_521:
  | chai276  {  $1  }
  ;

and_522:
  | chai276  {  $1  }
  ;

and_523:
  | chai276  {  $1  }
  ;

and_524:
  | chai276  {  $1  }
  ;

and_525:
  | chai276  {  $1  }
  ;

and_526:
  | chai276  {  $1  }
  ;

and_527:
  | chai276  {  $1  }
  ;

and_528:
  | chai276  {  $1  }
  ;

arit304:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit305:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit306:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit307:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit308:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit309:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit310:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit311:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit312:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit313:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit314:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit315:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit316:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit317:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit318:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit319:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit320:
  | SUB  {  Ast.Sub  }
  | ADD  {  Ast.Add  }
  ;

arit321:
  | ADD  {  Ast.Add  }
  | SUB  {  Ast.Sub  }
  ;

arit501:
  | chai248  {  $1  }
  ;

arit502:
  | chai248  {  $1  }
  ;

arit503:
  | chai248  {  $1  }
  ;

arit504:
  | chai248  {  $1  }
  ;

arit505:
  | chai248  {  $1  }
  ;

arit506:
  | chai248  {  $1  }
  ;

arit507:
  | chai248  {  $1  }
  ;

arit508:
  | chai248  {  $1  }
  ;

arit509:
  | chai248  {  $1  }
  ;

arit510:
  | chai248  {  $1  }
  ;

arit511:
  | chai248  {  $1  }
  ;

arit512:
  | chai248  {  $1  }
  ;

arit513:
  | chai248  {  $1  }
  ;

arit514:
  | chai248  {  $1  }
  ;

arit515:
  | chai248  {  $1  }
  ;

arit516:
  | chai248  {  $1  }
  ;

atom1:
  | LPAREN test55 RPAREN  {  $2  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | name446  {  $1  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | s=STRING  {  Ast.Str s  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  ;

atom10:
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | s=STRING  {  Ast.Str s  }
  | name446  {  $1  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | LPAREN test55 RPAREN  {  $2  }
  ;

atom11:
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | s=STRING  {  Ast.Str s  }
  | name446  {  $1  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | LPAREN test55 RPAREN  {  $2  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  ;

atom12:
  | LBRACKET RBRACKET  {  Ast.List []  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | s=STRING  {  Ast.Str s  }
  | name446  {  $1  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LPAREN test55 RPAREN  {  $2  }
  ;

atom13:
  | LBRACKET RBRACKET  {  Ast.List []  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LPAREN test55 RPAREN  {  $2  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | name446  {  $1  }
  | s=STRING  {  Ast.Str s  }
  ;

atom14:
  | LPAREN test55 RPAREN  {  $2  }
  | s=STRING  {  Ast.Str s  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | name446  {  $1  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  ;

atom15:
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | s=STRING  {  Ast.Str s  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | name446  {  $1  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LPAREN test55 RPAREN  {  $2  }
  ;

atom16:
  | s=STRING  {  Ast.Str s  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | LPAREN test55 RPAREN  {  $2  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | name446  {  $1  }
  ;

atom17:
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | name446  {  $1  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LPAREN test55 RPAREN  {  $2  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | s=STRING  {  Ast.Str s  }
  ;

atom18:
  | s=STRING  {  Ast.Str s  }
  | name446  {  $1  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LPAREN test55 RPAREN  {  $2  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  ;

atom19:
  | LPAREN test55 RPAREN  {  $2  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | s=STRING  {  Ast.Str s  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | name446  {  $1  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  ;

atom2:
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | s=STRING  {  Ast.Str s  }
  | name446  {  $1  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | LPAREN test55 RPAREN  {  $2  }
  ;

atom20:
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | s=STRING  {  Ast.Str s  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | LPAREN test55 RPAREN  {  $2  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | name446  {  $1  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  ;

atom21:
  | name446  {  $1  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | LPAREN test55 RPAREN  {  $2  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | s=STRING  {  Ast.Str s  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  ;

atom22:
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LPAREN test55 RPAREN  {  $2  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | name446  {  $1  }
  | s=STRING  {  Ast.Str s  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  ;

atom3:
  | name446  {  $1  }
  | s=STRING  {  Ast.Str s  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LPAREN test55 RPAREN  {  $2  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  ;

atom4:
  | s=STRING  {  Ast.Str s  }
  | name446  {  $1  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LPAREN test55 RPAREN  {  $2  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  ;

atom5:
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | name446  {  $1  }
  | LPAREN test55 RPAREN  {  $2  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | s=STRING  {  Ast.Str s  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  ;

atom6:
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | s=STRING  {  Ast.Str s  }
  | name446  {  $1  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | LPAREN test55 RPAREN  {  $2  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  ;

atom7:
  | LBRACKET RBRACKET  {  Ast.List []  }
  | name446  {  $1  }
  | LPAREN test55 RPAREN  {  $2  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | s=STRING  {  Ast.Str s  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  ;

atom8:
  | name446  {  $1  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | LPAREN test55 RPAREN  {  $2  }
  | s=STRING  {  Ast.Str s  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET RBRACKET  {  Ast.List []  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  ;

atom9:
  | LBRACKET RBRACKET  {  Ast.List []  }
  | BITSIZEOF LT typeName=type345 GT  {  Ast.BitSizeOfType{typeName}  }
  | SIZEOF LT typeName=type345 GT  {  Ast.ByteSizeOfType{typeName}  }
  | i=INT  {  Ast.IntNum (int_of_string i)  }
  | LBRACKET sepa127 RBRACKET  {  Ast.List $2  }
  | s=STRING  {  Ast.Str s  }
  | f=FLOAT  {  Ast.FloatNum (float_of_string f)  }
  | name446  {  $1  }
  | LPAREN test55 RPAREN  {  $2  }
  ;

bita487:
  | BITAND  {  Ast.BitAnd  }
  ;

bita488:
  | BITAND  {  Ast.BitAnd  }
  ;

bita489:
  | BITAND  {  Ast.BitAnd  }
  ;

bita490:
  | BITAND  {  Ast.BitAnd  }
  ;

bita491:
  | BITAND  {  Ast.BitAnd  }
  ;

bita492:
  | BITAND  {  Ast.BitAnd  }
  ;

bita493:
  | BITAND  {  Ast.BitAnd  }
  ;

bita494:
  | BITAND  {  Ast.BitAnd  }
  ;

bita495:
  | BITAND  {  Ast.BitAnd  }
  ;

bita496:
  | BITAND  {  Ast.BitAnd  }
  ;

bita497:
  | BITAND  {  Ast.BitAnd  }
  ;

bita498:
  | BITAND  {  Ast.BitAnd  }
  ;

bita499:
  | BITAND  {  Ast.BitAnd  }
  ;

bita500:
  | BITAND  {  Ast.BitAnd  }
  ;

bito477:
  | BITOR  {  Ast.BitOr  }
  ;

bito478:
  | BITOR  {  Ast.BitOr  }
  ;

bito479:
  | BITOR  {  Ast.BitOr  }
  ;

bito480:
  | BITOR  {  Ast.BitOr  }
  ;

bito481:
  | BITOR  {  Ast.BitOr  }
  ;

bito482:
  | BITOR  {  Ast.BitOr  }
  ;

bito483:
  | BITOR  {  Ast.BitOr  }
  ;

bito484:
  | BITOR  {  Ast.BitOr  }
  ;

bito485:
  | BITOR  {  Ast.BitOr  }
  ;

bito486:
  | BITOR  {  Ast.BitOr  }
  ;

bitx465:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx466:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx467:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx468:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx469:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx470:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx471:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx472:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx473:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx474:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx475:
  | BITXOR  {  Ast.BitXor  }
  ;

bitx476:
  | BITXOR  {  Ast.BitXor  }
  ;

chai220:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai221:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai222:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai223:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai224:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai225:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai226:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai227:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai228:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai229:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai230:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai231:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai232:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai233:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai234:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai235:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai236:
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | fact74  {  $1  }
  ;

chai237:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai238:
  | fact74  {  $1  }
  | fact74 term95 chai238  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai239:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai240:
  | term368  {  $1  }
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai241:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai242:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai243:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai244:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai245:
  | term368  {  $1  }
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai246:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai247:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai248:
  | term368  {  $1  }
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai249:
  | term368  {  $1  }
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai250:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai251:
  | term368  {  $1  }
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai252:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai253:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai254:
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | term368  {  $1  }
  ;

chai255:
  | term368  {  $1  }
  | term368 arit316 chai251  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai256:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai257:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai258:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai259:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai260:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai261:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai262:
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | arit512  {  $1  }
  ;

chai263:
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | arit512  {  $1  }
  ;

chai264:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai265:
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | arit512  {  $1  }
  ;

chai266:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai267:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai268:
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | arit512  {  $1  }
  ;

chai269:
  | arit512  {  $1  }
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai270:
  | arit512 shif109 chai266  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | arit512  {  $1  }
  ;

chai271:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai272:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai273:
  | shif378  {  $1  }
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai274:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai275:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai276:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai277:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai278:
  | shif378  {  $1  }
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai279:
  | shif378  {  $1  }
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai280:
  | shif378  {  $1  }
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai281:
  | shif378  {  $1  }
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai282:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai283:
  | shif378 bita491 chai274  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | shif378  {  $1  }
  ;

chai284:
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_523  {  $1  }
  ;

chai285:
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_523  {  $1  }
  ;

chai286:
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_523  {  $1  }
  ;

chai287:
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_523  {  $1  }
  ;

chai288:
  | and_523  {  $1  }
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai289:
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_523  {  $1  }
  ;

chai290:
  | and_523  {  $1  }
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai291:
  | and_523  {  $1  }
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai292:
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_523  {  $1  }
  ;

chai293:
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | and_523  {  $1  }
  ;

chai294:
  | and_523  {  $1  }
  | and_523 bitx471 chai289  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai295:
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_323  {  $1  }
  ;

chai296:
  | xor_323  {  $1  }
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai297:
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_323  {  $1  }
  ;

chai298:
  | xor_323  {  $1  }
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai299:
  | xor_323  {  $1  }
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai300:
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_323  {  $1  }
  ;

chai301:
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_323  {  $1  }
  ;

chai302:
  | xor_323  {  $1  }
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  ;

chai303:
  | xor_323 bito478 chai296  {  Ast.BinOp { left = $1; op = $2; right = $3}  }
  | xor_323  {  $1  }
  ;

comp213:
  | expr460  {  $1  }
  | left=expr460 ops=comp26 right=expr460  {  Ast.Compare {left;ops;right}  }
  ;

comp214:
  | left=expr460 ops=comp26 right=expr460  {  Ast.Compare {left;ops;right}  }
  | expr460  {  $1  }
  ;

comp215:
  | expr460  {  $1  }
  | left=expr460 ops=comp26 right=expr460  {  Ast.Compare {left;ops;right}  }
  ;

comp216:
  | left=expr460 ops=comp26 right=expr460  {  Ast.Compare {left;ops;right}  }
  | expr460  {  $1  }
  ;

comp217:
  | expr460  {  $1  }
  | left=expr460 ops=comp26 right=expr460  {  Ast.Compare {left;ops;right}  }
  ;

comp218:
  | left=expr460 ops=comp26 right=expr460  {  Ast.Compare {left;ops;right}  }
  | expr460  {  $1  }
  ;

comp219:
  | left=expr460 ops=comp26 right=expr460  {  Ast.Compare {left;ops;right}  }
  | expr460  {  $1  }
  ;

comp23:
  | LT  {  Ast.Lt  }
  | GT  {  Ast.Gt  }
  | NOTEQ  {  Ast.NotEq  }
  | EQ  {  Ast.Eq  }
  | GTE  {  Ast.GtE  }
  | LTE  {  Ast.LtE  }
  ;

comp24:
  | NOTEQ  {  Ast.NotEq  }
  | GTE  {  Ast.GtE  }
  | LT  {  Ast.Lt  }
  | GT  {  Ast.Gt  }
  | LTE  {  Ast.LtE  }
  | EQ  {  Ast.Eq  }
  ;

comp25:
  | GT  {  Ast.Gt  }
  | LT  {  Ast.Lt  }
  | NOTEQ  {  Ast.NotEq  }
  | GTE  {  Ast.GtE  }
  | LTE  {  Ast.LtE  }
  | EQ  {  Ast.Eq  }
  ;

comp26:
  | LTE  {  Ast.LtE  }
  | LT  {  Ast.Lt  }
  | EQ  {  Ast.Eq  }
  | GTE  {  Ast.GtE  }
  | NOTEQ  {  Ast.NotEq  }
  | GT  {  Ast.Gt  }
  ;

comp27:
  | LT  {  Ast.Lt  }
  | GTE  {  Ast.GtE  }
  | NOTEQ  {  Ast.NotEq  }
  | EQ  {  Ast.Eq  }
  | GT  {  Ast.Gt  }
  | LTE  {  Ast.LtE  }
  ;

comp28:
  | NOTEQ  {  Ast.NotEq  }
  | LT  {  Ast.Lt  }
  | GT  {  Ast.Gt  }
  | EQ  {  Ast.Eq  }
  | LTE  {  Ast.LtE  }
  | GTE  {  Ast.GtE  }
  ;

comp29:
  | LT  {  Ast.Lt  }
  | EQ  {  Ast.Eq  }
  | GTE  {  Ast.GtE  }
  | NOTEQ  {  Ast.NotEq  }
  | GT  {  Ast.Gt  }
  | LTE  {  Ast.LtE  }
  ;

comp30:
  | LTE  {  Ast.LtE  }
  | EQ  {  Ast.Eq  }
  | LT  {  Ast.Lt  }
  | GTE  {  Ast.GtE  }
  | NOTEQ  {  Ast.NotEq  }
  | GT  {  Ast.Gt  }
  ;

expr456:
  | test54 EOF  {  $1  }
  ;

expr457:
  | chai297  {  $1  }
  ;

expr458:
  | chai297  {  $1  }
  ;

expr459:
  | chai297  {  $1  }
  ;

expr460:
  | chai297  {  $1  }
  ;

expr461:
  | chai297  {  $1  }
  ;

expr462:
  | chai297  {  $1  }
  ;

expr463:
  | chai297  {  $1  }
  ;

expr464:
  | chai297  {  $1  }
  ;

fact56:
  | ADD fact75  {  $2  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  ;

fact57:
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  | ADD fact75  {  $2  }
  ;

fact58:
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | powe408  {  $1  }
  | ADD fact75  {  $2  }
  ;

fact59:
  | powe408  {  $1  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | ADD fact75  {  $2  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  ;

fact60:
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | ADD fact75  {  $2  }
  | powe408  {  $1  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  ;

fact61:
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | ADD fact75  {  $2  }
  | powe408  {  $1  }
  ;

fact62:
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  | ADD fact75  {  $2  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  ;

fact63:
  | ADD fact75  {  $2  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  ;

fact64:
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  | ADD fact75  {  $2  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  ;

fact65:
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | powe408  {  $1  }
  | ADD fact75  {  $2  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  ;

fact66:
  | ADD fact75  {  $2  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | powe408  {  $1  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  ;

fact67:
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | ADD fact75  {  $2  }
  ;

fact68:
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | ADD fact75  {  $2  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  ;

fact69:
  | powe408  {  $1  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | ADD fact75  {  $2  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  ;

fact70:
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | powe408  {  $1  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | ADD fact75  {  $2  }
  ;

fact71:
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | powe408  {  $1  }
  | ADD fact75  {  $2  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  ;

fact72:
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | ADD fact75  {  $2  }
  | powe408  {  $1  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  ;

fact73:
  | powe408  {  $1  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | ADD fact75  {  $2  }
  ;

fact74:
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | ADD fact75  {  $2  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | powe408  {  $1  }
  ;

fact75:
  | ADD fact75  {  $2  }
  | TILDE fact75  {  Ast.(UnaryOp {op = Invert; operand = $2})  }
  | SUB fact75  {  Ast.(UnaryOp {op = Minus; operand = $2})  }
  | powe408  {  $1  }
  ;

list191:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list192:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list193:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list194:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list195:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list196:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list197:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list198:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list199:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list200:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list201:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list202:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list203:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list204:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list205:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list206:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list207:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list208:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list209:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list210:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

list211:
  | trai44 list204  {  $1 :: $2  }
  |   {  []  }
  ;

list212:
  |   {  []  }
  | trai44 list204  {  $1 :: $2  }
  ;

name433:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name434:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name435:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name436:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name437:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name438:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name439:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name440:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name441:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name442:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name443:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name444:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name445:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name446:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name447:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name448:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name449:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name450:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name451:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name452:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name453:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name454:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

name455:
  | COLON2 sepa160  {  match Some(None), $2 with | _, [] -> assert false | None, [ "true" ] -> Ast.Bool true | None, [ "false" ] -> Ast.Bool false | None, [ name ] -> Ast.Name name | None, [ enumName; label ] -> (Ast.(EnumByLabel { label; enumName; inType = empty_typeId })) | prefix , path -> (let path, enumName, label = match List.rev path with | [] | [_] | [_;_] -> assert false | label :: enunName :: path_rev -> List.rev path_rev, enunName, label in let absolute = Option.is_some prefix in let inType = Ast.{ absolute; isArray = false; names = path } in Ast.(EnumByLabel { label; enumName; inType}))  }
  ;

not_185:
  | comp218  {  $1  }
  | NOT not_189  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  ;

not_186:
  | NOT not_189  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  | comp218  {  $1  }
  ;

not_187:
  | NOT not_189  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  | comp218  {  $1  }
  ;

not_188:
  | comp218  {  $1  }
  | NOT not_189  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  ;

not_189:
  | comp218  {  $1  }
  | NOT not_189  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  ;

not_190:
  | NOT not_189  {  Ast.(UnaryOp { op = Ast.Not; operand = $2 })  }
  | comp218  {  $1  }
  ;

pair409:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair410:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair411:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair412:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair413:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair414:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair415:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair416:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair417:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair418:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair419:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair420:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair421:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair422:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair423:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair424:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair425:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair426:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair427:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair428:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair429:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair430:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair431:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

pair432:
  | LBRACKET LBRACKET  {  Some ((), ())  }
  ;

powe388:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe389:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe390:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe391:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe392:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe393:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe394:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe395:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe396:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe397:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe398:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe399:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe400:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe401:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe402:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe403:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe404:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe405:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe406:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe407:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

powe408:
  | atom2 list192  {  List.fold_left (fun acc f -> f acc) $1 $2  }
  ;

sepa114:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa115:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa116:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa117:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa118:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa119:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa120:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa121:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa122:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa123:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa124:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa125:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa126:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa127:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa128:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa129:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa130:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa131:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa132:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa133:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa134:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa135:
  | test55 COMMA sepa132  {  $1 :: $3  }
  | test55  {  [$1]  }
  ;

sepa136:
  | test55  {  [$1]  }
  | test55 COMMA sepa132  {  $1 :: $3  }
  ;

sepa137:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa138:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa139:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa140:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa141:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa142:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa143:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa144:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa145:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa146:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa147:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa148:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa149:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa150:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa151:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa152:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa153:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa154:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa155:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa156:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa157:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa158:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa159:
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  | IDENT  {  [$1]  }
  ;

sepa160:
  | IDENT  {  [$1]  }
  | IDENT COLON2 sepa155  {  $1 :: $3  }
  ;

sepa161:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa162:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa163:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa164:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa165:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa166:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa167:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa168:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa169:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa170:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa171:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa172:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa173:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa174:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa175:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa176:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa177:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa178:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa179:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa180:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa181:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa182:
  |   {  []  }
  | sepa136  {  $1  }
  ;

sepa183:
  | sepa136  {  $1  }
  |   {  []  }
  ;

sepa184:
  | sepa136  {  $1  }
  |   {  []  }
  ;

shif100:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif101:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif102:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif103:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif104:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif105:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif106:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif107:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif108:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif109:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif110:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif111:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif112:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif113:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

shif374:
  | chai259  {  $1  }
  ;

shif375:
  | chai259  {  $1  }
  ;

shif376:
  | chai259  {  $1  }
  ;

shif377:
  | chai259  {  $1  }
  ;

shif378:
  | chai259  {  $1  }
  ;

shif379:
  | chai259  {  $1  }
  ;

shif380:
  | chai259  {  $1  }
  ;

shif381:
  | chai259  {  $1  }
  ;

shif382:
  | chai259  {  $1  }
  ;

shif383:
  | chai259  {  $1  }
  ;

shif384:
  | chai259  {  $1  }
  ;

shif385:
  | chai259  {  $1  }
  ;

shif386:
  | chai259  {  $1  }
  ;

shif387:
  | chai259  {  $1  }
  ;

shif98:
  | LSHIFT  {  Ast.LShift  }
  | RSHIFT  {  Ast.RShift  }
  ;

shif99:
  | RSHIFT  {  Ast.RShift  }
  | LSHIFT  {  Ast.LShift  }
  ;

term356:
  | chai237  {  $1  }
  ;

term357:
  | chai237  {  $1  }
  ;

term358:
  | chai237  {  $1  }
  ;

term359:
  | chai237  {  $1  }
  ;

term360:
  | chai237  {  $1  }
  ;

term361:
  | chai237  {  $1  }
  ;

term362:
  | chai237  {  $1  }
  ;

term363:
  | chai237  {  $1  }
  ;

term364:
  | chai237  {  $1  }
  ;

term365:
  | chai237  {  $1  }
  ;

term366:
  | chai237  {  $1  }
  ;

term367:
  | chai237  {  $1  }
  ;

term368:
  | chai237  {  $1  }
  ;

term369:
  | chai237  {  $1  }
  ;

term370:
  | chai237  {  $1  }
  ;

term371:
  | chai237  {  $1  }
  ;

term372:
  | chai237  {  $1  }
  ;

term373:
  | chai237  {  $1  }
  ;

term77:
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  ;

term78:
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  | DIV  {  Ast.Div  }
  ;

term79:
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  | MOD  {  Ast.Mod  }
  ;

term80:
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  | MOD  {  Ast.Mod  }
  ;

term81:
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  | DIV  {  Ast.Div  }
  ;

term82:
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  ;

term83:
  | MUL  {  Ast.Mult  }
  | DIV  {  Ast.Div  }
  | MOD  {  Ast.Mod  }
  ;

term84:
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  ;

term85:
  | MUL  {  Ast.Mult  }
  | DIV  {  Ast.Div  }
  | MOD  {  Ast.Mod  }
  ;

term86:
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  | DIV  {  Ast.Div  }
  ;

term87:
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  | DIV  {  Ast.Div  }
  ;

term88:
  | MUL  {  Ast.Mult  }
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  ;

term89:
  | DIV  {  Ast.Div  }
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  ;

term90:
  | DIV  {  Ast.Div  }
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  ;

term91:
  | DIV  {  Ast.Div  }
  | MOD  {  Ast.Mod  }
  | MUL  {  Ast.Mult  }
  ;

term92:
  | MUL  {  Ast.Mult  }
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  ;

term93:
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  | MOD  {  Ast.Mod  }
  ;

term94:
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  ;

term95:
  | MUL  {  Ast.Mult  }
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  ;

term96:
  | MOD  {  Ast.Mod  }
  | DIV  {  Ast.Div  }
  | MUL  {  Ast.Mult  }
  ;

test355:
  | not_190  {  $1  }
  ;

test54:
  | test355 OR test97  {  Ast.(BoolOp { op = Or; values = [$1; $3] })  }
  | test55 AND test76  {  Ast.(BoolOp { op = And; values = [$1; $3] })  }
  | condition=test97 QUESTION ifTrue=test76 COLON ifFalse=test76  {  Ast.(IfExp {condition; ifTrue; ifFalse})  }
  | not_190  {  $1  }
  ;

test55:
  | test355 OR test97  {  Ast.(BoolOp { op = Or; values = [$1; $3] })  }
  | condition=test97 QUESTION ifTrue=test76 COLON ifFalse=test76  {  Ast.(IfExp {condition; ifTrue; ifFalse})  }
  | not_190  {  $1  }
  | test55 AND test76  {  Ast.(BoolOp { op = And; values = [$1; $3] })  }
  ;

test76:
  | condition=test97 QUESTION ifTrue=test76 COLON ifFalse=test76  {  Ast.(IfExp {condition; ifTrue; ifFalse})  }
  | test355 OR test97  {  Ast.(BoolOp { op = Or; values = [$1; $3] })  }
  | not_190  {  $1  }
  ;

test97:
  | test355 OR test97  {  Ast.(BoolOp { op = Or; values = [$1; $3] })  }
  | not_190  {  $1  }
  ;

trai31:
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  ;

trai32:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  ;

trai33:
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  ;

trai34:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

trai35:
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  ;

trai36:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  ;

trai37:
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  ;

trai38:
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

trai39:
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

trai40:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  ;

trai41:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  ;

trai42:
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  ;

trai43:
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  ;

trai44:
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  ;

trai45:
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  ;

trai46:
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  ;

trai47:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  ;

trai48:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

trai49:
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  ;

trai50:
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  ;

trai51:
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  ;

trai52:
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  ;

trai53:
  | DOT IDENT  {  fun x -> Ast.Attribute { value = x; attr = $2 }  }
  | DOT AS LT typeName=type354 GT  {  fun x -> Ast.CastToType { value = x; typeName }  }
  | LPAREN args=sepa184 RPAREN  {  fun func -> Ast.Call { func; args }  }
  | LBRACKET test55 RBRACKET  {  fun x -> Ast.Subscript { value = x;  idx = $2 } }
  ;

type332:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type333:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type334:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type335:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type336:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type337:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type338:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type339:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type340:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type341:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type342:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type343:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type344:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type345:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type346:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type347:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type348:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type349:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type350:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type351:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type352:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type353:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

type354:
  | COLON2 sepa160 pair432  {  let absolute = true in Ast.({ absolute; isArray = false; names = $2 })  }
  ;

xor_322:
  | chai287  {  $1  }
  ;

xor_323:
  | chai287  {  $1  }
  ;

xor_324:
  | chai287  {  $1  }
  ;

xor_325:
  | chai287  {  $1  }
  ;

xor_326:
  | chai287  {  $1  }
  ;

xor_327:
  | chai287  {  $1  }
  ;

xor_328:
  | chai287  {  $1  }
  ;

xor_329:
  | chai287  {  $1  }
  ;

xor_330:
  | chai287  {  $1  }
  ;

xor_331:
  | chai287  {  $1  }
  ;
