
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 23 "grammars-revamp/G2/G2c.mly"
       (Range.t * string)
# 15 "grammars-revamp/G2/G2c.ml"
  )
    | TRUE of (
# 30 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 20 "grammars-revamp/G2/G2c.ml"
  )
    | TILDE of (
# 29 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 25 "grammars-revamp/G2/G2c.ml"
  )
    | RPAREN of (
# 28 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 30 "grammars-revamp/G2/G2c.ml"
  )
    | LPAREN of (
# 27 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 35 "grammars-revamp/G2/G2c.ml"
  )
    | FALSE of (
# 31 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 40 "grammars-revamp/G2/G2c.ml"
  )
    | EOF
    | BAR of (
# 25 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 46 "grammars-revamp/G2/G2c.ml"
  )
    | ARR of (
# 24 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 51 "grammars-revamp/G2/G2c.ml"
  )
    | AMPER of (
# 26 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 56 "grammars-revamp/G2/G2c.ml"
  )
  
end

include MenhirBasics

# 18 "grammars-revamp/G2/G2c.mly"
  
open Ast;;

# 67 "grammars-revamp/G2/G2c.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_toplevel) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: toplevel. *)

  | MenhirState03 : (('s, _menhir_box_toplevel) _menhir_cell1_TILDE, _menhir_box_toplevel) _menhir_state
    (** State 03.
        Stack shape : TILDE.
        Start symbol: toplevel. *)

  | MenhirState04 : (('s, _menhir_box_toplevel) _menhir_cell1_LPAREN, _menhir_box_toplevel) _menhir_state
    (** State 04.
        Stack shape : LPAREN.
        Start symbol: toplevel. *)

  | MenhirState08 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp _menhir_cell0_BAR, _menhir_box_toplevel) _menhir_state
    (** State 08.
        Stack shape : bexp BAR.
        Start symbol: toplevel. *)

  | MenhirState10 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp _menhir_cell0_ARR, _menhir_box_toplevel) _menhir_state
    (** State 10.
        Stack shape : bexp ARR.
        Start symbol: toplevel. *)

  | MenhirState12 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp _menhir_cell0_AMPER, _menhir_box_toplevel) _menhir_state
    (** State 12.
        Stack shape : bexp AMPER.
        Start symbol: toplevel. *)


and ('s, 'r) _menhir_cell1_bexp = 
  | MenhirCell1_bexp of 's * ('s, 'r) _menhir_state * (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 105 "grammars-revamp/G2/G2c.ml"
)

and 's _menhir_cell0_AMPER = 
  | MenhirCell0_AMPER of 's * (
# 26 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 112 "grammars-revamp/G2/G2c.ml"
)

and 's _menhir_cell0_ARR = 
  | MenhirCell0_ARR of 's * (
# 24 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 119 "grammars-revamp/G2/G2c.ml"
)

and 's _menhir_cell0_BAR = 
  | MenhirCell0_BAR of 's * (
# 25 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 126 "grammars-revamp/G2/G2c.ml"
)

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * (
# 27 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 133 "grammars-revamp/G2/G2c.ml"
)

and ('s, 'r) _menhir_cell1_TILDE = 
  | MenhirCell1_TILDE of 's * ('s, 'r) _menhir_state * (
# 29 "grammars-revamp/G2/G2c.mly"
       (Range.t)
# 140 "grammars-revamp/G2/G2c.ml"
)

and _menhir_box_toplevel = 
  | MenhirBox_toplevel of (
# 36 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 147 "grammars-revamp/G2/G2c.ml"
) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 44 "grammars-revamp/G2/G2c.mly"
                        ( True )
# 155 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 159 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_02 =
  fun () ->
    (
# 45 "grammars-revamp/G2/G2c.mly"
                        ( False )
# 167 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 171 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_03 =
  fun x ->
    (
# 46 "grammars-revamp/G2/G2c.mly"
                        ( Var (snd x) )
# 179 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 183 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_04 =
  fun l r ->
    (
# 47 "grammars-revamp/G2/G2c.mly"
                        ( Imp(l, r) )
# 191 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 195 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_05 =
  fun l r ->
    (
# 48 "grammars-revamp/G2/G2c.mly"
                        ( Or(l, r) )
# 203 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 207 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_06 =
  fun l r ->
    (
# 49 "grammars-revamp/G2/G2c.mly"
                        ( And(l, r) )
# 215 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 219 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_07 =
  fun b ->
    (
# 50 "grammars-revamp/G2/G2c.mly"
                        ( Not(b) )
# 227 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 231 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_08 =
  fun b ->
    (
# 51 "grammars-revamp/G2/G2c.mly"
                         ( b )
# 239 "grammars-revamp/G2/G2c.ml"
     : (
# 37 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 243 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_action_09 =
  fun b ->
    (
# 41 "grammars-revamp/G2/G2c.mly"
               ( b )
# 251 "grammars-revamp/G2/G2c.ml"
     : (
# 36 "grammars-revamp/G2/G2c.mly"
      (Ast.bexp)
# 255 "grammars-revamp/G2/G2c.ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AMPER _ ->
        "AMPER"
    | ARR _ ->
        "ARR"
    | BAR _ ->
        "BAR"
    | EOF ->
        "EOF"
    | FALSE _ ->
        "FALSE"
    | LPAREN _ ->
        "LPAREN"
    | RPAREN _ ->
        "RPAREN"
    | TILDE _ ->
        "TILDE"
    | TRUE _ ->
        "TRUE"
    | VAR _ ->
        "VAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_03 x in
      _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bexp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let b = _v in
          let _v = _menhir_action_09 b in
          MenhirBox_toplevel _v
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_BAR (_menhir_stack, _v) in
      let _menhir_s = MenhirState08 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE _ ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TILDE _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE _ ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_01 () in
      _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_TILDE (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE _ ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TILDE _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE _ ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState04 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE _ ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TILDE _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE _ ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_02 () in
      _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_ARR (_menhir_stack, _v) in
      let _menhir_s = MenhirState10 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE _ ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TILDE _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE _ ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_AMPER (_menhir_stack, _v) in
      let _menhir_s = MenhirState12 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE _ ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TILDE _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE _ ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_TILDE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | EOF | RPAREN _ ->
          let MenhirCell1_TILDE (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_07 b in
          _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_13 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp _menhir_cell0_AMPER as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | EOF | RPAREN _ ->
          let MenhirCell0_AMPER (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_06 l r in
          _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp _menhir_cell0_ARR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | EOF | RPAREN _ ->
          let MenhirCell0_ARR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_04 l r in
          _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp _menhir_cell0_BAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | EOF | RPAREN _ ->
          let MenhirCell0_BAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_05 l r in
          _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_08 b in
          _menhir_goto_bexp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BAR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | ARR _v_2 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | AMPER _v_3 ->
          let _menhir_stack = MenhirCell1_bexp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE _ ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TILDE _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE _ ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let toplevel =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_toplevel v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
