
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 17 "grammars/G1/G1s/G1d.mly"
       (Range.t * string)
# 15 "grammars/G1/G1s/G1d.ml"
  )
    | TRUE of (
# 24 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 20 "grammars/G1/G1s/G1d.ml"
  )
    | TILDE of (
# 23 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 25 "grammars/G1/G1s/G1d.ml"
  )
    | RPAREN of (
# 22 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 30 "grammars/G1/G1s/G1d.ml"
  )
    | LPAREN of (
# 21 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 35 "grammars/G1/G1s/G1d.ml"
  )
    | FALSE of (
# 25 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 40 "grammars/G1/G1s/G1d.ml"
  )
    | EOF
    | BAR of (
# 19 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 46 "grammars/G1/G1s/G1d.ml"
  )
    | ARR of (
# 18 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 51 "grammars/G1/G1s/G1d.ml"
  )
    | AMPER of (
# 20 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 56 "grammars/G1/G1s/G1d.ml"
  )
  
end

include MenhirBasics

# 12 "grammars/G1/G1s/G1d.mly"
  
open Ast;;

# 67 "grammars/G1/G1s/G1d.ml"

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

  | MenhirState09 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_BAR, _menhir_box_toplevel) _menhir_state
    (** State 09.
        Stack shape : bexp1 BAR.
        Start symbol: toplevel. *)

  | MenhirState11 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_ARR, _menhir_box_toplevel) _menhir_state
    (** State 11.
        Stack shape : bexp1 ARR.
        Start symbol: toplevel. *)

  | MenhirState13 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_AMPER, _menhir_box_toplevel) _menhir_state
    (** State 13.
        Stack shape : bexp1 AMPER.
        Start symbol: toplevel. *)


and ('s, 'r) _menhir_cell1_bexp1 = 
  | MenhirCell1_bexp1 of 's * ('s, 'r) _menhir_state * (
# 32 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 105 "grammars/G1/G1s/G1d.ml"
)

and 's _menhir_cell0_AMPER = 
  | MenhirCell0_AMPER of 's * (
# 20 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 112 "grammars/G1/G1s/G1d.ml"
)

and 's _menhir_cell0_ARR = 
  | MenhirCell0_ARR of 's * (
# 18 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 119 "grammars/G1/G1s/G1d.ml"
)

and 's _menhir_cell0_BAR = 
  | MenhirCell0_BAR of 's * (
# 19 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 126 "grammars/G1/G1s/G1d.ml"
)

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * (
# 21 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 133 "grammars/G1/G1s/G1d.ml"
)

and ('s, 'r) _menhir_cell1_TILDE = 
  | MenhirCell1_TILDE of 's * ('s, 'r) _menhir_state * (
# 23 "grammars/G1/G1s/G1d.mly"
       (Range.t)
# 140 "grammars/G1/G1s/G1d.ml"
)

and _menhir_box_toplevel = 
  | MenhirBox_toplevel of (
# 31 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 147 "grammars/G1/G1s/G1d.ml"
) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (
# 40 "grammars/G1/G1s/G1d.mly"
                          ( _1 )
# 155 "grammars/G1/G1s/G1d.ml"
     : (
# 32 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 159 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_02 =
  fun x ->
    (
# 41 "grammars/G1/G1s/G1d.mly"
                          ( Var (snd x) )
# 167 "grammars/G1/G1s/G1d.ml"
     : (
# 32 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 171 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_03 =
  fun l r ->
    (
# 42 "grammars/G1/G1s/G1d.mly"
                          ( Imp(l, r) )
# 179 "grammars/G1/G1s/G1d.ml"
     : (
# 32 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 183 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_04 =
  fun l r ->
    (
# 43 "grammars/G1/G1s/G1d.mly"
                          ( Or(l, r) )
# 191 "grammars/G1/G1s/G1d.ml"
     : (
# 32 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 195 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_05 =
  fun l r ->
    (
# 44 "grammars/G1/G1s/G1d.mly"
                          ( And(l, r) )
# 203 "grammars/G1/G1s/G1d.ml"
     : (
# 32 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 207 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_06 =
  fun () ->
    (
# 47 "grammars/G1/G1s/G1d.mly"
                          ( True )
# 215 "grammars/G1/G1s/G1d.ml"
     : (
# 33 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 219 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_07 =
  fun () ->
    (
# 48 "grammars/G1/G1s/G1d.mly"
                          ( False )
# 227 "grammars/G1/G1s/G1d.ml"
     : (
# 33 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 231 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_08 =
  fun b ->
    (
# 49 "grammars/G1/G1s/G1d.mly"
                          ( Not(b) )
# 239 "grammars/G1/G1s/G1d.ml"
     : (
# 33 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 243 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_09 =
  fun b ->
    (
# 50 "grammars/G1/G1s/G1d.mly"
                          ( b )
# 251 "grammars/G1/G1s/G1d.ml"
     : (
# 33 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 255 "grammars/G1/G1s/G1d.ml"
    ))

let _menhir_action_10 =
  fun b ->
    (
# 37 "grammars/G1/G1s/G1d.mly"
                ( b )
# 263 "grammars/G1/G1s/G1d.ml"
     : (
# 31 "grammars/G1/G1s/G1d.mly"
      (Ast.bexp)
# 267 "grammars/G1/G1s/G1d.ml"
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
      let _v = _menhir_action_02 x in
      _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bexp1 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let b = _v in
          let _v = _menhir_action_10 b in
          MenhirBox_toplevel _v
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_BAR (_menhir_stack, _v) in
      let _menhir_s = MenhirState09 in
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
      let _v = _menhir_action_06 () in
      _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bexp2 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState03 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_TILDE -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TILDE (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_08 b in
      _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_01 _1 in
      _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_TILDE (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
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
      let _v = _menhir_action_07 () in
      _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_ARR (_menhir_stack, _v) in
      let _menhir_s = MenhirState11 in
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
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_AMPER (_menhir_stack, _v) in
      let _menhir_s = MenhirState13 in
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
  
  and _menhir_run_14 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_AMPER as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | EOF | RPAREN _ ->
          let MenhirCell0_AMPER (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp1 (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_05 l r in
          _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_ARR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | EOF | RPAREN _ ->
          let MenhirCell0_ARR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp1 (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_03 l r in
          _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_BAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | EOF | RPAREN _ ->
          let MenhirCell0_BAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp1 (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_04 l r in
          _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_09 b in
          _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BAR _v_1 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | ARR _v_2 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | AMPER _v_3 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
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
