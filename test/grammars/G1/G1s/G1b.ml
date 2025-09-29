
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 13 "grammars/G1/G1s/G1b.mly"
       (Range.t * string)
# 15 "grammars/G1/G1s/G1b.ml"
  )
    | TRUE of (
# 20 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 20 "grammars/G1/G1s/G1b.ml"
  )
    | TILDE of (
# 19 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 25 "grammars/G1/G1s/G1b.ml"
  )
    | RPAREN of (
# 18 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 30 "grammars/G1/G1s/G1b.ml"
  )
    | LPAREN of (
# 17 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 35 "grammars/G1/G1s/G1b.ml"
  )
    | FALSE of (
# 21 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 40 "grammars/G1/G1s/G1b.ml"
  )
    | EOF
    | BAR of (
# 15 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 46 "grammars/G1/G1s/G1b.ml"
  )
    | ARR of (
# 14 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 51 "grammars/G1/G1s/G1b.ml"
  )
    | AMPER of (
# 16 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 56 "grammars/G1/G1s/G1b.ml"
  )
  
end

include MenhirBasics

# 8 "grammars/G1/G1s/G1b.mly"
  
open Ast;;

# 67 "grammars/G1/G1s/G1b.ml"

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

  | MenhirState07 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp2 _menhir_cell0_ARR, _menhir_box_toplevel) _menhir_state
    (** State 07.
        Stack shape : bexp2 ARR.
        Start symbol: toplevel. *)

  | MenhirState11 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_BAR, _menhir_box_toplevel) _menhir_state
    (** State 11.
        Stack shape : bexp1 BAR.
        Start symbol: toplevel. *)

  | MenhirState13 : (('s, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_AMPER, _menhir_box_toplevel) _menhir_state
    (** State 13.
        Stack shape : bexp1 AMPER.
        Start symbol: toplevel. *)


and ('s, 'r) _menhir_cell1_bexp1 = 
  | MenhirCell1_bexp1 of 's * ('s, 'r) _menhir_state * (
# 26 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 105 "grammars/G1/G1s/G1b.ml"
)

and ('s, 'r) _menhir_cell1_bexp2 = 
  | MenhirCell1_bexp2 of 's * ('s, 'r) _menhir_state * (
# 27 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 112 "grammars/G1/G1s/G1b.ml"
)

and 's _menhir_cell0_AMPER = 
  | MenhirCell0_AMPER of 's * (
# 16 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 119 "grammars/G1/G1s/G1b.ml"
)

and 's _menhir_cell0_ARR = 
  | MenhirCell0_ARR of 's * (
# 14 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 126 "grammars/G1/G1s/G1b.ml"
)

and 's _menhir_cell0_BAR = 
  | MenhirCell0_BAR of 's * (
# 15 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 133 "grammars/G1/G1s/G1b.ml"
)

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * (
# 17 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 140 "grammars/G1/G1s/G1b.ml"
)

and ('s, 'r) _menhir_cell1_TILDE = 
  | MenhirCell1_TILDE of 's * ('s, 'r) _menhir_state * (
# 19 "grammars/G1/G1s/G1b.mly"
       (Range.t)
# 147 "grammars/G1/G1s/G1b.ml"
)

and _menhir_box_toplevel = 
  | MenhirBox_toplevel of (
# 25 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 154 "grammars/G1/G1s/G1b.ml"
) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (
# 34 "grammars/G1/G1s/G1b.mly"
                          ( _1 )
# 162 "grammars/G1/G1s/G1b.ml"
     : (
# 26 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 166 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_02 =
  fun x ->
    (
# 35 "grammars/G1/G1s/G1b.mly"
                          ( Var (snd x) )
# 174 "grammars/G1/G1s/G1b.ml"
     : (
# 26 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 178 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_03 =
  fun l r ->
    (
# 36 "grammars/G1/G1s/G1b.mly"
                          ( Or(l, r) )
# 186 "grammars/G1/G1s/G1b.ml"
     : (
# 26 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 190 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_04 =
  fun l r ->
    (
# 37 "grammars/G1/G1s/G1b.mly"
                          ( And(l, r) )
# 198 "grammars/G1/G1s/G1b.ml"
     : (
# 26 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 202 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_05 =
  fun () ->
    (
# 40 "grammars/G1/G1s/G1b.mly"
                          ( True )
# 210 "grammars/G1/G1s/G1b.ml"
     : (
# 27 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 214 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_06 =
  fun () ->
    (
# 41 "grammars/G1/G1s/G1b.mly"
                          ( False )
# 222 "grammars/G1/G1s/G1b.ml"
     : (
# 27 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 226 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_07 =
  fun l r ->
    (
# 42 "grammars/G1/G1s/G1b.mly"
                          ( Imp(l, r) )
# 234 "grammars/G1/G1s/G1b.ml"
     : (
# 27 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 238 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_08 =
  fun b ->
    (
# 43 "grammars/G1/G1s/G1b.mly"
                          ( Not(b) )
# 246 "grammars/G1/G1s/G1b.ml"
     : (
# 27 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 250 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_09 =
  fun b ->
    (
# 44 "grammars/G1/G1s/G1b.mly"
                          ( b )
# 258 "grammars/G1/G1s/G1b.ml"
     : (
# 27 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 262 "grammars/G1/G1s/G1b.ml"
    ))

let _menhir_action_10 =
  fun b ->
    (
# 31 "grammars/G1/G1s/G1b.mly"
                ( b )
# 270 "grammars/G1/G1s/G1b.ml"
     : (
# 25 "grammars/G1/G1s/G1b.mly"
      (Ast.bexp)
# 274 "grammars/G1/G1s/G1b.ml"
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
      | MenhirState04 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
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
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | AMPER _v_1 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_BAR (_menhir_stack, _v) in
      let _menhir_s = MenhirState11 in
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
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_05 () in
      _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bexp2 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState03 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_TILDE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | AMPER _ | BAR _ | EOF | RPAREN _ ->
          let MenhirCell1_TILDE (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_08 b in
          _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp2 -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_ARR (_menhir_stack, _v) in
      let _menhir_s = MenhirState07 in
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
      let _v = _menhir_action_06 () in
      _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_12 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp1 _menhir_cell0_BAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | AMPER _ | BAR _ | EOF | RPAREN _ ->
          let MenhirCell0_BAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp1 (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_03 l r in
          _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_bexp2 _menhir_cell0_ARR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | AMPER _ | BAR _ | EOF | RPAREN _ ->
          let MenhirCell0_ARR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp2 (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_07 l r in
          _menhir_goto_bexp2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARR _v_0 ->
          let _menhir_stack = MenhirCell1_bexp2 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | AMPER _ | BAR _ | EOF | RPAREN _ ->
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
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
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | AMPER _v_1 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | EOF | RPAREN _ ->
          let MenhirCell0_AMPER (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_bexp1 (_menhir_stack, _menhir_s, l) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_04 l r in
          _menhir_goto_bexp1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
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
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPER _v_2 ->
          let _menhir_stack = MenhirCell1_bexp1 (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
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
