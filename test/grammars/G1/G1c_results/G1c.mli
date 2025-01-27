
(* The type of tokens. *)

type token = 
  | VAR of (Range.t * string)
  | TRUE of (Range.t)
  | TILDE of (Range.t)
  | RPAREN of (Range.t)
  | LPAREN of (Range.t)
  | FALSE of (Range.t)
  | EOF
  | BAR of (Range.t)
  | ARR of (Range.t)
  | AMPER of (Range.t)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val toplevel: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.bexp)
