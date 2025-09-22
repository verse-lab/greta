type annot = string

type expr =
  | EInt    of expr
  | EString of string
  | EBytes  of string
  | ESeq    of expr list
  | EPrim   of { name : string; annots : annot list; args : expr list }

(* A “script” is either a single Micheline expr, or the legacy header form. *)
type script =
  | SExpr of expr
  | SContract of { parameter : expr; storage : expr; code : expr } 
  (* code is always a sequence by convention, but we keep it generic expr *)