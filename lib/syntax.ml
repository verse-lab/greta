(* (untyped) Michelson program *)

type inst = 
  | Simple of string
  | SimpleArgCon of string * Parsetree.expression (* such as PUSH int 0 *)
  | SimpleWithNum of string * int
  | OneBlock of string * inst list  (* such as DIP code *)
  | OneBlockWithNum of string * int * inst list  (* such as DIP 3 code *)
  | OneBlockWithTwoTys of string * Parsetree.core_type * Parsetree.core_type * inst list  (* such as LAMBDA ty ty code *)
  | TwoBlocks of string * inst list * inst list  (* such IF code1 code 2 *)
  | CreateContract of string * program
  | Block of inst list
  | IfThen of inst list
  | IfThenElse of inst list * inst list
  | IfLeft of inst list * inst list
  | IfRight of inst list * inst list
  | IfNone of inst list * inst list
  | Loop of inst list
  | LoopLeft of inst list
  | Iter of inst list
  | Map of inst list
and program = 
    Code of (Parsetree.core_type * Parsetree.core_type) option * inst list