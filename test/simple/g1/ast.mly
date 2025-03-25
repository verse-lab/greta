type t =
  | Plus of t * t
  | Mul of t * t
  | Paren of t
  | Int of int64
  
type prog = t