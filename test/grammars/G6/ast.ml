type t = 
  | Lt of exp * exp
  | Gt of exp * exp
  | Eq of exp * exp
  | Ne of exp * exp
  | Gte of exp * exp
  | Lte of exp * exp
  | And of t * t
  | Or of t * t
  | Iff of t * t
  | Not of t
  | Bvar 
  | Bparen of t

and exp = 
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Divide of exp * exp
  | Power of exp * exp
  | Negative of exp
  | Ivar 
  | Int
  | Paren of exp


