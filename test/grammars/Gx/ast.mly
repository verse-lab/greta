type id = string

type t =
  | Na
  | Decl of decl
  | If2 of exp_t * then_t
  | If3 of exp_t * then_t * else_t

and then_t = 
  | Then of t

and else_t = 
  | Else of t

and exp_t =
  | Plus of exp_t * exp_t
  | Mul of exp_t * exp_t
  | Paren of exp_t
  | CInt of int64
  | Ident of id

and decl =
  | Assign of id * exp_t
  
type prog = t