type t =
  | Na
  | If of exp * then_t
  | Semi of decl
and then_t = 
  | Then of t * else_t
and else_t = 
  | Else of t

and decl = 
  | TDecl of string * exp
and exp =
  | Int of int
  | Plus of exp * exp
  | Star of exp * exp
  | Paren of exp
and id = string

