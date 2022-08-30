
type t =
  | Int of int
  | Plus of t * t
  | Mul of t * t
  | Paren of t

let rec pp fmt =
  function
  | Int i -> Format.pp_print_int fmt i
  | Paren v -> Format.fprintf fmt "@{Paren(%a)@}" pp v
  | Plus (l, r) -> Format.fprintf fmt "@{Plus(%a, %a)@}" pp l pp r
  | Mul (l, r) -> Format.fprintf fmt "@{Mul(%a,%a)@}" pp l pp r

let show expr = Format.asprintf "%a" pp expr
                  
