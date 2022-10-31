
type t =
  | Int of int
  | Bool of bool
  | Plus of t * t
  | Mul of t * t
  | Paren of t
  | If1 of t * t
  | If2 of t * t * t

let rec pp fmt =
  function
  | Int i -> Format.pp_print_int fmt i
  | Bool b -> Format.pp_print_bool fmt b
  | Paren v -> Format.fprintf fmt "@{Paren(%a)@}" pp v
  | Plus (l, r) -> Format.fprintf fmt "@{Plus(%a, %a)@}" pp l pp r
  | Mul (l, r) -> Format.fprintf fmt "@{Mul(%a, %a)@}" pp l pp r
  | If1 (b, then_br) -> Format.fprintf fmt "@{if1 %a then { %a }@}" pp b pp then_br
  | If2 (b, then_br, else_br) -> Format.fprintf fmt "@{if2 %a then { %a } else { %a }@}" pp b pp then_br pp else_br

let show expr = Format.asprintf "%a" pp expr
                  
