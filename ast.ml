
type t =
  | Void
  | Int of int
  | Bool of bool
  | Plus of t * t
  | Mul of t * t
  | Paren of t
  | If of t * then_t
  and then_t = 
  | Then of t * else_t
  and else_t = 
  | Else of t

let rec pp fmt =
  function
  | Void -> Format.pp_print_space fmt ()
  | Int i -> Format.pp_print_int fmt i
  | Bool b -> Format.pp_print_bool fmt b
  | Paren v -> Format.fprintf fmt "@{Paren(%a)@}" pp v
  | Plus (l, r) -> Format.fprintf fmt "@{Plus(%a, %a)@}" pp l pp r
  | Mul (l, r) -> Format.fprintf fmt "@{Mul(%a, %a)@}" pp l pp r
  | If (b, (Then (br1, Else Void))) -> Format.fprintf fmt "@{if1 %a then { %a }@}" pp b pp br1
  | If (b, (Then (br1, Else br2))) -> Format.fprintf fmt "@{if2 %a then { %a } else { %a }@}" pp b pp br1 pp br2

let show expr = Format.asprintf "%a" pp expr
                  
