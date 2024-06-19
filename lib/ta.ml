type state = string
type symbol = string * int
type transition = state * (symbol * state list)

type load = string
type tree = Leaf of load | Node of (symbol * (tree list))
type restriction = Assoc of (symbol * string) | Prec of (symbol * int)

exception Failure of string
exception Assoc_either_left_or_right
exception No_other_trivial_symbols
exception Invalid_transitions
exception Invalid_state_lists

(* Top-down TA := (Q, F, Q_s, \Del) *)
type ta =
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_state : state;
    mutable transitions : transition list;
  }

let null_ta = 
  { states = []; alphabet = []; start_state = ""; transitions = [] }

let epsilon_symb: symbol = ("ε", 1)
let epsilon_state: state = "ϵ"

let sym_equals sym str = (fst sym = str)

let syms_equals s1 s2 = ((fst s1) = (fst s2)) && ((snd s1) = (snd s2))

let arity (sym: symbol): int = snd sym


