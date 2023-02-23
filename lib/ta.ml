type state = string
type symbol = string * int
type transition = state * (symbol * state list)

type load = string
type tree = Leaf of load | Node of (symbol * (tree list))

(* Top-down TA := (Q, F, Q_s, \Del) *)
type ta =
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_state : state;
    mutable transitions : transition list;
  }

let null_ta = 
  { states = []; alphabet = []; start_state = ""; transitions = [] }

let sym_equals sym str = (fst sym = str)

let syms_equals s1 s2 = (fst s1) = (fst s2)

let arity (sym: symbol): int = snd sym


