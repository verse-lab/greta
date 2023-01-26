(* module C = Cfg *)

type state = string
type symbol = string * int
type transition = state * (symbol * state list)

type load = string
type tree = Leaf of load | Node of (symbol * (tree list))

type ta = (* TA := (Q, F, Q_s, \Del) *)
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_state : state;
    mutable transitions : transition list;
  }

  let null_ta = { states = []; alphabet = []; start_state = ""; transitions = [] }

let is_leaf (t: tree): bool =
  match t with Leaf _ -> true
  | Node (_, _) -> false

let return_state (t: tree): state =
  match t with Leaf v -> v
  | Node (_, _) -> "Error: Not a leaf"

let arity (sym: symbol): int = snd sym



