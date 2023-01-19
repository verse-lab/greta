(* module C = Cfg *)

type state = string
type symbol = string * int
type transition = state * (symbol * state list)

type ta = (* TA := (Q, F, Q_s, \Del) *)
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_state : state;
    mutable transitions : transition list;
  }

let null_ta = { states = []; alphabet = []; start_state = ""; transitions = [] }

