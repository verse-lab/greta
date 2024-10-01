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
exception Invalid_symbol_list
exception Not_same_sym_and_rhs_state_pairs
exception No_state_in_renaming_map

(* Top-down TA := (Q, F, Q_s, \Del) *)
type ta =
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_state : state;
    mutable transitions : transition list;
    mutable trivial_sym_nts : (symbol * state) list;
  }

type ta2 = 
  {
    mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_states : state list;
    mutable transitions : ((state * symbol), Cfg.sigma list list) Hashtbl.t;
    mutable trivial_sym_nts : (symbol * state) list;
  }

let null_ta : ta2 = 
  { states = []; alphabet = []; start_states = []; transitions = Hashtbl.create 0; trivial_sym_nts = [] }

let epsilon_symb: symbol = ("ε", 1)
let epsilon_state: state = "ϵ"

let sym_equals sym str = (fst sym = str)

let syms_equals s1 s2 = ((fst s1) = (fst s2)) && ((snd s1) = (snd s2))

let arity (sym: symbol): int = snd sym


