type state = string
type symbol = string * int
type transition = state * (symbol * state list)
type terminal = string

type beta = T of terminal | S of state
type load = string
type tree = Leaf of load | Node of (symbol * (tree list))
type restriction = Assoc of (symbol * string) | Prec of (symbol * int)

(* Top-down TA := (Q, F, Q_f, \Del) *)
type ta =
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable final_states : state list;
    mutable terminals : terminal list;
    mutable transitions : ((state * symbol), beta list list) Hashtbl.t;
    mutable trivial_sym_nts : (symbol * state) list;
  }

exception Failure of string
exception Assoc_either_left_or_right
exception No_other_trivial_symbols
exception Invalid_transitions
exception Invalid_state_lists
exception Invalid_symbol_list
exception Invalid_sigma_list
exception Not_same_sym_and_rhs_state_pairs
exception No_state_in_renaming_map

let null_ta : ta = 
  { states = []; alphabet = []; final_states = []; terminals = [] ; transitions = Hashtbl.create 0; trivial_sym_nts = [] }

let epsilon_symb: symbol = ("ε", 1)
let epsilon_state: state = "ϵ"

let sym_equals sym str = (fst sym = str)

let syms_equals s1 s2 = ((fst s1) = (fst s2)) && ((snd s1) = (snd s2))

let arity (sym: symbol): int = snd sym


(* type optimization = 
 {
   mutable eps_opt : bool;   (* true if there is no non-trivial <eps, 1> transition               *)
   mutable paren_opt : bool; (* true if there is no non-trivial <(), 1> transition                *)
   mutable triv_opt : bool;  (* true if there is no non-trivial <eps, 1> to triv_state transition *)
   mutable onoff_opt : bool   (* flag created for a special case where you need to turn on and off the eps flag *)
 } *)



