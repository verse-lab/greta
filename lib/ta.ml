type state = string
type terminal = string
type symbol = int * string * int (* 1st int to provide unique ID *)

type beta = T of terminal | S of state 
type transition = (state * symbol) * beta list

type load = string
type tree = Leaf of load | Node of (symbol * (tree list))
type restriction = Assoc of (symbol * int) | Prec of (symbol * int)

(* Top-down TA := (Q, F, Q_f, \Del) *)
type ta =
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable final_states : state list;
    mutable terminals : terminal list;
    mutable transitions : ((state * symbol), beta list) Hashtbl.t;
    (* mutable trivial_sym_states : (symbol * state) list; *)
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
  { states = []; alphabet = []; final_states = []; terminals = [] ; transitions = Hashtbl.create 0 } (* ; trivial_sym_states = [] *)

let dummy_sym : symbol = (-1, "", -1)

let epsilon_state: state = "ϵ"

let arity_of_sym (sym: symbol): int = match sym with _, _, n -> n
let id_of_sym (sym: symbol): int = match sym with i, _, _ -> i
let term_of_sym (sym: symbol): string = match sym with _, x, _ -> x

let syms_equals s1 s2 = 
  (id_of_sym s1) = (id_of_sym s2) && (arity_of_sym s1) = (arity_of_sym s2) && (term_of_sym s1) = (term_of_sym s2) 

let is_dummy_sym (s: symbol): bool = 
  syms_equals dummy_sym s
let count = ref 0
let gen_count = ref 0


