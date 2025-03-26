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
exception Invalid_sigma_list
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

type ta3 = 
  {
    mutable states : Cfg.nonterminal list;
    mutable start_states : Cfg.nonterminal list;
    (* arity -> transitions *)
    mutable transitions : (int, (Cfg.nonterminal * Cfg.sigma list) list) Hashtbl.t;
  }

(* Pretty printer for sigma (terminal/nonterminal) *)
let string_of_sigma (s: Cfg.sigma) : string =
  match s with
  | T terminal -> "'" ^ terminal ^ "'"
  | Nt nonterminal -> nonterminal

(* Pretty print a list with a custom separator *)
let rec print_list ~separator f lst =
  match lst with
  | [] -> ""
  | [x] -> f x
  | x::xs -> f x ^ separator ^ print_list ~separator f xs

(* Pretty printer for transitions *)
let string_of_transitions (transitions: (int, (Cfg.nonterminal * Cfg.sigma list) list) Hashtbl.t) : string =
  Hashtbl.fold (fun arity trans_list acc ->
    let arity_transitions = 
      List.map (fun (lhs, rhs) ->
        Printf.sprintf "  %s -> %s [%s]" 
          lhs 
          (print_list ~separator:" " string_of_sigma rhs)
          (string_of_int arity)
      ) trans_list
    in
    acc ^ 
    (Printf.sprintf "Arity %d Transitions:\n%s\n" 
      arity 
      (print_list ~separator:"\n" (fun x -> x) arity_transitions))
  ) transitions ""

(* Pretty printer for ta3 *)
let pp_ta3 (ta: ta3) : unit =
  (* Print header *)
  Printf.printf "Tree Automaton:\n";
  Printf.printf "==============\n\n";
  
  (* Print states *)
  Printf.printf "States:\n";
  Printf.printf "  %s\n\n" (print_list ~separator:", " (fun x -> x) ta.states);
  
  (* Print start states *)
  Printf.printf "Start States:\n";
  Printf.printf "  %s\n\n" (print_list ~separator:", " (fun x -> x) ta.start_states);
  
  (* Print transitions *)
  Printf.printf "Transitions:\n";
  print_endline (string_of_transitions ta.transitions)

type optimization = 
 {
   mutable eps_opt : bool;   (* true if there is no non-trivial <eps, 1> transition               *)
   mutable paren_opt : bool; (* true if there is no non-trivial <(), 1> transition                *)
   mutable triv_opt : bool   (* true if there is no non-trivial <eps, 1> to triv_state transition *)
 }

let null_ta : ta2 = 
  { states = []; alphabet = []; start_states = []; transitions = Hashtbl.create 0; trivial_sym_nts = [] }

let epsilon_symb: symbol = ("ε", 1)
let epsilon_state: state = "ϵ"

let sym_equals sym str = (fst sym = str)

let syms_equals s1 s2 = ((fst s1) = (fst s2)) && ((snd s1) = (snd s2))

let arity (sym: symbol): int = snd sym


