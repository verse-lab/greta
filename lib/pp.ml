module C = Cfg
module T = Ta
module E = Examples

open Printf
open List

let pp_terminals (ts: C.terminal list) =
  printf "\tTerminals : { "; ts |> iter (printf "%s "); printf "}\n"

let pp_start (s: C.nonterminal) = printf "\tStart symbol : { %s }\n" s

let pp_nonterminals (ns: C.nonterminal list) =
  printf "\tNonterminals : { "; ns |> iter (printf "%s "); printf "}\n"

let pp_productions (ps: C.production list) =
  printf "\tSet of productions : { \n"; ps |> iter (fun x -> 
    printf "\t\t\t\t%s -> ( %s " (fst x) (fst (snd x));
        (snd (snd x))|> iter (printf "%s "); printf ")\n"); printf "\t\t\t     }\n"

let pp_cfg (c: C.cfg) =
  pp_terminals (c.terms); pp_start (c.start); pp_nonterminals (c.nonterms); pp_productions (c.prods)

let pp_states (ss: T.state list) =
  printf "\tStates : { "; ss |> iter (printf "%s "); printf "}\n"

let pp_alphabet (a: T.symbol list) =
  printf "\tAlphabet : { "; a |> iter (fun x -> printf " <%s, %d> " (fst x) (snd x) ); printf "}\n"

let pp_root (s: T.state) = printf "\tFinal State : { %s }\n" s

let pp_transitions (ts: T.transition list) =
  printf "\tTransitions : { \n"; ts |> iter (fun x -> printf "\t\t\t%s ->_{%s} " 
    (fst x) (fst (fst (snd x))); (snd (snd x)) |> iter (printf "%s "); printf "\n"); printf "\t\t      }\n"

let pp_ta (a: T.ta): unit =
  pp_states (a.states); pp_alphabet (a.alphabet); pp_root (a.start_state); pp_transitions (a.transitions)

let pp_symbol (s: T.symbol): unit = printf "<%s, %d> " (fst s) (snd s)

let pp_example (e: E.tree): unit =
  match e with
  | Leaf s -> printf "Leaf of %s" s
  | Node (op, _) -> printf "Node of "; (pp_symbol op)

