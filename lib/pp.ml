module C = Cfg
module T = Ta

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
  pp_terminals (c.terms); pp_start (c.start); pp_nonterminals (c.nonterms); 
  pp_productions (c.prods)

let pp_states (ss: T.state list) =
  printf "\tStates : { "; ss |> iter (printf "%s "); printf "}\n"

let pp_alphabet (a: T.symbol list) =
  printf "\tAlphabet : { "; a |> iter (fun x -> 
    printf " <%s, %d> " (fst x) (snd x) ); printf "}\n"

let pp_root (s: T.state) = printf "\tStart State : { %s }\n" s

let pp_transitions (ts: T.transition list) =
  printf "\tTransitions : { \n"; ts |> iter (fun x -> 
    printf "\t\t\t%s ->_{%s} " (fst x) (fst (fst (snd x))); 
    (snd (snd x)) |> iter (printf "%s "); printf "\n"); printf "\t\t      }\n"

let pp_ta (a: T.ta): unit =
  pp_states (a.states); pp_alphabet (a.alphabet); pp_root (a.start_state); 
  pp_transitions (a.transitions)

let pp_symbol (s: T.symbol): unit = printf "<%s, %d> " (fst s) (snd s)
let pp_fst (s: T.symbol): unit = printf "\"%s\" " (fst s)

let pp_tree (e: T.tree): unit =
  let rec loop (e: T.tree) =
    match e with Leaf s -> printf " Leaf %s " s
    | Node (sym, subts) -> printf " Node ("; pp_fst sym; printf " ["; 
      let len = List.length subts in subts |> List.iteri (fun i x -> 
        if (i = len-1) then loop x else (loop x; printf "; ")); printf "])"
  in loop e

let rec pp_repeat (n: int) (s: string): unit =
  if n = 0 then printf "" else (printf "%s" s; pp_repeat (n-1) s)
