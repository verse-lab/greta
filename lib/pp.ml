module C = Cfg
module T = Ta

open Printf
open List

let pp_upline () = 
  let upleft, mid, upright = "╒══════════", "═══════════", "══════════╕\n" in
  let mids = mid ^ mid ^ mid ^ mid in
  let line = upleft ^ mids ^ mids ^ upright in 
  printf "%s\n" line

let pp_loline () = 
  let loleft, mid, loright = "\n╘══════════", "═══════════", "══════════╛" in
  let mids = mid ^ mid ^ mid ^ mid in
  let line = loleft ^ mids ^ mids ^ loright in 
  printf "%s\n" line

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
  pp_upline (); pp_nonterminals (c.nonterms); pp_terminals (c.terms);
  pp_start (c.start); pp_productions (c.productions); pp_loline ()

let pp_states (ss: T.state list) =
  printf "\tStates : { "; ss |> iter (printf "%s "); printf "}\n"

let pp_alphabet (a: T.symbol list) =
  printf "\tAlphabet : { "; a |> iter (fun x -> 
    printf " <%s, %d> " (fst x) (snd x) ); printf "}\n"

let pp_root (s: T.state) = printf "\tStart State : { %s }\n" s

let pp_transitions (ts: T.transition list) =
  printf "\tTransitions : { \n"; ts |> iter (fun x -> 
    printf "\t\t\t%s ->_{%s} " (fst x) (fst (fst (snd x))); 
    (snd (snd x)) |> iter (printf "%s "); printf "\n"); printf " \t\t      }\n"

let pp_ta (a: T.ta) =
  pp_upline (); pp_states (a.states); pp_alphabet (a.alphabet); 
  pp_root (a.start_state); pp_transitions (a.transitions); pp_loline ()

let pp_symbol (s: T.symbol) = printf "<%s, %d> " (fst s) (snd s)
let pp_fst (s: T.symbol) = printf "\"%s\" " (fst s)

let pp_tree (e: T.tree) =
  let rec loop (e: T.tree) =
    match e with Leaf s -> printf " Leaf %s " s
    | Node (sym, subts) -> printf " Node ("; pp_fst sym; printf " ["; 
      let len = List.length subts in subts |> List.iteri (fun i x -> 
        if (i = len-1) then loop x else (loop x; printf "; ")); printf "])"
  in loop e

let rec pp_repeat (n: int) (s: string) =
  if n = 0 then printf "" else (printf "%s" s; pp_repeat (n-1) s)

let pp_collected_from_conflicts (inp_ls: (string list * string list) list) =
  printf "  >> Collected the following lines from conflicts: \n";
  inp_ls |> iter (fun (lns_ls, terms_ls) -> printf "\n\t>> Relevant lines: \n"; lns_ls |> iter (printf "\t%s\n"); 
  printf "\n\t>> Relevant terms: "; terms_ls |> iter (printf "%s "); printf "\n\n")

let pp_tree_pairs_syms (inp_ls: (T.tree * T.tree * string list) list) =
  printf "\n  >> Extracted trees and corresponding symbols: \n";
  inp_ls |> iter (fun (t1, t2, syms) -> 
    printf "\t>> First tree : "; pp_tree t1; printf "\n\t>> Second tree : "; pp_tree t2; 
    printf "\n\t>> Symbols : "; syms |> iter (printf "%s "); printf "\n\n")

let pp_combined_trees (inp_ls: (T.tree * T.tree) list) =
  printf "\n  >> Resulted example trees: \n"; 
  inp_ls |> iter (fun (t1, t2) -> printf "\t>> First tree : "; pp_tree t1; 
    printf "\n\t>> Second tree : "; pp_tree t2; printf "\n\n")

