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

let pp_raw_state (ss: (T.state * T.state)) = 
  printf "(%s, %s) " (fst ss) (snd ss)

let pp_raw_states (ss: (T.state * T.state) list) = 
  printf "\tRaw states : { "; ss |> iter pp_raw_state; printf "}\n"

let pp_raw_pair_of_state_pairs ((ss1, ss2): (T.state * T.state) * (T.state * T.state)) = 
  printf " ((%s, %s), (%s, %s)) " (fst ss1) (snd ss1) (fst ss2) (snd ss2)

let pp_alphabet (a: T.symbol list) =
  printf "\tAlphabet : { "; a |> iter (fun x -> 
    printf " <%s, %d> " (fst x) (snd x) ); printf "}\n"

let pp_root (s: T.state) = printf "\tStart State : { %s }\n" s

let pp_transitions (ts: T.transition list) =
  printf "\tTransitions : { \n"; ts |> iter (fun x -> 
    printf "\t\t\t%s ->_{%s} " (fst x) (fst (fst (snd x))); 
    (snd (snd x)) |> iter (printf "%s "); printf "\n"); printf " \t\t      }\n"

let pp_raw_transitions (ts: ((T.state * T.state) * (T.symbol * (T.state * T.state) list)) list) = 
  printf "\tRaw Transitions : { \n"; ts |> iter (fun ((st1, st2), (sym, st_pairs_ls)) -> 
    printf "\t\t\t(%s, %s) ->_{<%s, %i>} [ " st1 st2 (fst sym) (snd sym); 
    st_pairs_ls |> iter (fun (rst1, rst2) -> printf "(%s, %s) " rst1 rst2); printf "]\n");
    printf " \t\t      }\n"

let pp_raw_trans_blocks (ts_blocks: ((T.state * T.state) * ((T.state * T.state) * (T.symbol * (T.state * T.state) list)) list) list) =
  ts_blocks |> List.iter (fun ((st1, st2), raw_trans) -> Printf.printf "\n\tFor states (%s, %s), blocks of transitions : \n" st1 st2;
  pp_raw_transitions raw_trans)

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

let pp_collected_from_conflicts (inp_ls: string list) =
  printf "  >> Collected the following lines from conflicts: \n";
  inp_ls |> iter (fun s -> printf "\t%s\n" s)

let pp_tree_pairs_syms (inp_ls: (T.tree * T.tree * string list) list) =
  printf "\n  >> Extracted trees and corresponding symbols: \n";
  inp_ls |> iter (fun (t1, t2, syms) -> 
    printf "\n\t>> First tree : "; pp_tree t1; printf "\n\t>> Second tree : "; pp_tree t2; 
    printf "\n\t>> Symbols : "; syms |> iter (printf "%s "); printf "\n")

let pp_tree_to_expr (e: T.tree) = 
  let is_empty_leaf (ts: T.tree list) = 
    match (hd ts, length ts) with (Leaf "ϵ"), 1 -> true | _ -> false in
  let rec ptree_loop (e: T.tree) =
    match e with Leaf s -> printf "%s " s
    | Node (sym, subts) -> 
      (* TO FIX: example trees gen with versatiles sometimes have arities not equal to (length subts) 
       *         so instead of using (length subts) should be able to use (snd sym) *)
      let s', rnk = fst sym, (length subts) in
      if (rnk = 0 && is_empty_leaf subts) then 
        (printf "%s " s')
      else if (rnk = 2 && s' = "IF") then 
        (printf "( %s " s'; ptree_loop (nth subts 0); 
        printf "THEN "; ptree_loop (nth subts 1); printf ") ")
      else if (rnk = 2 && not (s' = "IF")) then 
        (printf "( "; ptree_loop (nth subts 0); 
        printf "%s " s'; ptree_loop (nth subts 1); printf ") ")
      else if (rnk = 1 && s' = "LPARENRPAREN") then 
        (printf "( LPAREN "; ptree_loop (nth subts 0); printf "RPAREN ) ") 
      else if (rnk = 3 && s' = "IF") then 
        (printf "( %s " s'; ptree_loop (nth subts 0);
        printf "THEN "; ptree_loop (nth subts 1);
        printf "ELSE "; ptree_loop (nth subts 2); printf ") ")
      else printf "Node %s with a rnk other than 1, 2 or 3!" (fst sym)
  in ptree_loop e  

let pp_expr_lst (sls:string list) = 
  sls |> iter (fun s -> printf " %s " s); printf "\n" 

let pp_restriction_lst (rls:T.restriction list) =
  rls |> iter (fun r -> match r with 
      | T.Assoc (s, a) -> (printf "("; pp_symbol s; printf ", %s) " a)
      | T.Prec (s, i) -> printf "("; pp_symbol s; printf ", %i) " i); printf "\n\n"

let pp_restriction'_lst (rls:(T.restriction * (C.nonterminal * C.sigma list)) list) =
  let pp_sigma_list sls =
    printf "%s " (fst sls);
    printf "[ "; 
    iter (fun s -> match s with 
      | C.T s' -> printf "%s " s'
      | C.Nt s' -> printf "%s " s') (snd sls); 
    printf "]\n"
  in
  rls |> iter (fun r -> match r with 
      | T.Assoc (s, a), sg -> (printf "("; pp_symbol s; printf ", %s) " a);pp_sigma_list sg
      | T.Prec (s, i), sg -> printf "("; pp_symbol s; printf ", %i) " i;pp_sigma_list sg)

let pp_combined_trees (inp_ls: ((T.tree * (bool * bool) * T.restriction list)) list) =
  printf "\n  >> Resulted example trees: \n\n"; 
  inp_ls |> iter (fun ((t, (oa, op), rls)) -> 
    printf "\t>> Tree : "; pp_tree t;
    printf "\n\t\t O_a : %b" oa; printf "\n\t\t O_p : %b" op; 
    printf "\n\t\t Expression : "; pp_tree_to_expr t;
    printf "\n\t\t Restrictions : "; pp_restriction_lst rls; printf "\n") 

let pp_exprs (exprs_ls: (string list * string list) list) =
  printf "\n  >> Resulted expressions: \n";
  exprs_ls |> iter (fun (ls1, ls2) -> 
    printf "\n\t>> First expression : "; ls1 |> iter (printf "%s ");
    printf "\n\t>> Second expression : "; ls2 |> iter (printf "%s "); printf "\n"); 
    printf "\n"
