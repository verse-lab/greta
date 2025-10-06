module C = Cfg
module T = Ta

open Printf
open List

(* hack fix because there are too many such uses to fix... *)
let debug = true
let noprintf fmt = if debug then printf fmt else ifprintf stdout fmt

let pp_upline () = 
  let upleft, mid, upright = "╒══════════", "═══════════", "══════════╕\n" in
  let mids = mid ^ mid ^ mid ^ mid in
  let line = upleft ^ mids ^ mids ^ upright in 
  noprintf "%s\n" line

let pp_loline () = 
  let loleft, mid, loright = "\n╘══════════", "═══════════", "══════════╛" in
  let mids = mid ^ mid ^ mid ^ mid in
  let line = loleft ^ mids ^ mids ^ loright in 
  noprintf "%s\n" line

let pp_terminals (ts: C.terminal list) =
  noprintf "\tTerminals : { "; ts |> iter (noprintf "%s "); noprintf "}\n"

let pp_start (s: C.nonterminal) = noprintf "\tStart symbol : { %s }\n" s

let pp_starts (ss: C.nonterminal list) = 
  noprintf "\tStart symbols : { "; ss |> iter (noprintf "%s "); noprintf "}\n"

let pp_sigma s = match s with 
  | C.Term s' -> noprintf "%s " s'
  | C.Nt s' -> noprintf "%s " s'

let pp_beta (s: T.beta) = match s with 
  | T.T s' -> noprintf "%s " s'
  | T.S s' -> noprintf "%s " s'

let pp_sigma_list sls =
  noprintf "[ "; sls |> iter pp_sigma; noprintf "] "

let pp_beta_list (sls: T.beta list) =
  noprintf "[ "; sls |> iter pp_beta; noprintf "] "

let pp_nonterminals (ns: C.nonterminal list) =
  noprintf "\tNonterminals : { "; ns |> iter (noprintf "%s "); noprintf "}\n"

let pp_production (p: C.production) = 
  noprintf "\t\t %s  ->  (" (fst p); pp_sigma_list (snd p); noprintf ")\n"

let pp_productions (ps: C.production list) =
  noprintf "\tSet of productions : { \n"; ps |> iter pp_production; noprintf "\t\t\t     }\n"

let pp_cfg (c: C.cfg) = 
  pp_upline (); pp_nonterminals (c.nonterms); pp_terminals (c.terms); pp_starts (c.starts); 
  pp_productions (c.productions); pp_loline ()

(* let pp_prods_mapping1 (pms: ((string list * string) * string list) list) = 
  noprintf "\n\t (changed) Nontrivial productions mapping \n"; 
  pms |> iter (fun ((ts, prod), nts) -> 
    noprintf "\n\tTerminals : "; ts |> iter (fun s -> noprintf "%s " s); 
    noprintf "\t with production %s " prod;
    noprintf " mapped to ===> \n\t\t Nonterminals : "; nts |> List.iter (fun s -> noprintf " %s  " s); noprintf "\n") *)

let pp_prods_mapping (pms: ((string list * int) * (string * string list)) list) = 
  noprintf "\n\t (changed) Nontrivial productions mapping \n"; 
  pms |> iter (fun ((ts, nts_num), (prod, nts)) -> 
    noprintf "\n\tTerminals : "; ts |> iter (fun s -> noprintf "%s " s); 
    noprintf "\t with %d number of Nonterminals mapped to ===> \n\t\t %s\n" nts_num prod; 
    noprintf "And Nonterminals : "; nts |> List.iter (noprintf "%s "); noprintf "\n")

let pp_states (ss: T.state list) =
  noprintf "\tStates : { "; ss |> iter (noprintf "%s "); noprintf "}\n"

let pp_raw_state (ss: (T.state * T.state)) = 
  noprintf "(%s, %s) " (fst ss) (snd ss)

let pp_raw_states (ss: (T.state * T.state) list) = 
  noprintf "\tStates : { "; ss |> iter pp_raw_state; noprintf "}\n"

let pp_raw_pair_of_state_pairs ((ss1, ss2): (T.state * T.state) * (T.state * T.state)) = 
  noprintf " ((%s, %s), (%s, %s)) " (fst ss1) (snd ss1) (fst ss2) (snd ss2); noprintf "\n"

let pp_symbol (s: T.symbol) = 
  noprintf " <%d: %s, %d> " (T.id_of_sym s) (T.term_of_sym s) (T.arity_of_sym s)

let pp_beta_beta_list (bbls: (T.beta * T.beta) list) = 
  noprintf "  [ "; bbls |> iter (fun (b1, b2) -> 
    noprintf "  ( "; pp_beta b1; noprintf ", "; pp_beta b2; noprintf ")   "); noprintf "] \n"

let pp_sigma_listlist (slsls: C.sigma list list) = 
  noprintf "\t\t\t\t\t  [     "; slsls |> iter pp_sigma_list; noprintf "     ]\n"

let pp_alphabet (a: T.symbol list) =
  noprintf "\tAlphabet : { "; a |> iter (fun x -> pp_symbol x); noprintf "}\n"

let pp_final_states (ss: T.state list) = 
  noprintf "\tFinal States : { "; ss |> iter (noprintf "%s "); noprintf "}\n"

let pp_transitions (ts: T.transition list) =
  noprintf "\tTransitions : { \n"; ts |> iter (fun ((st, sy), bls) -> 
    noprintf "\t\t\t%s ->_{ " st; pp_symbol sy; noprintf " } -> ";
    pp_beta_list bls; noprintf "\n"); noprintf " \t\t      }\n"

let pp_obp_tbl (obp_tbl: (int, T.symbol list) Hashtbl.t) = 
  noprintf "\n    [\n";
  obp_tbl |> Hashtbl.iter (fun o_idx s_ls -> noprintf "\n\tOrder %i -> " o_idx; 
    s_ls |> iter pp_symbol; noprintf "\n"); noprintf "    ]\n"

let pp_beta_pair (bb: (T.beta * T.beta)) = 
  let b1, b2 = (fst bb), (snd bb) in 
  noprintf " ( "; pp_beta b1; noprintf ", "; pp_beta b2; noprintf ")  "

let pp_raw_transition (tran: ((T.state * T.state) * T.symbol) * (T.beta * T.beta) list) = 
  let sts_pair_sym, beta_pair_ls = (fst tran), (snd tran) in
  let sts_pair, sym = (fst sts_pair_sym), (snd sts_pair_sym) in
  let st1, st2 = (fst sts_pair), (snd sts_pair) in 
  noprintf "\t\t (%s, %s) ->_{< " st1 st2; pp_symbol sym; noprintf ">} [ "; beta_pair_ls |> iter pp_beta_pair

let pp_raw_transitions (ts: (((T.state * T.state) * T.symbol) * (T.beta * T.beta) list) list) = 
  noprintf "\t Raw Transitions : { \n\t"; ts |> iter (fun raw_tran -> pp_raw_transition raw_tran; noprintf "\n\t"); 
  noprintf "]\n"; noprintf " \t      }\n"


let pp_raw_cart_product_trans (ts: (((T.state * T.state) * T.symbol) * (T.beta * T.beta) list) list) =
  noprintf "\n\t Raw Transitions : { \n"; ts |> iter (fun (((st1, st2), sym), beta_pair_ls) -> 
    (* [prev] "\t\t\t(%s, %s)  ->_{<%s, %i>}  " *)
    noprintf "\t    (%s, %s)  ->_{" st1 st2; pp_symbol sym; noprintf "}  ";
    beta_pair_ls |> pp_beta_beta_list) ; noprintf "\t     }\n"


let pp_transitions_tbl (tbl: ((T.state * T.symbol), T.beta list) Hashtbl.t) = 
  noprintf "\n\tTransitions (htbl): { ";
  tbl |> Hashtbl.iter (fun (lhs, s) ls ->  (* prev below "\n\t\t\t\t( State %s, " *)
    let print_lhs_st_sym () = noprintf "\n( State %s, " lhs; pp_symbol s; noprintf ") -> " in 
      print_lhs_st_sym (); noprintf "[ "; ls |> pp_beta_list; noprintf "] "); 
      noprintf "\n\t\t\t    }\n"

let pp_raw_trans_blocks (ts_blocks: ((T.state * T.state) * ((T.state * T.state) * (T.symbol * (T.beta * T.beta) list)) list) list) =
  let open List in
  ts_blocks |> iter (fun (((st1, st2), blocks_ls): (T.state * T.state) * ((T.state * T.state) * (T.symbol * (T.beta * T.beta) list)) list) -> 
    noprintf "\n\tRaw blocks for state pairs : \t(%s, %s) =>  \n" st1 st2;
    blocks_ls |> iter (fun (((st1', st2'), (sym, sig_sig_ls)): (T.state * T.state) * (T.symbol * (T.beta * T.beta) list)) ->
      noprintf "\t\t\t\t\t  (%s, %s)  ->_{" st1' st2'; pp_symbol sym; noprintf "}  "; pp_beta_beta_list sig_sig_ls))



let pp_sym_nts (sn: (T.symbol * T.state)) = 
  noprintf "( "; pp_symbol (fst sn); noprintf "  --->  State %s )" (snd sn) 

let pp_sym_nts_ls (sns: (T.symbol * T.state) list) =
  noprintf "\n\t(Trivial symbol, Trivial state) list: {";
  sns |> List.iter (fun sn -> noprintf "\n\t  "; pp_sym_nts sn); noprintf "   }\n"
let pp_ta (a: T.ta) =
  pp_upline (); pp_states (a.states); pp_alphabet (a.alphabet); 
  pp_final_states (a.final_states); pp_transitions_tbl (a.transitions); pp_loline ()

let pp_tree (e: T.tree) =
  let rec loop (e: T.tree) =
    match e with Leaf s -> noprintf " Leaf %s " s
    | Node (sym, subts) -> noprintf " Node ("; pp_symbol sym; noprintf " ["; 
      let len = List.length subts in subts |> List.iteri (fun i x -> 
        if (i = len-1) then loop x else (loop x; noprintf "; ")); noprintf "])"
  in loop e

let rec pp_repeat (n: int) (s: string) =
  if n = 0 then noprintf "" else (noprintf "%s" s; pp_repeat (n-1) s)

let pp_collected_from_conflicts (inp_ls: string list) =
  noprintf "  >> Collected the following lines from conflicts: \n";
  inp_ls |> iter (fun s -> noprintf "\t%s\n" s)

let pp_tree_pairs_syms (inp_ls: (T.tree * T.tree * string list) list) =
  noprintf "\n  >> Extracted trees and corresponding symbols: \n";
  inp_ls |> iter (fun (t1, t2, syms) -> 
    noprintf "\n\t>> First tree : "; pp_tree t1; noprintf "\n\t>> Second tree : "; pp_tree t2; 
    noprintf "\n\t>> Symbols : "; syms |> iter (noprintf "%s "); noprintf "\n")

let pp_tree_to_expr (e: T.tree) = 
  let is_empty_leaf (ts: T.tree list) = 
    match (hd ts, length ts) with (Leaf "ϵ"), 1 -> true | _ -> false in
  let rec ptree_loop (e: T.tree) =
    match e with Leaf s -> noprintf "%s " s
    | Node (sym, subts) -> 
      (* TO FIX: example trees gen with versatiles sometimes have arities not equal to (length subts) 
       *         so instead of using (length subts) should be able to use (snd sym) *)
      let s', rnk = (T.term_of_sym sym), (T.arity_of_sym sym) in 
      match s', rnk with 
      | _, 0 -> 
        if (rnk = 0 && is_empty_leaf subts) then (noprintf "%s " s')
        else raise (Failure "pp_tree_to_expr : no trivial symbol case")
      | "LPARENRPAREN", 1 ->
        noprintf "( LPAREN "; ptree_loop (nth subts 0); noprintf "RPAREN ) "
      | "LBRACERPRACE", 1 ->
        noprintf "( LBRACE "; ptree_loop (nth subts 0); noprintf "RBRACE ) "
      | _, 2 -> 
        noprintf "( "; ptree_loop (nth subts 0); 
        noprintf "( %s " s'; ptree_loop (nth subts 1); noprintf ") "
      | s, _ ->
        noprintf "( %s " s;  subts |> List.iter (fun t -> ptree_loop t); noprintf " ) "
         (* noprintf "Node %s with a rnk other than 1, 2 or 3!" (fst sym) *)
  in ptree_loop e  

let pp_expr_lst (sls:string list) = 
  sls |> iter (fun s -> noprintf " %s " s); noprintf "\n" 

let pp_restriction (r: T.restriction) = 
  match r with 
  | T.Assoc (s, i) -> (noprintf "("; pp_symbol s; noprintf ", Index:  %d) " i)
  | T.Prec (s, i) -> noprintf "("; pp_symbol s; noprintf ",  %i) " i

let pp_restriction_lst (rls:T.restriction list) =
  rls |> iter pp_restriction; noprintf "\n\n"


let pp_combined_trees (inp_ls: ((T.tree * (bool * bool * bool) * T.restriction list)) list) =
  noprintf "\n\n Resulted example trees: \n\n"; 
  inp_ls |> iter (fun ((t, (oa_pos, _oa_neg, op), rls)) -> 
    noprintf "\t* Tree : "; pp_tree t;
    noprintf "\n\t\t O_a : %b" oa_pos; noprintf "\n\t\t O_p : %b" op; 
    noprintf "\n\t\t Expression : "; pp_tree_to_expr t;
    noprintf "\n\t\t Restrictions : "; pp_restriction_lst rls; noprintf "\n") 

let pp_exprs (exprs_ls: (string list * string list) list) =
  noprintf "\n Resulted expressions: \n";
  exprs_ls |> iter (fun (ls1, ls2) -> 
    noprintf "\n\t * First expression : "; ls1 |> iter (noprintf "%s ");
    noprintf "\n\t * Second expression : "; ls2 |> iter (noprintf "%s "); noprintf "\n"); 
    noprintf "\n"


