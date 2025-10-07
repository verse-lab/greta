open Ta
open Treeutils
(* open Cfg *)

let pp_loline_new (debug: bool) = 
  let loleft, mid, loright = "\t╘----------", "----------", "----------╛" 
  in let mids = mid ^ mid ^ mid ^ mid in let line = loleft ^ mids ^ mids ^ loright 
  in if debug then Printf.printf "%s\n\n" line

let pp_upline_new (debug: bool) = 
  let upleft, mid, upright = "\n\n\n\t╒----------", "----------", "----------╕" 
  in let mids = mid ^ mid ^ mid ^ mid in let line = upleft ^ mids ^ mids ^ upright 
  in if debug then Printf.printf "%s\n\t " line

 let wrapped_printf debug fmt = 
    if debug then Printf.printf fmt else Printf.ifprintf stdout fmt 

(* Helpers *)

let find_intermediate_states (from_st: state) (trans_tbl: ((state * symbol), beta list) Hashtbl.t) 
  (debug: bool): state list = 
  let rec loop (from_sts: state list) (res_acc: state list) = 
    match from_sts with [] -> List.rev res_acc
    | from_stat :: from_st_tl -> 
      let to_acc: state list = 
        (let rhs_nonterms: beta list = Hashtbl.find_all trans_tbl (from_stat, epsilon_sym) |> List.flatten in 
          rhs_nonterms 
          |> List.map (fun s -> match s with S s' -> s' | T _ -> raise No_terminal_possible)) 
          |> List.filter (fun x -> not (List.mem x res_acc))
      in 
      if (List.is_empty to_acc)
      then loop from_st_tl res_acc
      else loop (from_st_tl @ to_acc) (to_acc @ res_acc)
  in let res = loop [from_st] [] in 
  if debug then (Printf.printf "\n\t\tFound intermediate states for State %s  :  " from_st; 
  Pp.pp_states res); res

let find_corr_trans_in_tbl (sym: symbol) (st: state) (tbl: ((state * symbol), beta list) Hashtbl.t): 
  (beta list) list = Hashtbl.find_all tbl (st, sym) 


(* --- helper to traverse alphabet and find transitions --- *)
(* let find_rhs_in_tbl (sym: symbol) (st: state) (interm_sts: state list) 
  (tbl: ((state * symbol), beta list) Hashtbl.t) (debug: bool): (beta list) list = 
  let open Printf in
  let rec find_rhs_loop (sym: symbol) (st: state) (interm_sts: state list) 
    (tbl: ((state * symbol), beta list) Hashtbl.t) (acc: (beta list) list) = 
    match Hashtbl.find_all tbl (st, sym) with 
    | [] -> (match interm_sts with 
            | [] -> 
              (* if debug then printf "\n\t\t => Interm states empty\n";  *) 
              acc
            | ist_hd :: ist_tl -> 
              (* if debug then printf "\n\t\t => Interm states not empty\n"; *)
              find_rhs_loop sym ist_hd ist_tl tbl acc)
    | sig_ls -> 
        (match interm_sts with 
        | [] -> sig_ls @ acc
        | ist_hd :: ist_tl -> 
          (* if debug then printf "\n\t => Interm states not empty\n"; *)
          find_rhs_loop sym ist_hd ist_tl tbl (sig_ls @ acc)
        )
  in let blsls_res = find_rhs_loop sym st interm_sts tbl [] in
  if debug then (printf "\n\t\t  Found rhs in tbl for ( State %s, symbol " st; 
    Pp.pp_symbol sym; printf ")\t"; blsls_res |> List.iter (fun bls -> Pp.pp_beta_list bls)); 
    blsls_res *)

let rec find_reachable_symbols_from_state (from_st: state) (tbl: ((state * symbol), beta list) Hashtbl.t): symbol list = 
  Hashtbl.fold (fun (st, sym) bls acc -> 
    if st = from_st 
    then 
      (if (syms_equals epsilon_sym sym)
      then 
        (let rhs_st = 
          match (List.hd bls) with S st -> st | T _ -> raise (Failure "find_reachable_symbols : T not possible here") in 
        let syms_from_linked_state = find_reachable_symbols_from_state rhs_st tbl in 
        acc @ syms_from_linked_state)
      else sym::acc )
  else acc) tbl [] |> List.rev
    
let reachable_symbols_from_states (states: state list) (tbl: ((state * symbol), beta list) Hashtbl.t): symbol list = 
  states |> List.fold_left (fun acc st -> 
    acc @ (find_reachable_symbols_from_state st tbl)) [] |> remove_dup_symbols

let cartesian_product_trans_from (starting_states: (state * state) list) 
  (trans_tbl1: ((state * symbol), beta list) Hashtbl.t) (trans_tbl2: ((state * symbol), beta list) Hashtbl.t) 
  (debug: bool): (((state * state) * symbol) * (beta * beta) list) list = 
  
  let res_cart_product_trans = starting_states |> List.fold_left (fun acc (st1, st2) -> 
    let interm_sts1 = find_intermediate_states st1 trans_tbl1 debug in 
    let interm_sts2 = find_intermediate_states st2 trans_tbl2 debug in  
    
    (* Consider intersected symbols list *)
    let symbols_reachable_from_sts1: symbol list = 
      reachable_symbols_from_states (st1::interm_sts1) trans_tbl1 in
    let symbols_reachable_from_sts2: symbol list = 
      reachable_symbols_from_states (st2::interm_sts2) trans_tbl2 in
    let symbols_common: symbol list = 
      symbols_in_both_lists symbols_reachable_from_sts1 symbols_reachable_from_sts2 in 
      (wrapped_printf debug "\n\t\tSymbols in common: "; symbols_common |> List.iter Pp.pp_symbol;
      wrapped_printf debug "\n");
    let beta_pair_lsls: (symbol * (beta * beta) list) list = 
      symbols_common |> List.fold_left (fun acc sym -> 
        let rhs_blsls1: (beta list) list = find_corr_trans_in_tbl sym st1 trans_tbl1 in
        let rhs_blsls2: (beta list) list = find_corr_trans_in_tbl sym st2 trans_tbl2 in
        let beta_pair_lsls: (beta * beta) list list = cross_product_raw_betapair_ls rhs_blsls1 rhs_blsls2 debug in 
        let to_acc: (symbol * (beta * beta) list) list = 
          beta_pair_lsls |> List.map (fun beta_pair_ls -> (sym, beta_pair_ls)) 
        in to_acc @ acc 
        ) [] 
    in 
    let to_acc: (((state * state) * symbol) * (beta * beta) list) list = 
      beta_pair_lsls |> List.map (fun (sym, b1b2_ls) -> ((st1, st2), sym), b1b2_ls)
    in to_acc @ acc
    ) [] in 
  (wrapped_printf debug "\n\t Resulted cartesian product transitions:\n"; 
  Pp.pp_raw_cart_product_trans res_cart_product_trans);
  res_cart_product_trans

let reachable_beta_lsls_from_state_symbol (state: state) (sym: symbol) (tbl: ((state * symbol), beta list) Hashtbl.t)
  (debug: bool): beta list list = 
  let reachabe_states: state list = find_intermediate_states state tbl debug in 
  [state] @ reachabe_states |> List.fold_left (fun acc curr_st -> 
    let to_acc: beta list list = Hashtbl.find_all tbl (curr_st, sym) in 
    to_acc @ acc) [] 


(* 'cartesian_product_trans_from_for_sym' is concerned with a particular 'sym' from 'sts_pair' *)
let cartesian_product_trans_from_for_sym (sts_pair: (state * state)) (sym: symbol) 
  (tbl1: ((state * symbol), beta list) Hashtbl.t) (tbl2: ((state * symbol), beta list) Hashtbl.t)
  (debug: bool): (((state * state) * symbol) * (beta * beta) list) list = 
  let st1, st2 = (fst sts_pair), (snd sts_pair) in 
  let blsls1: beta list list = 
    reachable_beta_lsls_from_state_symbol st1 sym tbl1 debug in 
  wrapped_printf debug "\n\t Beta list list in Transitions1 for State %s, Symbol " st1; Pp.pp_symbol sym; 
    blsls1 |> List.iter Pp.pp_beta_list; wrapped_printf debug "\n";

  let blsls2: beta list list = 
    reachable_beta_lsls_from_state_symbol st2 sym tbl2 debug in 
  wrapped_printf debug "\n\t Beta list list in Transitions2 for State %s, Symbol " st2; Pp.pp_symbol sym; 
    blsls2 |> List.iter Pp.pp_beta_list; wrapped_printf debug "\n";
  
  let beta_pair_lsls: (beta * beta) list list = cross_product_raw_betapair_ls blsls1 blsls2 debug in 
  beta_pair_lsls |> List.map (fun beta_pair_ls -> (sym, beta_pair_ls)) 
  |> List.map (fun (sym, b1b2_ls) -> ((st1, st2), sym), b1b2_ls)


(* let reachable_sts_pairs_without (beta_pair_ls: (beta * beta) list) (sts_pair: (state * state)): (state * state) list = 
  beta_pair_ls |> List.filter are_states_pair |> List.map beta_pair_to_states_pair |> List.filter (fun x -> not (are_same_states_pairs sts_pair x)) *)

let find_reachable_states (trans_ls: (((state * state) * symbol) * (beta * beta) list) list): (state * state) list = 
  trans_ls |> List.fold_left (fun acc ((_from_sts_pair, _sym), beta_pair_ls) -> 
    let curr_reachable_sts: (state * state) list = 
      beta_pair_ls |> List.filter are_states_pair |> List.map beta_pair_to_states_pair
    in 
    curr_reachable_sts @ acc) [] |> Cfgutils.remove_dups


let collect_existing_raw_trans_for_states_pair (from_states: state * state) (raw_trans: (((state * state) * symbol) * (beta * beta) list) list): 
  ((state * state) * (symbol * (beta * beta) list)) list  = 
  let from_st1, from_st2 = fst from_states, snd from_states 
  in raw_trans |> List.fold_left (fun acc (((st1, st2), sym), bb_ls) ->
    if ((String.equal from_st1 st1) && (String.equal from_st2 st2))
    then ((st1, st2), (sym, bb_ls)) :: acc
    else acc) []

(* helper for 'find_duplicate_state_pairs_in_trans_blocks' *)
let find_state_pair_w_same_raw_rhs (sym_rhs_raw_states: (symbol * (beta * beta) list) list) 
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list): (state * state) list = 
  let rec loop ls acc = 
    match ls with [] -> List.rev acc 
    | (st_pair', raw_trans_ls') :: tl -> 
      let curr_sym_and_rhs_state_pairs = sym_and_rhs_beta_pairs raw_trans_ls' in
      if (same_sym_and_rhs_beta_pairs curr_sym_and_rhs_state_pairs sym_rhs_raw_states)
      then loop tl (st_pair'::acc)
      else loop tl acc
  in loop trans_blocks_ls []

let find_duplicate_state_pairs_in_trans_blocks 
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (_debug: bool): ((state * state) * (state * state)) list =
  let rec traverse_trans_blocks (ls: ((state * state) * (((state * state) * (symbol * (beta * beta) list))) list) list) 
    (acc: ((state * state) * (state * state)) list) =
    match ls with [] -> acc 
    | (st_pair, raw_trans_ls) :: tl -> 
      let sym_and_rhs_raw_sigmas = raw_trans_ls |> sym_and_rhs_beta_pairs in
      let same_rhs_states_ls: (state * state) list = find_state_pair_w_same_raw_rhs sym_and_rhs_raw_sigmas tl in
      let state_pairs_to_acc = same_rhs_states_ls |> List.map (fun st_pair' -> (st_pair', st_pair)) in
      traverse_trans_blocks tl (acc @ state_pairs_to_acc)
  in let res_state_pairs = traverse_trans_blocks trans_blocks_ls [] in 
  (* if debug then (Printf.printf "\n\t * Duplicate states pairs : \n\t"; 
    res_state_pairs |> List.iter (fun pair_of_stats_pair ->  
      Pp.pp_raw_pair_of_state_pairs pair_of_stats_pair; wrapped_printf debug "\n\t"));  *)
  res_state_pairs

let remove_transitions_of_duplicate_states (dup_states_ls: ((state * state) * (state * state)) list) 
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (_debug: bool): ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list =
  let trans_blocks = ref trans_blocks in
    dup_states_ls |> List.iter (fun (_, sts_pair2) ->   
      trans_blocks := !trans_blocks |> List.filter (fun ((st_pair, _ (* block *)): 
      (state * state) * ((state * state) * (symbol * (beta * beta) list)) list) -> 
        (* if there is sts_pair1 in trans_blocks, then remove sts_pair2's block *)
        not (state_pairs_equal st_pair sts_pair2))); 
  (* if debug then (Printf.printf "\n\t * Results of removing : \n"; !trans_blocks |> Pp.pp_raw_trans_blocks); *)
  !trans_blocks

let replace_dups_in_block (dup_states_ls: ((state * state) * (state * state)) list) 
  (trans_block: ((state * state) * (symbol * (beta * beta) list)) list): 
  ((state * state) * (symbol * (beta * beta) list)) list =
  let rec replace_dup_pair_loop ((dup_sts_pair1, dup_sts_pair2): (state * state) * (state * state)) 
    (ls: ((state * state) * (symbol * (beta * beta) list)) list) acc = 
    match ls with [] -> List.rev acc
    | (st_pair, (sym, rhs_sig_pair_ls)) :: tl -> 
      let new_rhs_sig_pair_ls: (beta * beta) list = rhs_sig_pair_ls |> 
        List.map (fun sig_pair -> 
          if (beta_pair_equals_state_pair sig_pair dup_sts_pair2) 
          then 
            (let st1, st2 = (fst dup_sts_pair1), (snd dup_sts_pair1) 
             in (S st1, S st2)) 
          else sig_pair) 
      in 
      let new_raw_trans: (state * state) * (symbol * (beta * beta) list) = (st_pair, (sym, new_rhs_sig_pair_ls)) in 
      replace_dup_pair_loop (dup_sts_pair1, dup_sts_pair2) tl (new_raw_trans::acc)
  and 
  traverse_dups dups_ls trans_blck_acc =
    match dups_ls with [] -> trans_blck_acc 
    | (dup_st1, dup_st2) :: dup_tl -> 
      let new_trans_block = replace_dup_pair_loop (dup_st1, dup_st2) trans_blck_acc []
      in traverse_dups dup_tl new_trans_block
  in traverse_dups dup_states_ls trans_block

let replace_dup_state_names (dup_states_ls: ((state * state) * (state * state)) list) 
(trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
(_debug: bool): ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list =
  let rec traverse_blocks ls acc =
    match ls with [] -> List.rev acc
    | (st_pr1, trans_lst_block) :: tl ->
      let replaced_dups_block: ((state * state) * (symbol * (beta * beta) list)) list = 
        replace_dups_in_block dup_states_ls trans_lst_block in 
      traverse_blocks tl ((st_pr1, replaced_dups_block)::acc)
  in let res_trans_blocks = traverse_blocks trans_blocks [] in 
  (* if debug then (Printf.printf "\n\t * Results of replacing : \n"; res_trans_blocks |> Pp.pp_raw_trans_blocks); *)
  res_trans_blocks

let collect_unique_states_and_map_to_new_states 
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (start_states: (state * state) list) (debug: bool): ((state * state) * (state * state)) list =
  let unique_states_ls: (state * state) list = 
    trans_blocks |> List.map (fun (st_pair, _) -> st_pair) in
  let rec map_loop ls cnt start_cnt acc: ((state * state) * (state * state)) list =
    match ls with [] -> List.rev acc 
    | st_pair_hd :: tl -> 
      if (List.mem st_pair_hd start_states)
      then
        (let mapped_pair: (state * state) = 
            gen_informative_name_pair st_pair_hd start_cnt
            (* "e" ^ (string_of_int start_cnt), ""  *)
         in 
         let to_acc: (state * state) * (state * state) = (st_pair_hd, mapped_pair) in
         map_loop tl cnt (start_cnt+1) (to_acc::acc)) 
      else
        (let mapped_pair: (state * state) = 
          gen_informative_name_pair st_pair_hd cnt
          (* "x" ^ (string_of_int cnt), ""  *)
        in 
        let to_acc: (state * state) * (state * state) = (st_pair_hd, mapped_pair) in 
        map_loop tl (cnt+1) start_cnt (to_acc::acc))
  in let res_map = map_loop unique_states_ls 1 1 [] in 
  if debug then (wrapped_printf debug "\n\t * Results of unique states to new states mapping : ";
  res_map |> List.iter (fun sts_pair_pair -> wrapped_printf debug "\n\t"; Pp.pp_raw_pair_of_state_pairs sts_pair_pair));
  res_map





(** Intersection of tree automata *)
let intersect (a1: ta) (a2: ta) (debug: bool): ta =
  let wrapped_printf fmt = 
    if debug then Printf.printf fmt else Printf.ifprintf stdout fmt 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 1 - Find the set of final states, ie, Q_f := Q_f_1 x Q_f_2 *)
  
  let final_states_raw: (state * state) list = 
    a1.final_states |> List.map (fun s1 -> a2.final_states |> List.map (fun s2 -> (s1, s2))) |> List.flatten
  in

  (pp_upline_new debug; wrapped_printf  "### Step 1 - Found the set of final states\n\t"; 
  final_states_raw |> Pp.pp_raw_states; pp_loline_new debug);


  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 2 - Get raw transitions for symbols that 'final_states_raw' from Q_f *)

  let raw_init_trans_ls: (((state * state) * symbol) * (beta * beta) list) list = 
      cartesian_product_trans_from final_states_raw a1.transitions a2.transitions debug 
  in
  
  (pp_upline_new debug; wrapped_printf "### Step 2 - Find final states-starting transitions : \n\t"; 
  Pp.pp_raw_transitions raw_init_trans_ls; pp_loline_new debug);


  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 3 - Find reachable states based on final-states-starting transitions *)
  
  let init_reachable_states: (state * state) list = 
    find_reachable_states raw_init_trans_ls 
    (* Exclude the 'final_states_raw' from the 'init_reachable_states' *)
    |> List.filter (fun x -> (not (List.mem x final_states_raw)))
  in
  (if debug then pp_upline_new debug; 
  wrapped_printf "### Step 3 - Found reachable states based on intial states-starting transitions\n\t"; 
  Pp.pp_raw_states init_reachable_states; pp_loline_new debug);
  

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 4 - Based on (Ei, Ej) in list of reachable states, find transitions starting from (Ei, Ej) *)
  
  let rec collect_from_all_reachable_states (states_reachable_left: (state * state) list) 
    (states_reachabe_acc: (state * state) list) (trans_acc: (((state * state) * symbol) * (beta * beta) list) list): 
    (((state * state) * symbol) * (beta * beta) list) list * (state * state) list = 
    match states_reachable_left with 
    | [] -> List.rev trans_acc, List.rev states_reachabe_acc
    | (st_hd1, st_hd2) :: reachable_states_tl -> 
      if debug then (wrapped_printf "\n\t * Looking for raw trans from (%s, %s) \n" st_hd1 st_hd2);
      
      (* --- find transitions from the curr states pair --- *)
      let alph1: symbol list = 
        find_reachable_symbols_from_state st_hd1 a1.transitions in 
      let alph2: symbol list = 
        find_reachable_symbols_from_state st_hd2 a2.transitions in  
      let alph_overlapped: symbol list = 
        symbols_in_both_lists alph1 alph2
      in 
        wrapped_printf "\n\t Symbols common in states %s %s:   " st_hd1 st_hd2; 
        alph_overlapped |> List.iter Pp.pp_symbol;
      
      let curr_raw_trans_from_states_pair: 
        (((state * state) * symbol) * (beta * beta) list) list = 
        alph_overlapped |> List.fold_left (fun acc sym -> 
          let trans_to_acc: (((state * state) * symbol) * (beta * beta) list) list = 
            cartesian_product_trans_from_for_sym (st_hd1, st_hd2) sym a1.transitions a2.transitions debug 
          in 
            trans_to_acc @ acc) [] 
      in
      (* Collect reachable states from the curr states pair *)
      let curr_reachable_states = 
        find_reachable_states curr_raw_trans_from_states_pair
      in
      (* Pass in as new 'states_reachable', the ones that do not already appeared *)
      let new_states_reachable_to_add: (state * state) list = 
        curr_reachable_states |> List.filter (fun x -> not (List.mem x states_reachabe_acc))
      in
        (* 
        (wrapped_printf "\t ---> Found all reachabe states :  "; 
        curr_reachable_states |> List.iter Pp.pp_raw_state; wrapped_printf "\n"); 
        *)

        (* Check which new reachable states have been added *)
        if (List.is_empty new_states_reachable_to_add) then wrapped_printf "\t => Found NO new reachable states \n\n\n\n" 
        else 
          (wrapped_printf "\t => Found new reachabe states to add :  "; 
          new_states_reachable_to_add |> List.iter Pp.pp_raw_state; wrapped_printf "\n\n\n\n");

        collect_from_all_reachable_states 
          (reachable_states_tl @ new_states_reachable_to_add) 
          (states_reachabe_acc @ new_states_reachable_to_add)
          (curr_raw_trans_from_states_pair @ trans_acc)
  in 
  let raw_trans_from_all_reachables, all_raw_states_reachable = 
    collect_from_all_reachable_states init_reachable_states init_reachable_states [] in 
  let all_raw_trans = 
    raw_init_trans_ls @ raw_trans_from_all_reachables in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 4 - Found all the raw trasitions from all the reachable states  : \n\t"; 
  Pp.pp_raw_transitions all_raw_trans; pp_loline_new debug);


  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 5 - Write the 'all_raw_trans' in blocks and sort them for better comparison *)
  let raw_trans_in_blocks_sorted: 
    ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list =
  
    (final_states_raw @ all_raw_states_reachable) |> List.fold_left (fun acc (s1, s2) -> 
      let block: ((state * state) * (symbol * (beta * beta) list)) list = 
        collect_existing_raw_trans_for_states_pair (s1, s2) all_raw_trans 
      in 
        acc @ [(s1, s2), block]) []
      |> List.stable_sort (fun (_, trans_ls1) (_, trans_ls2) -> 
      Int.compare (List.length trans_ls2) (List.length trans_ls1))
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 5 - Putt raw transitions in blocks of transitions : \n\t";
    Pp.pp_raw_trans_blocks raw_trans_in_blocks_sorted; pp_loline_new debug);


  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 6 - Find a list of duplicate states pairs *)
  
  let dup_states_pair_ls: ((state * state) * (state * state)) list = 
    find_duplicate_state_pairs_in_trans_blocks raw_trans_in_blocks_sorted debug
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 6 - Find a list of duplicate states pairs : \n";
  dup_states_pair_ls |> List.iter (fun ls -> wrapped_printf "\n\t"; Pp.pp_raw_pair_of_state_pairs ls); 
  pp_loline_new debug);


  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 7 - Remove transitions based on 'dup_states_pair_ls' *)  
  let raw_trans_blocks_cleaned: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    remove_transitions_of_duplicate_states dup_states_pair_ls raw_trans_in_blocks_sorted debug
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 7 - Remove raw transitions wrt duplicate states pairs : \n";
  Pp.pp_raw_trans_blocks raw_trans_blocks_cleaned; pp_loline_new debug);


  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 8 - Replace state names based on 'dup_states_pair_ls' *)  
  
  let trans_blocks_replaced: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    replace_dup_state_names dup_states_pair_ls raw_trans_blocks_cleaned debug
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 8 - Replace state names wrt duplicate states pairs : \n";
    Pp.pp_raw_trans_blocks trans_blocks_replaced; pp_loline_new debug);

  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 9 - Rename states and populate Q, raw trans blocks *)  
  
  let states_renaming_map: ((state * state) * (state * state)) list =
    collect_unique_states_and_map_to_new_states trans_blocks_replaced final_states_raw debug in
  
  let state_pairs_renamed: (state * state) list = 
    states_renaming_map |> List.map snd in 
  let res_states_fst: state list = state_pairs_renamed |> List.map state_pair_append in 
  let trans_blocks_renamed: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    rename_trans_blocks states_renaming_map trans_blocks_replaced debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 9 - Rename states in Q and raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_renamed; pp_loline_new debug);


  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 10 - Remove duplicate transitions in each raw trans block *)  
  let trans_blocks_wo_dup_trans = remove_dup_trans_for_each_block trans_blocks_renamed debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 10 - Removed dup trans in raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_wo_dup_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 11 - Introduce epsilon transitions to simplify raw trans blocks *)
  let trans_blocks_simplified_eps_trans: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    simplify_trans_blocks_with_epsilon_transitions trans_blocks_renamed (List.rev state_pairs_renamed) debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 11 - Introduce epsilon transitions to simplify raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_eps_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step X - Convert raw trans blocks to transitions format ((state, symbol), sigma list list) Hashtbl.t *)
  let res_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    trans_blocks_simplified_eps_trans |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, sig_sig_ls)) -> 
        let trans_new = ((st_pair, sym), sig_sig_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in 
  Pp.pp_raw_transitions res_raw_trans;
  let res_trans_ls: ((state * symbol) * beta list) list = 
    res_raw_trans |> List.map (fun ((st_pair, sym), sig_sig_ls) -> 
      let new_sig_ls = sig_sig_ls |> List.map fst in ((fst st_pair), sym), new_sig_ls) 
  in 
  let res_trans_tbl: ((state * symbol), beta list) Hashtbl.t = Hashtbl.create (Hashtbl.length a2.transitions) in
    res_trans_ls |> List.iter (fun ((st, sym), beta_ls) -> 
      Hashtbl.add res_trans_tbl (st, sym) beta_ls);

  (* let states_to_remove = dup_states_pair_ls_after_eps_intro |> List.map snd |> List.map fst in *)
  let res_states_final = res_states_fst in
  let corr_final_states: state list = 
    
    final_states_raw |> List.map fst 
    (* let start_states_mapped = states_rename_map |> List.filter (fun (x, _) -> List.mem x start_states_prev_fsts) in 
    if (List.length start_states_mapped) > 1 then       
      states_rename_map |> List.filter (fun (x, y) -> List.mem (x, y) start_states_prev) |> List.map snd
    else start_states_mapped |> List.map snd  *)
  in 

  wrapped_printf"\nIntersect the following 2 TAs:\n\n  (1) First TA:\n"; Pp.pp_ta a1; 
  wrapped_printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; wrapped_printf "\n\n";

  let res_ta: ta = 
    { states = res_states_final ; alphabet = a1.alphabet ; final_states = corr_final_states; terminals = [];
    transitions = res_trans_tbl } 
  in
  wrapped_printf "\n ** Result of TA intersection: \n"; Pp.pp_ta res_ta; wrapped_printf "\n\n"; 
  res_ta


(* 

let remove_dup_sigma_sigma_ls (sig_sig_ls: (sigma * sigma) list): (sigma * sigma) list = 
  List.rev (List.fold_left cons_uniq [] sig_sig_ls)

let find_all_interm_sts (sym_rhs_stat_pairs_ls: (symbol * (sigma * sigma) list) list) 
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): (sigma * sigma) list = 
  let curr_interm_sts: (sigma * sigma) list = 
    sym_rhs_stat_pairs_ls |> List.fold_left (fun acc (sym, sgsgls) -> 
      if (syms_equals sym epsilon_symb) then (List.hd sgsgls) :: acc else acc) [] 
  in
  let rec find_interm_stat_pairs_loop (from_sts_ls: (sigma * sigma) list) (sts_acc: (sigma * sigma) list) = 
    match from_sts_ls with 
    | [] -> sts_acc |> remove_dup_sigma_sigma_ls
    | hd_sigsig :: tl_sigsigs -> 
      let from_sts: nonterminal * nonterminal = 
        match hd_sigsig with (Nt st1, Nt st2) -> (st1, st2) | _ -> raise Not_possible in
      let hd_sym_rhs_stat_pairs_ls: (symbol * (sigma * sigma) list) list = 
        trans_blocks_ls |> List.filter (fun (sts_pr, _sts_sym_sigsigls) -> state_pairs_equal sts_pr from_sts) 
        |> List.map (fun (_sts_par, sts_sym_sigsigls) -> sts_sym_sigsigls) |> List.flatten |> sym_and_rhs_sigma_pairs  in
      let interms_from_hd_sigsig: (sigma * sigma) list = 
        hd_sym_rhs_stat_pairs_ls |> List.fold_left (fun acc (sym, sgsgls) -> 
          if (syms_equals sym epsilon_symb) then (List.hd sgsgls) :: acc else acc) [] in
      if (List.is_empty interms_from_hd_sigsig) 
      then find_interm_stat_pairs_loop tl_sigsigs sts_acc
      else find_interm_stat_pairs_loop (tl_sigsigs @ interms_from_hd_sigsig) (sts_acc @ interms_from_hd_sigsig)
  in let res = find_interm_stat_pairs_loop curr_interm_sts curr_interm_sts in 
  if debug then (Printf.printf "\n\t\t Found all intermiedate states : \n" ; res |> Pp.pp_sigma_sigma_list); res

let eps_trans_accounted_mem (sym_rhs_stat_pairs1: symbol * (sigma * sigma) list) 
  (sym_rhs_stat_pairs_ls2: (symbol * (sigma * sigma) list) list)
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): bool = 
  if List.mem sym_rhs_stat_pairs1 sym_rhs_stat_pairs_ls2 
  then true 
  else (if (syms_equals (fst sym_rhs_stat_pairs1) epsilon_symb)
        then 
          (let eps_rhs_stat1: sigma * sigma = (List.hd (snd sym_rhs_stat_pairs1)) in
           let interm_sts2: (sigma * sigma) list = find_all_interm_sts sym_rhs_stat_pairs_ls2 trans_blocks_ls debug in 
           List.mem eps_rhs_stat1 interm_sts2) 
        else false)

let same_sym_and_rhs_sigma_pairs_w_eps_intro 
  (sym_rhs_stat_pairs1: (symbol * (sigma * sigma) list) list) 
  (sym_rhs_stat_pairs2: (symbol * (sigma * sigma) list) list) 
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): bool =
  if debug then (Printf.printf "\n\t Is same sym and rhs sigma pairs after eps intro?\n");
  let res = 
    if not ((List.length sym_rhs_stat_pairs1) = (List.length sym_rhs_stat_pairs2))
    then false
    else (let rec traverse ls = match ls with [] -> true 
          | (sym_rhs_states: symbol * (sigma * sigma) list) :: tl -> 
            (if (eps_trans_accounted_mem sym_rhs_states sym_rhs_stat_pairs2 trans_blocks_ls debug)
              then traverse tl
              else false)
          in traverse sym_rhs_stat_pairs1) in 
  if debug then (if res then Printf.printf "\t\tYES\n" else Printf.printf "\t\tNO\n"); res

(* helper for 'find_duplicate_state_pairs_after_eps_intro' *)
let find_state_pair_w_same_rhs_w_eps_intro 
  (sym_rhs_raw_states: (symbol * (sigma * sigma) list) list) 
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): (state * state) list = 
  let rec loop blocks_ls acc = 
    match blocks_ls with [] -> acc 
    | (st_pair', raw_trans_ls') :: blocks_tl -> 
      let curr_sym_rhs_state_pairs: (symbol * (sigma * sigma) list) list = sym_and_rhs_sigma_pairs raw_trans_ls' in
      if (same_sym_and_rhs_sigma_pairs_w_eps_intro sym_rhs_raw_states curr_sym_rhs_state_pairs trans_blocks_ls debug)
      then loop blocks_tl (st_pair'::acc)
      else loop blocks_tl acc
  in loop trans_blocks_ls []

let not_same_sts_pair (st_pair1: state * state) (st_pair2: state * state) = 
  not (String.equal (fst st_pair1) (fst st_pair2))
  
let find_duplicate_state_pairs_after_eps_intro
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list) 
  (debug: bool): ((state * state) * (state * state)) list =
  let rec traverse_trans_blocks (blocks_ls: ((state * state) * (((state * state) * (symbol * (sigma * sigma) list))) list) list) 
    (acc: ((state * state) * (state * state)) list) =
    match blocks_ls with [] -> acc 
    | (st_pair, raw_trans_ls) :: blocks_tl -> 
      let sym_and_rhs_raw_sigmas: (symbol * (sigma * sigma) list) list = raw_trans_ls |> sym_and_rhs_sigma_pairs in
      let same_rhs_states_ls: (state * state) list = 
        find_state_pair_w_same_rhs_w_eps_intro sym_and_rhs_raw_sigmas trans_blocks_ls debug in
      let state_pairs_to_acc = same_rhs_states_ls |> List.filter (fun st_pair' -> not_same_sts_pair st_pair' st_pair) 
        |> List.map (fun st_pair' -> (st_pair', st_pair)) in
      traverse_trans_blocks blocks_tl (acc @ state_pairs_to_acc)
  in let res_state_pairs = traverse_trans_blocks trans_blocks_ls [] in 
  if debug then (Printf.printf "\n\t >> Duplicate states pairs (after epsilon introduction) : "; 
    res_state_pairs |> List.iter (fun pair_of_stats_pair ->  Pp.pp_raw_pair_of_state_pairs pair_of_stats_pair)); 
  res_state_pairs




  

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 12 - Find duplicates after epsilon introduction *)
  let dup_states_pair_ls_after_eps_intro: ((state * state) * (state * state)) list = 
    (if debug_print then printf "\n*** Finding duplicate raw_states pairs : \n");
    (* TODO: To start from here! *)
    find_duplicate_state_pairs_after_eps_intro trans_blocks_simplified_eps_trans_trimmed debug_print
  in
  (if debug_print then begin pp_upline_new (); printf "### Step 12 - Found duplicate states pairs after eps introduction : \n";
    dup_states_pair_ls_after_eps_intro |> List.iter (fun ls -> printf "\n\t"; Pp.pp_raw_pair_of_state_pairs ls); pp_loline_new () end);

  
  

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 13 - Remove duplicates after epsilon introduction to simplify further *)
  let trans_blocks_simplified_further = 
    let trans_blocks_after_removing = 
      remove_transitions_of_duplicate_states dup_states_pair_ls_after_eps_intro trans_blocks_simplified_eps_trans_trimmed debug_print 
    in 
      replace_dup_state_names dup_states_pair_ls_after_eps_intro trans_blocks_after_removing debug_print
      (* |> remove_meaningless_transitions  *)
      (* Note: Needed to comment the above out for G2a *)
  in
  (if debug_print then begin pp_upline_new (); printf "### Step 13 - Remove duplicates after eps introduction to simplify further : \n";
    Pp.pp_raw_trans_blocks trans_blocks_simplified_further; pp_loline_new () end);

  
  triv_sym_state_ls |> List.iter (fun (sym, st) -> 
    Hashtbl.add res_trans_tbl (st, sym) [[(T (fst sym))]]);
  (if debug_print then begin pp_upline_new (); printf "### Step 14 - Converted trans blocks (and trivial trans) to transitions hashtbl : \n";
    Pp.pp_transitions_tbl res_trans_tbl; pp_loline_new () end);

  let states_rename_map: (state * state) list = 
    states_renaming_map |> List.map (fun ((orig_st, _), (new_st, _)) -> (orig_st, new_st))
  in 
  


 *)


  