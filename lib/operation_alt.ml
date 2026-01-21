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

(* ---------------------------------- Helpers ---------------------------------- *)

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

let rec collect_eps_connected_states_from_states_pair (from_states: state * state) (raw_trans: (((state * state) * symbol) * (beta * beta) list) list): 
  (state * state) list = 
  let from_st1, from_st2 = (fst from_states), (snd from_states) in 
  raw_trans |> List.fold_left (fun acc (((st1, st2), sym), beta_pair_ls) -> 
    if (String.equal st1 from_st1) && (String.equal st2 from_st2) && (syms_equals sym epsilon_sym)
    then 
      (let rhs_sts_pair_ls: (state * state) list = 
        beta_pair_ls |> List.filter are_states_pair |> List.map beta_pair_to_states_pair in 
      let rhs_sts_pair: (state * state) = 
        if (List.is_empty rhs_sts_pair_ls) || ((List.length rhs_sts_pair_ls) > 1) 
        then raise (Failure "find_epsilon_trans_connection : not State_pair ->_eps_sym State_pair")
        else rhs_sts_pair_ls |> List.hd 
      in 
      let eps_connected_states_from_rhs_sts_pair: (state * state) list = 
        collect_eps_connected_states_from_states_pair rhs_sts_pair raw_trans
      in  
        (rhs_sts_pair :: eps_connected_states_from_rhs_sts_pair) @ acc)
    else acc) []

let find_epsilon_transition_connection_in_states (state_pair_ls: (state * state) list) (trans: (((state * state) * symbol) * (beta * beta) list) list) 
  (_debug: bool): ((state * state) * ((state * state) list)) list = 

  let state_eps_connected_states: ((state * state) * (state * state) list) list = 
    state_pair_ls |> List.fold_left (fun acc sts_pair -> 
      let eps_connected_sts_pair_ls: (state * state) list = 
        collect_eps_connected_states_from_states_pair sts_pair trans 
      in 
        if (List.is_empty eps_connected_sts_pair_ls) then acc 
        else
         (sts_pair, eps_connected_sts_pair_ls) :: acc) []
  in
  (* if debug then (wrapped_printf debug "\n\t * State -> Epsilon connected states - sorted to increasing length (connected states) : \n"; 
    state_eps_connected_states |> List.iter (fun (sts_pair, sts_pair_ls) -> 
      wrapped_printf debug "\t\t State (%s, %s) is epsilon-connected to -> " (fst sts_pair) (snd sts_pair); 
      sts_pair_ls |> Pp.pp_raw_states)); *)
  state_eps_connected_states 

let beta_pair_ls_contains_sts_pair (beta_pair_ls: (beta * beta) list) (dead_sts_pair: state * state): bool = 
  let corr_sts_pair_ls: (state * state) list = 
    beta_pair_ls |> List.map beta_pair_to_state_or_terminal_pair in 
  List.mem dead_sts_pair corr_sts_pair_ls
  (* beta_pair_ls |> List.fold_left (fun bool_acc curr_beta_pair -> 
    if (are_states_pair curr_beta_pair) then 
      let corresponding_states_pair = beta_pair_to_states_pair curr_beta_pair in 
      let curr_equal = state_pairs_equal corresponding_states_pair dead_sts_pair in
      curr_equal && bool_acc
    else bool_acc) false *)

let clean_raw_transitions_per_dead_states_pair (raw_trans: (((state * state) * symbol) * (beta * beta) list) list) 
  (dead_sts_pair: state * state) (debug: bool): (((state * state) * symbol) * (beta * beta) list) list = 
  let cleaned_raw_trans = 
    raw_trans |> List.filter (fun ((sts_pair, _sym), beta_pair_ls) -> 
      let res_bool = 
        not (state_pairs_equal dead_sts_pair sts_pair) && not (beta_pair_ls_contains_sts_pair beta_pair_ls dead_sts_pair)
      in
      (* 
      wrapped_printf debug "\n\t For states pair"; Pp.pp_raw_state sts_pair; 
      wrapped_printf debug "\n\t Does states equal to dead state_pair?  "; Pp.pp_raw_state dead_sts_pair;
      wrapped_printf debug "\n\t %b Or Does beta list include the dead states pair? %b " (state_pairs_equal dead_sts_pair sts_pair) (beta_pair_ls_contains_sts_pair beta_pair_ls dead_sts_pair);
      *)
       res_bool
      ) 
  in if debug then (wrapped_printf debug "\n\t * Cleaned raw transitions per dead state (%s, %s)" (fst dead_sts_pair) (snd dead_sts_pair);
  raw_trans |> Pp.pp_raw_transitions); 
  cleaned_raw_trans

let clean_raw_transitions_wrt_dead_states (raw_trans: (((state * state) * symbol) * (beta * beta) list) list) 
  (dead_states: (state * state) list) (debug: bool): (((state * state) * symbol) * (beta * beta) list) list = 
  dead_states |> List.fold_left (fun trans_acc dead_sts_pair -> 
    clean_raw_transitions_per_dead_states_pair trans_acc dead_sts_pair debug
    ) raw_trans

let clean_raw_trans_blocks_per_dead_states_pair (trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (dead_sts_pair: state * state) (debug: bool): ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list =

  let cleaned_raw_trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    trans_blocks |> List.filter (fun (sts_pair, _sts_pair_to_sym_bbls_ls) ->
      not (state_pairs_equal dead_sts_pair sts_pair)) 
  in 
  let res_raw_trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    cleaned_raw_trans_blocks |> List.map (fun (sts_pair, sts_pair_to_sym_bbls_ls) ->
      let cleaned_sts_pair_to_sym_bbls_ls: ((state * state) * (symbol * (beta * beta) list)) list = 
        sts_pair_to_sym_bbls_ls |> List.fold_left (fun acc (sts_pair, sym_bbls) -> 
          let sym, bbls = 
            (fst sym_bbls), (snd sym_bbls) 
          in 
          let cleaned_sym_bbls: (beta * beta) list = 
              if (beta_pair_ls_contains_sts_pair bbls dead_sts_pair)
              then []
              else bbls
          in 
            if (List.is_empty cleaned_sym_bbls) then acc 
            else
              (sts_pair, (sym, cleaned_sym_bbls)) :: acc
          
        ) [] |> List.rev
      in 
        (sts_pair, cleaned_sts_pair_to_sym_bbls_ls)
    )
  in 
  if debug then (wrapped_printf debug "\n\t * Cleaned raw transb per dead state (%s, %s)" (fst dead_sts_pair) (snd dead_sts_pair);
  res_raw_trans_blocks |> Pp.pp_raw_trans_blocks); 
  res_raw_trans_blocks

let clean_raw_trans_blocks_wrt_dead_states (trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (dead_states: (state * state) list) (debug: bool): ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
  dead_states |> List.fold_left (fun trans_blocks_acc dead_sts_pair ->
    clean_raw_trans_blocks_per_dead_states_pair trans_blocks_acc dead_sts_pair debug
    ) trans_blocks

let replace_beta_beta_list_from_sts_to_sts (betapair_ls: (beta * beta) list) (to_replace_sts: state * state) (new_sts: state * state): 
  (beta * beta) list = 
  let to_replace_beta_pair = states_pair_to_beta_pair to_replace_sts in 
  let new_beta_pair = states_pair_to_beta_pair new_sts in 
  betapair_ls |> List.map (fun curr_beta_pair -> 
    if (beta_pairs_equal to_replace_beta_pair curr_beta_pair) then new_beta_pair else curr_beta_pair)

let same_rhs_sym_bbls_lists (curr_rhs_sym_bbls: symbol * (beta * beta) list) (rhs_sym_bbls: symbol * (beta * beta) list): bool = 
  let sym1, sym2 = (fst curr_rhs_sym_bbls), (fst rhs_sym_bbls) in 
  if (syms_equals sym1 sym2) then false 
  else true

let semi_compare_beta_pair_lists (bbls1: (beta * beta) list) (bbls2: (beta * beta) list): bool = 
  List.fold_left2 (fun acc beta_pair1 beta_pair2 -> 
    let curr_beta_pairs_comparison = 
      match beta_pair1, beta_pair2 with 
      | (T t1, T t1'), (T t2, T t2') -> t1 = t2 && t1' = t2'
      | (S _s1, S _s1'), (S _s2, S _s2') -> true
      | _, _ -> false 
    in 
      curr_beta_pairs_comparison && acc 
    ) true bbls1 bbls2

let are_semi_rhs_sym_bbls_lists (rhs_sym_bbls1: symbol * (beta * beta) list) (rhs_sym_bbls2: symbol * (beta * beta) list): bool = 
  let sym1, sym2 = (fst rhs_sym_bbls1), (fst rhs_sym_bbls2) in 
  if (syms_equals sym1 sym2) then 
    begin
      let bbls1, bbls2 = 
        (snd rhs_sym_bbls1), (snd rhs_sym_bbls2) 
      in
        (semi_compare_beta_pair_lists bbls1 bbls2)
    end
  else false

let replace_sts_beta_pair (beta_pair: beta * beta) 
  (from_sts_pair: state * state) (to_sts_pair: state * state): beta * beta  = 
  let from_sts1, from_sts2 = (fst from_sts_pair), (snd from_sts_pair) in 
  let to_sts1, to_sts2 = (fst to_sts_pair), (snd to_sts_pair) in
    match (fst beta_pair), (snd beta_pair) with 
    | S s1, S s2 -> if (s1 = to_sts1) && (s2 = to_sts2) then (S from_sts1), (S from_sts2) else (S s1), (S s2)
    | b1, b2 -> (b1, b2)

let replace_sym_bbls_to_sts_pair_with_from_sts_pair (curr_sym_bbls: symbol * (beta * beta) list) 
  (from_sts_pair: (state * state)) (to_sts_pair: (state * state)): symbol * (beta * beta) list = 
  let curr_beta_beta_ls: (beta * beta) list = (snd curr_sym_bbls) in
  let replaced_beta_beta_ls: (beta * beta) list = 
      curr_beta_beta_ls |> List.fold_left (fun acc beta_pair -> 
        let to_acc = 
          if (is_terminal_beta_pair beta_pair) then beta_pair
          else replace_sts_beta_pair beta_pair from_sts_pair to_sts_pair
        in to_acc :: acc
    ) [] |> List.rev
  in (fst curr_sym_bbls), replaced_beta_beta_ls

let replace_all_semi_dup_rhs_sym_bbls_ls
  (all_semi_dup_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list) 
  (from_sts_pair: (state * state)) (to_sts_pair: (state * state)): (symbol * (beta * beta) list) list = 
  all_semi_dup_rhs_sym_bbls_ls |> 
  List.fold_left (fun acc curr_sym_bbls -> 
    let to_acc = 
      if (List.mem curr_sym_bbls all_semi_dup_rhs_sym_bbls_ls)
      then (replace_sym_bbls_to_sts_pair_with_from_sts_pair curr_sym_bbls from_sts_pair to_sts_pair)
      else curr_sym_bbls 
    in to_acc :: acc) 
  [] |> List.rev

let now_replace_semi_dup_sym_bbls_ls (semi_dup_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list) 
  (from_sts_pair: (state * state)) (to_sts_pair: (state * state))
  (debug: bool): 
  (symbol * (beta * beta) list) list = 
  let replaced_all_semi_dups_from_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list =  
    replace_all_semi_dup_rhs_sym_bbls_ls semi_dup_rhs_sym_bbls_ls from_sts_pair to_sts_pair
  in
  (if debug then wrapped_printf debug "\n\t\t ->> After replacing Semi-Dup Sym_beta_pair_ls List : \n\t\t";
    if (List.is_empty replaced_all_semi_dups_from_rhs_sym_bbls_ls) then wrapped_printf debug "\n\t\t EMPTY \n\t\t" else
    replaced_all_semi_dups_from_rhs_sym_bbls_ls |> List.iter (fun (sym, bbls) -> Pp.pp_symbol sym; 
    bbls |> Pp.pp_beta_beta_list; wrapped_printf debug "\n\t\t"));
  replaced_all_semi_dups_from_rhs_sym_bbls_ls

let same_rhs_sym_bbls 
  (curr_rhs_sym_bbls: (symbol * (beta * beta) list)) 
  (rhs_sym_bbls: (symbol * (beta * beta) list)): bool = 
  let sym1, sym2 = (fst curr_rhs_sym_bbls), (fst rhs_sym_bbls) in 
  if (not (syms_equals sym1 sym2)) then false 
  else 
    begin 
      let bbls1, bbls2 = (snd curr_rhs_sym_bbls), (snd rhs_sym_bbls) in 
      List.fold_left2 (fun bool_acc bb1 bb2 -> 
        (beta_pairs_equal bb1 bb2) && bool_acc) true
      bbls1 bbls2
    end

(* Finding all_semi_dup rhs symb bbls lists first *)
let find_all_semi_dup_rhs_sym_bbls_ls (rhs_sym_bbls: symbol * (beta * beta) list) 
  (rest_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list) 
  (_debug: bool): 
  (symbol * (beta * beta) list) list = 
  let all_semi_dup_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list = 
    rest_rhs_sym_bbls_ls |> List.fold_left (fun acc curr_rhs_sym_bbls -> 
      if (are_semi_rhs_sym_bbls_lists rhs_sym_bbls curr_rhs_sym_bbls)
      then 
        if (List.mem curr_rhs_sym_bbls acc)
        then acc
        else curr_rhs_sym_bbls :: acc 
      else acc) []
  in 
  all_semi_dup_rhs_sym_bbls_ls 

let replace_sts_pair_trans_ls_per_sts_pairs (from_sts_pair: (state * state)) (to_sts_pair: (state * state)) 
  (sts_pair_trans_ls: ((state * state) * (symbol * (beta * beta) list)) list) (debug: bool):
  ((state * state) * (symbol * (beta * beta) list)) list = 
  let sts_pair: state * state = 
    if (List.is_empty sts_pair_trans_ls) then raise (Failure "In replacing sts_pair cannot be empty")
    else List.hd sts_pair_trans_ls |> fst
  in
  let curr_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list = 
    if (List.is_empty sts_pair_trans_ls) then raise (Failure "In replacing sym_beta_pair_ls cannot be empty")
    else sts_pair_trans_ls |> List.map snd
  in

  (* Find all (semi_dup_rhs_prod) list in 'curr tran_sls' *)
  (*   Note: (semi_dup_rhs_prod) list considered semi_dup if all but nonterminals are equal *)
  let all_semi_dup_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list = 
    curr_rhs_sym_bbls_ls |> List.fold_left (fun dups_acc curr_rhs_sym_bbls -> 
      
      (* Accumulate all the ones that are semi_dup -> check for each sym_bbls in the current trans_ls *)
      let rest_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list = 
        curr_rhs_sym_bbls_ls |> List.filter (fun x -> not (same_rhs_sym_bbls curr_rhs_sym_bbls x))
      in 
      let curr_all_semi_dup_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list = 
        find_all_semi_dup_rhs_sym_bbls_ls curr_rhs_sym_bbls rest_rhs_sym_bbls_ls debug
      in
        curr_all_semi_dup_rhs_sym_bbls_ls @ dups_acc
    ) [] |> clean_up_rhs_beta_pair_lsls
  in
  
  (* When there are no semi_dup_prod list then just return the trans_ls *)
  if (List.is_empty all_semi_dup_rhs_sym_bbls_ls) then sts_pair_trans_ls
  else
    ((* 
    if debug then wrapped_printf debug "\n\t\t >> Found all Semi-Dup Sym_beta_pair_ls List : \n\t\t";
    all_semi_dup_rhs_sym_bbls_ls |> List.iter (fun (sym, bbls) -> Pp.pp_symbol sym; 
    bbls |> Pp.pp_beta_beta_list; wrapped_printf debug "\n\t\t");
    *)

    (* Take out all the non-(semi_dup_rhs_prod) list in 'curr tran_sls' *)
    let non_semi_dup_rhs_sym_bbls_ls: (symbol * (beta * beta) list) list = 
      curr_rhs_sym_bbls_ls |> List.filter (fun rhs_sym_bbls -> 
        not (sym_bbls_exists_in_sym_bbls_ls rhs_sym_bbls all_semi_dup_rhs_sym_bbls_ls))
    in
    
    (* 
    if debug then wrapped_printf debug "\n\t\t >> _Non_ Semi-Dup Sym_beta_pair_ls List : \n\t\t";
    non_semi_dup_rhs_sym_bbls_ls |> List.iter (fun (sym, bbls) -> Pp.pp_symbol sym; 
    bbls |> Pp.pp_beta_beta_list; wrapped_printf debug "\n\t\t");      
    *)

    (* Replace 'to_sts_pair' with 'from_sts_pair' in '(semi_dup_rhs_prod) list' *)
    let replaced_dup_sym_bbls_ls: (symbol * (beta * beta) list) list = 
      now_replace_semi_dup_sym_bbls_ls all_semi_dup_rhs_sym_bbls_ls from_sts_pair to_sts_pair debug
    in 

    (* 
    if debug then wrapped_printf debug "\n\t\t >> Replaced Semi-Dup Sym_beta_pair_ls List : \n\t\t";
    non_semi_dup_rhs_sym_bbls_ls |> List.iter (fun (sym, bbls) -> Pp.pp_symbol sym; 
    bbls |> Pp.pp_beta_beta_list; wrapped_printf debug "\n\t\t");       
    *)

    (* Combine replaced '(semi_dup_rhs_prod) list' with  non-(semi_dup_rhs_prod) list in 'curr tran_sls' *)
    replaced_dup_sym_bbls_ls @ non_semi_dup_rhs_sym_bbls_ls
    |> List.map (fun x -> (sts_pair, x)))

let update_raw_trans_per_eps_linked_sts_pairs 
  (trans: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (from_sts_pair: state * state) (to_sts_pair: state * state) (debug: bool): 
  ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 

  wrapped_printf debug "\n\t\t * From States pair "; Pp.pp_raw_state from_sts_pair;  
  wrapped_printf debug "\n\t\t * To States pair "; Pp.pp_raw_state to_sts_pair; wrapped_printf debug "\n\n"; 
  let rec traverse_raw_trans_blocks 
    (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
    (acc_trans: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) = 
    match trans_blocks_ls with 
    | [] -> List.rev acc_trans
    | (sts_pair, sts_pair_trans_ls) :: trans_blocks_tl -> 
      let replaced_sts_pair_trans_ls: 
        ((state * state) * (symbol * (beta * beta) list)) list = 
        replace_sts_pair_trans_ls_per_sts_pairs from_sts_pair to_sts_pair sts_pair_trans_ls debug
      in
      let to_acc: (state * state) * ((state * state) * (symbol * (beta * beta) list)) list = 
        (sts_pair, replaced_sts_pair_trans_ls) 
      in 
        traverse_raw_trans_blocks trans_blocks_tl (to_acc :: acc_trans)
  in let res_raw_trans_blocks = traverse_raw_trans_blocks trans [] 
  in 
  (* if debug then (wrapped_printf debug "\n\t * Updated trans blocks \n"; res_raw_trans_blocks |> Pp.pp_raw_trans_blocks); *)
    res_raw_trans_blocks

let update_raw_trans_per_eps_stspair_to_stspairs_ls 
  (trans: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (from_sts_pair1: state * state) (eps_connected_sts_pairs_ls: (state * state) list) (debug: bool): 
  ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
  let res_raw_trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    eps_connected_sts_pairs_ls 
    |> List.rev 
    |> List.fold_left (fun acc_trans eps_linked_sts_pair2 -> 
      update_raw_trans_per_eps_linked_sts_pairs acc_trans from_sts_pair1 eps_linked_sts_pair2 debug) trans
  in 
    res_raw_trans_blocks

let update_trans_blocks_per_eps_linkage (raw_trans_blocks: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list) 
  (state_eps_connected_states: ((state * state) * ((state * state) list)) list) (debug: bool): 
  ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
  let new_raw_trans_blocks: 
    ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    state_eps_connected_states |> List.fold_left (fun acc_trans (sts_pair1, eps_connected_sts_pairs_ls) -> 
      update_raw_trans_per_eps_stspair_to_stspairs_ls acc_trans sts_pair1 eps_connected_sts_pairs_ls debug) raw_trans_blocks  
  in 
    new_raw_trans_blocks

(* --------------------------- End of Helpers ---------------------------------- *)


(** intersect_wo_opt1 - intersect without reachability analysis  *)
let intersect_wo_opt1 (a1: ta) (a2: ta) (debug: bool): ta =
  let wrapped_printf fmt = 
    if debug then Printf.printf fmt else Printf.ifprintf stdout fmt 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 0 - Find set of all possible states, ie, Q := Q_1 x Q_2 ------------------------------------ *)
  
  let states_raw: (state * state) list = 
    a1.states |> List.map (fun s1 -> a2.states |> List.map (fun s2 -> (s1, s2))) |> List.flatten
  in

  (pp_upline_new debug; wrapped_printf  "### Step 0 - Found the set of all possible states\n\t"; 
  states_raw |> Pp.pp_raw_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 1 - Get all possible raw transitions for 'states_raw' ----------------------------------------- *)

  let raw_init_trans_ls: (((state * state) * symbol) * (beta * beta) list) list = 
      cartesian_product_trans_from states_raw a1.transitions a2.transitions debug 
  in
  
  (pp_upline_new debug; wrapped_printf "### Step 1 - Find states_raw-starting transitions : \n\t"; 
  Pp.pp_raw_transitions raw_init_trans_ls; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 3 - Based on (Ei, Ej) in list of possible states, find transitions starting from (Ei, Ej) ----- *)
  
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
    collect_from_all_reachable_states states_raw states_raw [] in 
  let all_raw_trans = 
    raw_init_trans_ls @ raw_trans_from_all_reachables 
    |> Cfgutils.remove_dups
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 3 - Found all the raw trasitions from all possible states  : \n\t"; 
  Pp.pp_raw_transitions all_raw_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 4 - Write the 'all_raw_trans' in blocks and sort them for better comparison ------------------- *)
  let raw_trans_in_blocks_sorted: 
    ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list =
    
    (states_raw @ all_raw_states_reachable) |> List.fold_left (fun acc (s1, s2) -> 
      let block: ((state * state) * (symbol * (beta * beta) list)) list = 
        collect_existing_raw_trans_for_states_pair (s1, s2) all_raw_trans 
      in 
        acc @ [(s1, s2), block]) []
      |> List.stable_sort (fun (_, trans_ls1) (_, trans_ls2) -> 
      Int.compare (List.length trans_ls2) (List.length trans_ls1))
      |> Cfgutils.remove_dups
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 4 - Put raw transitions in blocks of transitions : \n\t";
    Pp.pp_raw_trans_blocks raw_trans_in_blocks_sorted; pp_loline_new debug);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 5 - Find and remove duplicate states ---------------------------------------------------------- *)

  (* Step 5.1 - Find a list of duplicate states pairs *)
  let dup_states_pair_ls: ((state * state) * (state * state)) list = 
    find_duplicate_state_pairs_in_trans_blocks raw_trans_in_blocks_sorted debug
  in
  let combined_dup_states_pair_ls: ((state * state) list) list =
    group_common_dup_state_pairs_ls state_pairs_equal dup_states_pair_ls
  in 
  (* wrapped_printf "\n\n\t\t State pair list grouped"; 
  combined_dup_states_pair_ls |> List.iter (fun st_pair_ls -> 
    wrapped_printf "\n\t\t[ "; st_pair_ls |> List.iter Pp.pp_raw_state; wrapped_printf " ]"); *)

  (if debug then pp_upline_new debug; wrapped_printf "##### 5.1 - Find a list of duplicate states pairs : \n";
  combined_dup_states_pair_ls |> List.iter (fun st_pair_ls -> wrapped_printf "\n\t\t[ "; 
  st_pair_ls |> List.iter Pp.pp_raw_state; wrapped_printf " ]\n" ); pp_loline_new debug);

  (* Step 5.2 - Remove transitions based on 'dup_states_pair_ls' *)  
  let raw_trans_blocks_cleaned: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    combined_dup_states_pair_ls 
    |> List.fold_left (fun trans_acc sts_pair_ls -> 

        let fst_sts_pair: (state * state) = 
          List.hd sts_pair_ls
        in
        let remain_sts_pair: (state * state) list = 
          List.tl sts_pair_ls 
        in 
        remain_sts_pair 
          |> List.fold_left (fun inner_inner_trans_acc curr_sts_pair -> 
            let curr_dup_sts_pair_ls: ((state * state) * (state * state)) list = 
              (fst_sts_pair, curr_sts_pair)::[] 
            in
              remove_transitions_of_duplicate_states curr_dup_sts_pair_ls inner_inner_trans_acc debug          
            ) trans_acc
            
    ) raw_trans_in_blocks_sorted
    
  in
  (if debug then pp_upline_new debug; wrapped_printf "##### Step 5.2 - Remove raw transitions wrt duplicate states pairs : \n";
  Pp.pp_raw_trans_blocks raw_trans_blocks_cleaned; pp_loline_new debug);

  (* Step 5.3 - Replace state names based on 'dup_states_pair_ls' *)
  let trans_blocks_replaced: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    replace_dup_state_names dup_states_pair_ls raw_trans_blocks_cleaned debug
  in
  (if debug then pp_upline_new debug; wrapped_printf "##### Step 5.3 - Replace state names wrt duplicate states pairs : \n";
    Pp.pp_raw_trans_blocks trans_blocks_replaced; pp_loline_new debug);

  (* Step 5.4 - Remove duplicate transitions in each raw trans block *)  
  let trans_blocks_wo_dup_trans = 
    remove_dup_trans_for_each_block trans_blocks_replaced debug 
  in 
  (if debug then pp_upline_new debug; wrapped_printf "##### Step 5.4 - Removed dup trans in raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_wo_dup_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 8 - Rename states and populate Q, raw trans blocks -------------------------------------------- *)  
  
  let states_renaming_map: ((state * state) * (state * state)) list =
    collect_unique_states_and_map_to_new_states trans_blocks_wo_dup_trans states_raw debug 
  in
  let state_pairs_renamed: (state * state) list = 
    states_renaming_map |> List.map snd 
  in 
  wrapped_printf "\n\t States renamed : "; state_pairs_renamed |> List.iter Pp.pp_raw_state; wrapped_printf "\n\n";

  let trans_blocks_renamed: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    rename_trans_blocks states_renaming_map trans_blocks_wo_dup_trans debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 8 - Rename states in Q and raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_renamed; pp_loline_new debug);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 10 - Introduce epsilon transitions to simplify raw trans blocks ------------------------------- *)
  
  let trans_blocks_simplified_eps_trans: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    simplify_trans_blocks_with_epsilon_transitions trans_blocks_renamed (List.rev state_pairs_renamed) debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 10 - Introduce epsilon transitions to simplify raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_eps_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 11 - Convert raw trans blocks to transitions list --------------------------------------------- *)
  
  let raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    trans_blocks_simplified_eps_trans |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, beta_beta_ls)) -> 
        let trans_new = ((st_pair, sym), beta_beta_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 11 - Convert raw trans blocks to transitions list : \n";
  Pp.pp_raw_transitions raw_trans; pp_loline_new debug);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 12 - Identify dead states and get rid of transistions involving dead states ------------------- *)
  
  let dead_states: (state * state) list = 
    trans_blocks_simplified_eps_trans |> List.fold_left (fun acc (sts_pair, raw_trans_ls) -> 
      if (List.is_empty raw_trans_ls) then sts_pair :: acc else acc) []  
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 12 - Identify dead states : \n";
  dead_states |> Pp.pp_raw_states; pp_loline_new debug);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 13 - Simplify raw transitions list wrt. dead states (any prods involving them)----------------- *)

  let cleaned_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states raw_trans dead_states debug
  in
  
  let cleaned_trans_blocks_wrt_dead_states: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    clean_raw_trans_blocks_wrt_dead_states trans_blocks_simplified_eps_trans dead_states debug 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 14 - Find epsilon-transition linkage between states ------------------------------------------- *)
  
  let state_eps_connected_states: ((state * state) * ((state * state) list)) list = 
    find_epsilon_transition_connection_in_states state_pairs_renamed cleaned_raw_trans debug
    |> List.sort (fun (_sts_pair1, sts_pair_ls1) (_sts_pair2, sts_pair_ls2) ->
      Int.compare (List.length sts_pair_ls1) (List.length sts_pair_ls2))
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 14 - Identify State -> epsilon-connected states (sorted) : \n";
    state_eps_connected_states |> List.iter (fun (sts_pair, sts_pair_ls) -> 
      wrapped_printf "\t\t State (%s, %s) is epsilon-connected to -> " (fst sts_pair) (snd sts_pair);
       sts_pair_ls |> Pp.pp_raw_states); pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 15 - Simplify based on epsilon-transition linkage between states ------------------------------ *)
  
  let trans_blocks_simplified_per_eps_linkage: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    update_trans_blocks_per_eps_linkage cleaned_trans_blocks_wrt_dead_states state_eps_connected_states debug
  in 

  (if debug then pp_upline_new debug; wrapped_printf "### Step 15 - Simplify raw trans blocks wrt. epsilon-connected states : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_per_eps_linkage; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 17 - Remove any duplicate transition in each block after simplification ----------------------- *)
  
  let trans_blocks_simplified_wo_dup_trans = 
    remove_dup_trans_for_each_block trans_blocks_simplified_per_eps_linkage debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 17 - Removed dup trans in raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_wo_dup_trans; pp_loline_new debug);

  let new_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    trans_blocks_simplified_wo_dup_trans |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, beta_beta_ls)) -> 
        let trans_new = ((st_pair, sym), beta_beta_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in
  let cleaned_raw_trans_wrt_dead_states: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states new_raw_trans dead_states debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 17.5 - Convert raw trans blocks to transitions list after removing any duplicates : \n";
  Pp.pp_raw_transitions cleaned_raw_trans_wrt_dead_states; pp_loline_new debug);

  let new_cleaned_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states new_raw_trans dead_states debug
  in 

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 18 - Convert the raw transitions blocks to transitions tbl ------------------------------------ *)

  (* Step 16.1 - Convert the raw transitions to transitions tbl *)
  let res_trans_ls: ((state * symbol) * beta list) list = 
    new_cleaned_raw_trans |> List.map (fun ((st_pair, sym), sig_sig_ls) -> 
      let new_sig_ls = sig_sig_ls |> List.map fst in ((fst st_pair), sym), new_sig_ls) 
  in 
  let res_trans_tbl: ((state * symbol), beta list) Hashtbl.t = Hashtbl.create (Hashtbl.length a2.transitions) in
    res_trans_ls |> List.iter (fun ((st, sym), beta_ls) -> 
      Hashtbl.add res_trans_tbl (st, sym) beta_ls);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 19 - Populate 'states' and 'final_states' according to renaming map --------------------------- *)
  
  (* Note! Might need later this rename_map in Formatter *)
  let _states_rename_map: (state * state) list = 
    states_renaming_map |> List.map (fun ((orig_st, _), (new_st, _)) -> (orig_st, new_st)) in 
 
  let res_states_final = 
    state_pairs_renamed |> List.filter (fun x -> not (List.mem x dead_states)) |> List.map fst in
  let corr_final_states: state list = 
    let final_states_orig: state list = 
      a1.final_states |> List.map (fun s1 -> a2.final_states |> List.map (fun s2 -> (s1, s2)))
      |> List.flatten |> List.map fst (* e.g., [program] *)
    in 
    states_renaming_map |> List.filter (fun ((orig, _internal), (_renamed, _)) -> List.mem orig final_states_orig) 
    |> List.map snd |> List.map fst
  in 

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 20 - Put everything together and return the resulted TA --------------------------------------- *)
  
  if debug then (wrapped_printf"\nIntersect the following 2 TAs:\n\n  (1) First TA:\n"; Pp.pp_ta a1; 
  wrapped_printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; wrapped_printf "\n\n");

  let res_ta: ta = 
    { 
      states = res_states_final @ corr_final_states; alphabet = a1.alphabet; 
      final_states = corr_final_states; terminals = a1.terminals; transitions = res_trans_tbl 
    }
  in
  wrapped_printf "\n ** Result of TA intersection: \n"; Pp.pp_ta res_ta; wrapped_printf "\n\n"; 
  res_ta


(** intersect_wo_opt2 - intersect without removal of duplicates *)
let intersect_wo_opt2 (a1: ta) (a2: ta) (debug: bool): ta =
  let wrapped_printf fmt = 
    if debug then Printf.printf fmt else Printf.ifprintf stdout fmt 
  in
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 0 - Find the set of final states, ie, Q_f := Q_f_1 x Q_f_2 ------------------------------------ *)
  
  let final_states_raw: (state * state) list = 
    a1.final_states |> List.map (fun s1 -> a2.final_states |> List.map (fun s2 -> (s1, s2))) |> List.flatten
  in
  (pp_upline_new debug; wrapped_printf  "### Step 0 - Found the set of final states\n\t"; 
  final_states_raw |> Pp.pp_raw_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 1 - Get raw transitions for symbols that 'final_states_raw' from Q_f -------------------------- *)

  let raw_init_trans_ls: (((state * state) * symbol) * (beta * beta) list) list = 
      cartesian_product_trans_from final_states_raw a1.transitions a2.transitions debug 
  in
  
  (pp_upline_new debug; wrapped_printf "### Step 1 - Find final states-starting transitions : \n\t"; 
  Pp.pp_raw_transitions raw_init_trans_ls; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 2 - Find reachable states based on final-states-starting transitions -------------------------- *)
  
  let init_reachable_states: (state * state) list = 
    find_reachable_states raw_init_trans_ls 
    (* Exclude the 'final_states_raw' from the 'init_reachable_states' *)
    |> List.filter (fun x -> (not (List.mem x final_states_raw)))
  in
  (if debug then pp_upline_new debug; 
  wrapped_printf "### Step 2 - Found reachable states based on intial states-starting transitions\n\t"; 
  Pp.pp_raw_states init_reachable_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 3 - Based on (Ei, Ej) in list of reachable states, find transitions starting from (Ei, Ej) ---- *)
  
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
  (if debug then pp_upline_new debug; wrapped_printf "### Step 3 - Found all the raw trasitions from all the reachable states  : \n\t"; 
  Pp.pp_raw_transitions all_raw_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 4 - Write the 'all_raw_trans' in blocks and sort them for better comparison ------------------- *)
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
  (if debug then pp_upline_new debug; wrapped_printf "### Step 4 - Put raw transitions in blocks of transitions : \n\t";
    Pp.pp_raw_trans_blocks raw_trans_in_blocks_sorted; pp_loline_new debug);
 
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 8 - Rename states and populate Q, raw trans blocks -------------------------------------------- *)  
  
  (* raw_trans_in_blocks_sorted: 
    ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list *)

  let states_renaming_map: ((state * state) * (state * state)) list =
    collect_unique_states_and_map_to_new_states raw_trans_in_blocks_sorted final_states_raw debug 
  in
  let state_pairs_renamed: (state * state) list = 
    states_renaming_map |> List.map snd 
  in 
  wrapped_printf "\n\t States renamed : "; state_pairs_renamed |> List.iter Pp.pp_raw_state; wrapped_printf "\n\n";

  let trans_blocks_renamed: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    rename_trans_blocks states_renaming_map raw_trans_in_blocks_sorted debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "\n\n##### SKIPPED REMOVAL OF DUPLICATES (Steps 5, 6, 7, 9) #####\n\n";
  wrapped_printf "### Step 8 - Rename states in Q and raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_renamed; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 10 - Introduce epsilon transitions to simplify raw trans blocks ------------------------------- *)
  
  let trans_blocks_simplified_eps_trans: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    simplify_trans_blocks_with_epsilon_transitions trans_blocks_renamed (List.rev state_pairs_renamed) debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 10 - Introduce epsilon transitions to simplify raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_eps_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 11 - Convert raw trans blocks to transitions list --------------------------------------------- *)
  
  let _raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    trans_blocks_simplified_eps_trans |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, beta_beta_ls)) -> 
        let trans_new = ((st_pair, sym), beta_beta_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 11 - Convert raw trans blocks to transitions list : \n";
  Pp.pp_raw_transitions _raw_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 12 - Identify dead states and get rid of transistions involving dead states ------------------- *)
  
  let dead_states: (state * state) list = 
    trans_blocks_simplified_eps_trans |> List.fold_left (fun acc (sts_pair, raw_trans_ls) -> 
      if (List.is_empty raw_trans_ls) then sts_pair :: acc else acc) []  
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 12 - Identify dead states : \n";
  dead_states |> Pp.pp_raw_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 13 - Simplify raw transitions list wrt. dead states (any prods involving them)----------------- *)

  let cleaned_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states _raw_trans dead_states debug
  in
  
  let cleaned_trans_blocks_wrt_dead_states: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    clean_raw_trans_blocks_wrt_dead_states trans_blocks_simplified_eps_trans dead_states debug 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 14 - Find epsilon-transition linkage between states ------------------------------------------- *)
  
  let state_eps_connected_states: ((state * state) * ((state * state) list)) list = 
    find_epsilon_transition_connection_in_states state_pairs_renamed cleaned_raw_trans debug
    |> List.sort (fun (_sts_pair1, sts_pair_ls1) (_sts_pair2, sts_pair_ls2) ->
      Int.compare (List.length sts_pair_ls1) (List.length sts_pair_ls2))
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 14 - Identify State -> epsilon-connected states (sorted) : \n";
    state_eps_connected_states |> List.iter (fun (sts_pair, sts_pair_ls) -> 
      wrapped_printf "\t\t State (%s, %s) is epsilon-connected to -> " (fst sts_pair) (snd sts_pair);
       sts_pair_ls |> Pp.pp_raw_states); pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 15 - Simplify based on epsilon-transition linkage between states ------------------------------ *)
  
  let trans_blocks_simplified_per_eps_linkage: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    update_trans_blocks_per_eps_linkage cleaned_trans_blocks_wrt_dead_states state_eps_connected_states debug
  in 

  (if debug then pp_upline_new debug; wrapped_printf "### Step 15 - Simplify raw trans blocks wrt. epsilon-connected states : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_per_eps_linkage; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 17 - Remove any duplicate transition in each block after simplification ----------------------- *)
  
  let trans_blocks_simplified_wo_dup_trans = 
    remove_dup_trans_for_each_block trans_blocks_simplified_per_eps_linkage debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 17 - Removed dup trans in raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_wo_dup_trans; pp_loline_new debug);

  let new_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    trans_blocks_simplified_wo_dup_trans |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, beta_beta_ls)) -> 
        let trans_new = ((st_pair, sym), beta_beta_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in
  let cleaned_raw_trans_wrt_dead_states: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states new_raw_trans dead_states debug
  in 

  (if debug then pp_upline_new debug; wrapped_printf "### Step 17.5 - Convert raw trans blocks to transitions list after removing any duplicates : \n";
  Pp.pp_raw_transitions cleaned_raw_trans_wrt_dead_states; pp_loline_new debug);

  let new_cleaned_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states new_raw_trans dead_states debug
  in 

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 18 - Convert the raw transitions to transitions tbl ------------------------------------------- *)

  let res_trans_ls: ((state * symbol) * beta list) list = 
    new_cleaned_raw_trans |> List.map (fun ((st_pair, sym), sig_sig_ls) -> 
      let new_sig_ls = sig_sig_ls |> List.map fst in ((fst st_pair), sym), new_sig_ls) 
  in 
  let res_trans_tbl: ((state * symbol), beta list) Hashtbl.t = Hashtbl.create (Hashtbl.length a2.transitions) in
    res_trans_ls |> List.iter (fun ((st, sym), beta_ls) -> 
      Hashtbl.add res_trans_tbl (st, sym) beta_ls);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 19 - Populate 'states' and 'final_states' according to renaming map --------------------------- *)
  
  (* Note! Might need later this rename_map in Formatter *)
  let _states_rename_map: (state * state) list = 
    states_renaming_map |> List.map (fun ((orig_st, _), (new_st, _)) -> (orig_st, new_st)) in 
 
  let res_states_final = 
    state_pairs_renamed |> List.filter (fun x -> not (List.mem x dead_states)) |> List.map fst in
  let corr_final_states: state list = 
    let final_states_orig: state list = 
      final_states_raw |> List.map fst (* e.g., [program] *)
    in 
    states_renaming_map |> List.filter (fun ((orig, _internal), (_renamed, _)) -> List.mem orig final_states_orig) 
    |> List.map snd |> List.map fst
  in 

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 20 - Put everything together and return the resulted TA --------------------------------------- *)
  
  if debug then (wrapped_printf"\nIntersect the following 2 TAs:\n\n  (1) First TA:\n"; Pp.pp_ta a1; 
  wrapped_printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; wrapped_printf "\n\n");

  let res_ta: ta = 
    { states = res_states_final ; alphabet = a1.alphabet ; final_states = corr_final_states; terminals = a1.terminals;
    transitions = res_trans_tbl } 
  in
  wrapped_printf "\n ** Result of TA intersection: \n"; Pp.pp_ta res_ta; wrapped_printf "\n\n"; 
  res_ta


(** intersect_wo_opt3 - intersect without epsilon introduction *)
let intersect_wo_opt3 (a1: ta) (a2: ta) (debug: bool): ta =
  let wrapped_printf fmt = 
    if debug then Printf.printf fmt else Printf.ifprintf stdout fmt 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 0 - Find the set of final states, ie, Q_f := Q_f_1 x Q_f_2 ------------------------------------ *)
  
  let final_states_raw: (state * state) list = 
    a1.final_states |> List.map (fun s1 -> a2.final_states |> List.map (fun s2 -> (s1, s2))) |> List.flatten
  in

  (pp_upline_new debug; wrapped_printf  "### Step 0 - Found the set of final states\n\t"; 
  final_states_raw |> Pp.pp_raw_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 1 - Get raw transitions for symbols that 'final_states_raw' from Q_f -------------------------- *)

  let raw_init_trans_ls: (((state * state) * symbol) * (beta * beta) list) list = 
      cartesian_product_trans_from final_states_raw a1.transitions a2.transitions debug 
  in
  
  (pp_upline_new debug; wrapped_printf "### Step 1 - Find final states-starting transitions : \n\t"; 
  Pp.pp_raw_transitions raw_init_trans_ls; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 2 - Find reachable states based on final-states-starting transitions -------------------------- *)
  
  let init_reachable_states: (state * state) list = 
    find_reachable_states raw_init_trans_ls 
    (* Exclude the 'final_states_raw' from the 'init_reachable_states' *)
    |> List.filter (fun x -> (not (List.mem x final_states_raw)))
  in
  (if debug then pp_upline_new debug; 
  wrapped_printf "### Step 2 - Found reachable states based on intial states-starting transitions\n\t"; 
  Pp.pp_raw_states init_reachable_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 3 - Based on (Ei, Ej) in list of reachable states, find transitions starting from (Ei, Ej) ---- *)
  
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
  (if debug then pp_upline_new debug; wrapped_printf "### Step 3 - Found all the raw trasitions from all the reachable states  : \n\t"; 
  Pp.pp_raw_transitions all_raw_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 4 - Write the 'all_raw_trans' in blocks and sort them for better comparison ------------------- *)
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
  (if debug then pp_upline_new debug; wrapped_printf "### Step 4 - Put raw transitions in blocks of transitions : \n\t";
    Pp.pp_raw_trans_blocks raw_trans_in_blocks_sorted; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 5 - Find a list of duplicate states pairs ----------------------------------------------------- *)
  
  let dup_states_pair_ls: ((state * state) * (state * state)) list = 
    find_duplicate_state_pairs_in_trans_blocks raw_trans_in_blocks_sorted debug
  in
  let combined_dup_states_pair_ls: ((state * state) list) list =
    group_common_dup_state_pairs_ls state_pairs_equal dup_states_pair_ls 
  in 
  (* wrapped_printf "\n\n\t\t State pair list grouped"; 
  combined_dup_states_pair_ls |> List.iter (fun st_pair_ls -> 
    wrapped_printf "\n\t\t[ "; st_pair_ls |> List.iter Pp.pp_raw_state; wrapped_printf " ]"); *)

  (if debug then pp_upline_new debug; wrapped_printf "### Step 5 - Find a list of duplicate states pairs : \n";
  combined_dup_states_pair_ls |> List.iter (fun st_pair_ls -> wrapped_printf "\n\t\t[ "; 
  st_pair_ls |> List.iter Pp.pp_raw_state; wrapped_printf " ]" ); pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 6 - Remove transitions based on 'dup_states_pair_ls' ------------------------------------------ *)  
  let raw_trans_blocks_cleaned: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    
    combined_dup_states_pair_ls 
    |> List.fold_left (fun trans_acc sts_pair_ls -> 

        let fst_sts_pair: (state * state) = 
          List.hd sts_pair_ls
        in
        let remain_sts_pair: (state * state) list = 
          List.tl sts_pair_ls 
        in 
        remain_sts_pair 
          |> List.fold_left (fun inner_inner_trans_acc curr_sts_pair -> 
            let curr_dup_sts_pair_ls: ((state * state) * (state * state)) list = 
              (fst_sts_pair, curr_sts_pair)::[] 
            in
              remove_transitions_of_duplicate_states curr_dup_sts_pair_ls inner_inner_trans_acc debug          
            ) trans_acc
            
    ) raw_trans_in_blocks_sorted
    
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 6 - Remove raw transitions wrt duplicate states pairs : \n";
  Pp.pp_raw_trans_blocks raw_trans_blocks_cleaned; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 7 - Replace state names based on 'dup_states_pair_ls' ----------------------------------------- *)  
  
  let trans_blocks_replaced: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    replace_dup_state_names dup_states_pair_ls raw_trans_blocks_cleaned debug
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 7 - Replace state names wrt duplicate states pairs : \n";
    Pp.pp_raw_trans_blocks trans_blocks_replaced; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 8 - Rename states and populate Q, raw trans blocks -------------------------------------------- *)  
  
  let states_renaming_map: ((state * state) * (state * state)) list =
    collect_unique_states_and_map_to_new_states trans_blocks_replaced final_states_raw debug 
  in
  let state_pairs_renamed: (state * state) list = 
    states_renaming_map |> List.map snd 
  in 
  wrapped_printf "\n\t States renamed : "; state_pairs_renamed |> List.iter Pp.pp_raw_state; wrapped_printf "\n\n";

  let trans_blocks_renamed: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    rename_trans_blocks states_renaming_map trans_blocks_replaced debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 8 - Rename states in Q and raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_renamed; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 9 - Remove duplicate transitions in each raw trans block -------------------------------------- *)  
  
  let trans_blocks_wo_dup_trans = 
    remove_dup_trans_for_each_block trans_blocks_renamed debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 9 - Removed dup trans in raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_wo_dup_trans; pp_loline_new debug);

  let new_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    (* trans_blocks_simplified_wo_dup_trans *)
    trans_blocks_wo_dup_trans |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, beta_beta_ls)) -> 
        let trans_new = ((st_pair, sym), beta_beta_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 12 - Identify dead states and get rid of transistions involving dead states ------------------- *)
  
  let dead_states: (state * state) list = 
    trans_blocks_wo_dup_trans |> List.fold_left (fun acc (sts_pair, raw_trans_ls) -> 
      if (List.is_empty raw_trans_ls) then sts_pair :: acc else acc) []  
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 12 - Identify dead states : \n";
  dead_states |> Pp.pp_raw_states; pp_loline_new debug);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 13 - Simplify raw transitions list wrt. dead states (any prods involving them)----------------- *)

  let _cleaned_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states new_raw_trans dead_states debug
  in
  
  let cleaned_trans_blocks_wrt_dead_states: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    clean_raw_trans_blocks_wrt_dead_states trans_blocks_wo_dup_trans dead_states debug 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 17 - Remove any duplicate transition in each block after simplification ----------------------- *)
  
  let trans_blocks_simplified_wo_dup_trans = 
    remove_dup_trans_for_each_block cleaned_trans_blocks_wrt_dead_states debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 17 - Removed dup trans in raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_simplified_wo_dup_trans; pp_loline_new debug);

  let new_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    trans_blocks_simplified_wo_dup_trans |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, beta_beta_ls)) -> 
        let trans_new = ((st_pair, sym), beta_beta_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in
  let cleaned_raw_trans_wrt_dead_states: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states new_raw_trans dead_states debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 17.5 - Convert raw trans blocks to transitions list after removing any duplicates : \n";
  Pp.pp_raw_transitions cleaned_raw_trans_wrt_dead_states; pp_loline_new debug);

  let new_cleaned_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states new_raw_trans dead_states debug
  in 

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 18 - Convert the raw transitions to transitions tbl ------------------------------------------- *)

  let res_trans_ls: ((state * symbol) * beta list) list = 
    (* new_cleaned_raw_trans *)
    new_cleaned_raw_trans |> List.map (fun ((st_pair, sym), sig_sig_ls) -> 
      let new_sig_ls = sig_sig_ls |> List.map fst in ((fst st_pair), sym), new_sig_ls) 
  in 
  let res_trans_tbl: ((state * symbol), beta list) Hashtbl.t = Hashtbl.create (Hashtbl.length a2.transitions) in
    res_trans_ls |> List.iter (fun ((st, sym), beta_ls) -> 
      Hashtbl.add res_trans_tbl (st, sym) beta_ls);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 19 - Populate 'states' and 'final_states' according to renaming map --------------------------- *)
  
  (* Note! Might need later this rename_map in Formatter *)
  let _states_rename_map: (state * state) list = 
    states_renaming_map |> List.map (fun ((orig_st, _), (new_st, _)) -> (orig_st, new_st)) in 
 
  let res_states_final = 
    state_pairs_renamed |> List.map fst in
  let corr_final_states: state list = 
    let final_states_orig: state list = 
      final_states_raw |> List.map fst (* e.g., [program] *)
    in 
    states_renaming_map |> List.filter (fun ((orig, _internal), (_renamed, _)) -> List.mem orig final_states_orig) 
    |> List.map snd |> List.map fst
  in 

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 20 - Put everything together and return the resulted TA --------------------------------------- *)
  
  if debug then (wrapped_printf"\nIntersect the following 2 TAs:\n\n  (1) First TA:\n"; Pp.pp_ta a1; 
  wrapped_printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; wrapped_printf "\n\n");

  let res_ta: ta = 
    { states = res_states_final ; alphabet = a1.alphabet ; final_states = corr_final_states; terminals = a1.terminals;
    transitions = res_trans_tbl } 
  in
  wrapped_printf "\n ** Result of TA intersection: \n"; Pp.pp_ta res_ta; wrapped_printf "\n\n"; 
  res_ta


(** intersect_wo_opt123 - intersect without any optimizations *)
let intersect_wo_opt123 (a1: ta) (a2: ta) (debug: bool): ta =
  let wrapped_printf fmt = 
    if debug then Printf.printf fmt else Printf.ifprintf stdout fmt 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 0 - Find set of all possible states, ie, Q := Q_1 x Q_2 ------------------------------------ *)
  
  let states_raw: (state * state) list = 
    a1.states |> List.map (fun s1 -> a2.states |> List.map (fun s2 -> (s1, s2))) |> List.flatten
  in

  (pp_upline_new debug; wrapped_printf  "### Step 0 - Found the set of all possible states\n\t"; 
  states_raw |> Pp.pp_raw_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 1 - Get all possible raw transitions for 'states_raw' ----------------------------------------- *)

  let raw_init_trans_ls: (((state * state) * symbol) * (beta * beta) list) list = 
      cartesian_product_trans_from states_raw a1.transitions a2.transitions debug 
  in
  
  (pp_upline_new debug; wrapped_printf "### Step 1 - Find states_raw-starting transitions : \n\t"; 
  Pp.pp_raw_transitions raw_init_trans_ls; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 3 - Based on (Ei, Ej) in list of possible states, find transitions starting from (Ei, Ej) ----- *)
  
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
    collect_from_all_reachable_states states_raw states_raw [] in 
  let all_raw_trans = 
    raw_init_trans_ls @ raw_trans_from_all_reachables 
    |> Cfgutils.remove_dups
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 3 - Found all the raw trasitions from all possible states  : \n\t"; 
  Pp.pp_raw_transitions all_raw_trans; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 4 - Write the 'all_raw_trans' in blocks and sort them for better comparison ------------------- *)
  let raw_trans_in_blocks_sorted: 
    ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list =
    
    (states_raw @ all_raw_states_reachable) |> List.fold_left (fun acc (s1, s2) -> 
      let block: ((state * state) * (symbol * (beta * beta) list)) list = 
        collect_existing_raw_trans_for_states_pair (s1, s2) all_raw_trans 
      in 
        acc @ [(s1, s2), block]) []
      |> List.stable_sort (fun (_, trans_ls1) (_, trans_ls2) -> 
      Int.compare (List.length trans_ls2) (List.length trans_ls1))
      |> Cfgutils.remove_dups
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 4 - Put raw transitions in blocks of transitions : \n\t";
    Pp.pp_raw_trans_blocks raw_trans_in_blocks_sorted; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 8 - Rename states and populate Q, raw trans blocks -------------------------------------------- *)  
  
  let states_renaming_map: ((state * state) * (state * state)) list =
    collect_unique_states_and_map_to_new_states raw_trans_in_blocks_sorted states_raw debug 
  in
  let state_pairs_renamed: (state * state) list = 
    states_renaming_map |> List.map snd 
  in 
  wrapped_printf "\n\t States renamed : "; state_pairs_renamed |> List.iter Pp.pp_raw_state; wrapped_printf "\n\n";

  let trans_blocks_renamed: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    rename_trans_blocks states_renaming_map raw_trans_in_blocks_sorted debug
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 8 - Rename states in Q and raw trans blocks : \n";
  Pp.pp_raw_trans_blocks trans_blocks_renamed; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 11 - Convert raw trans blocks to transitions list --------------------------------------------- *)
  
  let raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    trans_blocks_renamed |> List.fold_left (fun res_acc (_, trans_block) -> 
      let transformed = trans_block |> List.fold_left (fun acc (st_pair, (sym, beta_beta_ls)) -> 
        let trans_new = ((st_pair, sym), beta_beta_ls) in trans_new :: acc) []
      in transformed @ res_acc) []
  in
  (if debug then pp_upline_new debug; wrapped_printf "### Step 11 - Convert raw trans blocks to transitions list : \n";
  Pp.pp_raw_transitions raw_trans; pp_loline_new debug);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 12 - Identify dead states and get rid of transistions involving dead states ------------------- *)
  
  let dead_states: (state * state) list = 
    trans_blocks_renamed |> List.fold_left (fun acc (sts_pair, raw_trans_ls) -> 
      if (List.is_empty raw_trans_ls) then sts_pair :: acc else acc) []  
  in 
  (if debug then pp_upline_new debug; wrapped_printf "### Step 12 - Identify dead states : \n";
  dead_states |> Pp.pp_raw_states; pp_loline_new debug);

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 13 - Simplify raw transitions list wrt. dead states (any prods involving them)----------------- *)

  let cleaned_raw_trans: (((state * state) * symbol) * (beta * beta) list) list = 
    clean_raw_transitions_wrt_dead_states raw_trans dead_states debug
  in
  
  let _cleaned_trans_blocks_wrt_dead_states: ((state * state) * ((state * state) * (symbol * (beta * beta) list)) list) list = 
    clean_raw_trans_blocks_wrt_dead_states trans_blocks_renamed dead_states debug 
  in

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 18 - Convert the raw transitions to transitions tbl ------------------------------------------- *)

  let res_trans_ls: ((state * symbol) * beta list) list = 
    cleaned_raw_trans |> List.map (fun ((st_pair, sym), sig_sig_ls) -> 
      let new_sig_ls = sig_sig_ls |> List.map fst in ((fst st_pair), sym), new_sig_ls) 
  in 
  let res_trans_tbl: ((state * symbol), beta list) Hashtbl.t = Hashtbl.create (Hashtbl.length a2.transitions) in
    res_trans_ls |> List.iter (fun ((st, sym), beta_ls) -> 
      Hashtbl.add res_trans_tbl (st, sym) beta_ls);
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 19 - Populate 'states' and 'final_states' according to renaming map --------------------------- *)
  
  (* Note! Might need later this rename_map in Formatter *)
  let _states_rename_map: (state * state) list = 
    states_renaming_map |> List.map (fun ((orig_st, _), (new_st, _)) -> (orig_st, new_st)) in 
 
  let res_states_final = 
    state_pairs_renamed |> List.filter (fun x -> not (List.mem x dead_states)) |> List.map fst in
  let corr_final_states: state list = 
    let final_states_orig: state list = 
      a1.final_states |> List.map (fun s1 -> a2.final_states |> List.map (fun s2 -> (s1, s2)))
      |> List.flatten |> List.map fst (* e.g., [program] *)
    in 
    states_renaming_map |> List.filter (fun ((orig, _internal), (_renamed, _)) -> List.mem orig final_states_orig) 
    |> List.map snd |> List.map fst
  in 
  
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Step 20 - Put everything together and return the resulted TA --------------------------------------- *)
  
  if debug then (wrapped_printf"\nIntersect the following 2 TAs:\n\n  (1) First TA:\n"; Pp.pp_ta a1; 
  wrapped_printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; wrapped_printf "\n\n");

  let res_ta: ta = 
    { 
      states = res_states_final @ corr_final_states; alphabet = a1.alphabet; 
      final_states = corr_final_states; terminals = a1.terminals; transitions = res_trans_tbl 
    } 
  in
  wrapped_printf "\n ** Result of TA intersection: \n"; Pp.pp_ta res_ta; wrapped_printf "\n\n"; 
  res_ta

  