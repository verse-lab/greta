open Ta
open Treeutils

let cartesian_product_trans_from (starting_states: state * state) (trans1: transition list) (trans2: transition list) 
  (a: symbol list) (debug: bool): ((state * state) * (symbol * (state * state) list)) list = 
  let st1, st2 = (fst starting_states), (snd starting_states) in
  let rec traverse_alphabet ls (acc: ((state * state) * (symbol * (state * state) list)) list): 
    ((state * state) * (symbol * (state * state) list)) list = 
    match ls with [] -> acc
    | hd_sym :: tl -> 
      let rht_sts1: state list = find_rhs_states_from_state_with_sym st1 hd_sym trans1 debug in
      let rht_sts2: state list = find_rhs_states_from_state_with_sym st2 hd_sym trans2 debug in
      let new_rht_sts: (state * state) list = cross_product_raw_state_lists rht_sts1 rht_sts2 in
      let new_trans: (state * state) * (symbol * (state * state) list) = 
        ((st1, st2), (hd_sym, new_rht_sts)) in 
      traverse_alphabet tl (new_trans::acc)
  in let res_st1_st2_trans = traverse_alphabet a [] in 
  if debug then (Printf.printf "\nResult of %s-starting transitions X %s-starting transitions : \n" st1 st2; 
    Pp.pp_raw_transitions res_st1_st2_trans); res_st1_st2_trans

let find_reachable_states (from_st: state * state) (trans_ls: ((state * state) * (symbol * (state * state) list)) list) 
  (debug: bool): (state * state) list = 
  let rec find_loop ls (acc: (state * state) list) = 
    match ls with [] -> List.rev acc
    | ((_, _), (_, rhs_sts12_ls)) :: tl -> 
      let new_sts = rhs_sts12_ls |> List.fold_left (fun acc_ls (rhs_st1, rhs_st2) -> 
        if ((state_pairs_list_mem (rhs_st1, rhs_st2) acc) || (state_pairs_list_mem (rhs_st1, rhs_st2) acc_ls) 
            || (state_pairs_equal from_st (rhs_st1, rhs_st2) || (state_pairs_equal (epsilon_state, epsilon_state) (rhs_st1, rhs_st2))))
        then acc_ls else (rhs_st1, rhs_st2)::acc_ls) [] 
      in find_loop tl (new_sts @ acc)
  in let res: (state * state) list = find_loop trans_ls [] in
  (if debug then Printf.printf "\n  >> Reachable states : "; Pp.pp_raw_states res); res

let accessible_symbols_for_state (init_st: state) (trans_ls: transition list) (debug: bool): symbol list =
  let rec interm_sts_loop (ls': state list) (syms_ls: symbol list): symbol list * state list = 
    match ls' with 
    | [] -> (remove_dup_symbols syms_ls), [] 
    | interm_hd :: interm_tl -> 
      Printf.printf "\n\t -->> Now looking for symbols starting from %s \n" interm_hd;
      let i_syms, next_interm_ls = find_accessible_symbols trans_ls interm_hd [] syms_ls
      in if (next_interm_ls = []) 
         then interm_sts_loop interm_tl (i_syms@syms_ls) 
         else interm_sts_loop next_interm_ls (i_syms@syms_ls)
  and 
  find_accessible_symbols (ls: transition list) (from_st: state) (interm_sts: state list) (syms_acc: symbol list): 
    symbol list * state list =
    match ls with 
    | [] ->
      if interm_sts = []
      then syms_acc, []
      else (Printf.printf "\n\tInterm states not empty: "; Pp.pp_states interm_sts; 
           interm_sts_loop interm_sts syms_acc)
    | (lft_st, (sym, rhs_sts)) :: tl -> 
      if (lft_st = from_st) && (not (syms_equals sym epsilon_symb))
      then find_accessible_symbols tl from_st interm_sts (sym::syms_acc)
      else if (lft_st = from_st) && (syms_equals sym epsilon_symb)
      then (let next_st = (List.hd rhs_sts) (* if eps-trans, save in 'interm_sts' and traverse til the end *)
            in find_accessible_symbols tl from_st (next_st::interm_sts) syms_acc)
      else find_accessible_symbols tl from_st interm_sts syms_acc
  in let syms_res, _ = find_accessible_symbols trans_ls init_st [] [] in 
  (if debug then Printf.printf "\n\t -->> .. Found Symbols: "; syms_res |> List.iter Pp.pp_symbol);
  syms_res

let find_transitions_from_state_pairs (st_pair: state * state) (trans_ls1: transition list) (trans_ls2: transition list) (debug: bool): 
  ((state * state) * (symbol * (state * state) list)) list =
  let st1, st2 = (fst st_pair), (snd st_pair) in 
  let alph1: symbol list = accessible_symbols_for_state st1 trans_ls1 debug in 
  let alph2: symbol list = accessible_symbols_for_state st2 trans_ls2 debug in 
  let alph_new: symbol list = take_smaller_symbols_list alph1 alph2 debug in 
  cartesian_product_trans_from st_pair trans_ls1 trans_ls2 alph_new debug 

(* helper for 'find_duplicate_state_pairs_in_trans_blocks' *)
let find_state_pair_w_same_raw_rhs (sym_rhs_raw_states: (symbol * (state * state) list) list) 
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list): (state * state) list = 
  let rec loop ls acc = 
    match ls with [] -> List.rev acc 
    | (st_pair', raw_trans_ls') :: tl -> 
      let curr_sym_and_rhs_state_pairs = sym_and_rhs_state_pairs raw_trans_ls' in
      if (same_sym_and_rhs_state_pairs curr_sym_and_rhs_state_pairs sym_rhs_raw_states)
      then loop tl (st_pair'::acc)
      else loop tl acc
  in loop trans_blocks_ls []

let find_duplicate_state_pairs_in_trans_blocks 
  (trans_blocks_ls: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list) 
  (debug: bool): ((state * state) * (state * state)) list =
  let rec traverse_trans_blocks (ls: ((state * state) * (((state * state) * (symbol * (state * state) list))) list) list) 
    (acc: ((state * state) * (state * state)) list) =
    match ls with [] -> acc 
    | (st_pair, raw_trans_ls) :: tl -> 
      let sym_and_rhs_raw_states = raw_trans_ls |> sym_and_rhs_state_pairs in
      let same_rhs_states_ls: (state * state) list = find_state_pair_w_same_raw_rhs sym_and_rhs_raw_states tl in
      let state_pairs_to_acc = same_rhs_states_ls |> List.map (fun st_pair' -> (st_pair', st_pair)) in
      traverse_trans_blocks tl (acc @ state_pairs_to_acc)
  in let res_state_pairs = traverse_trans_blocks trans_blocks_ls [] in 
  if debug then (Printf.printf "\n\t >> Duplicate states pairs : "; 
    res_state_pairs |> List.iter (fun pair_of_stats_pair ->  Pp.pp_raw_pair_of_state_pairs pair_of_stats_pair)); 
  res_state_pairs

let remove_transitions_of_duplicate_states (dup_states_ls: ((state * state) * (state * state)) list) 
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list) 
  (debug: bool): ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list =
  let trans_blocks = ref trans_blocks in
    dup_states_ls |> List.iter (fun (_, sts_pair2) -> 
      trans_blocks := !trans_blocks |> List.filter (fun ((st_pair, _ (* block *)): 
        (state * state) * ((state * state) * (symbol * (state * state) list)) list) -> 
        (* if there is sts_pair1 in trans_blocks, then remove sts_pair2's block *)
        not (state_pairs_equal st_pair sts_pair2))); 
  if debug then (Printf.printf "\n\t >> Results of removing : \n"; !trans_blocks |> Pp.pp_raw_trans_blocks);
  !trans_blocks

let replace_dups_in_block (dup_states_ls: ((state * state) * (state * state)) list) 
  (trans_block: ((state * state) * (symbol * (state * state) list)) list): 
  ((state * state) * (symbol * (state * state) list)) list =
  let rec replace_dup_pair_loop ((dup_sts_pair1, dup_sts_pair2): (state * state) * (state * state)) ls acc = 
    match ls with [] -> List.rev acc
    | (st_pair, (sym, rhs_st_pair_ls)) :: tl -> 
      let new_rhs_st_pair_ls: (state * state) list = rhs_st_pair_ls |> 
        List.map (fun st_pair -> if (state_pairs_equal st_pair dup_sts_pair2) then dup_sts_pair1 else st_pair) in 
      let new_raw_trans: (state * state) * (symbol * (state * state) list) = (st_pair, (sym, new_rhs_st_pair_ls)) in 
      replace_dup_pair_loop (dup_sts_pair1, dup_sts_pair2) tl (new_raw_trans::acc)
  and 
  traverse_dups dups_ls trans_blck_acc =
    match dups_ls with [] -> trans_blck_acc 
    | (dup_st1, dup_st2) :: dup_tl -> 
      let new_trans_block = replace_dup_pair_loop (dup_st1, dup_st2) trans_blck_acc []
      in traverse_dups dup_tl new_trans_block
  in traverse_dups dup_states_ls trans_block

let replace_dup_state_names (dup_states_ls: ((state * state) * (state * state)) list) 
(trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list) 
(debug: bool): ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list =
  let rec traverse_blocks ls acc =
    match ls with [] -> List.rev acc
    | (st_pr1, trans_lst_block) :: tl ->
      let replaced_dups_block: ((state * state) * (symbol * (state * state) list)) list = 
        replace_dups_in_block dup_states_ls trans_lst_block in 
      traverse_blocks tl ((st_pr1, replaced_dups_block)::acc)
  in let res_trans_blocks = traverse_blocks trans_blocks [] in 
  if debug then (Printf.printf "\n\t >> Results of replacing : \n"; res_trans_blocks |> Pp.pp_raw_trans_blocks);
  res_trans_blocks

let collect_unique_states_and_map_to_new_states 
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list) 
  (debug: bool): ((state * state) * (state * state)) list =
  let unique_states_ls: (state * state) list = 
    trans_blocks |> List.map (fun (st_pair, _) -> st_pair) in
  let rec map_loop ls cnt cond_cnt acc: ((state * state) * (state * state)) list =
    match ls with [] -> List.rev acc 
    | st_pair_hd :: tl -> 
      if (is_cond_state (fst st_pair_hd)) || (is_cond_state (snd st_pair_hd)) 
      then (let mapped_pair: (state * state) = "C" ^ (string_of_int cond_cnt), "" in 
            let to_acc: (state * state) * (state * state) = (st_pair_hd, mapped_pair)
            in map_loop tl cnt (cond_cnt+1) (to_acc::acc)) 
      else (let mapped_pair: (state * state) = "X" ^ (string_of_int cnt), "" in 
            let to_acc: (state * state) * (state * state) = (st_pair_hd, mapped_pair)
            in map_loop tl (cnt+1) cond_cnt (to_acc::acc))
  in let res_map = map_loop unique_states_ls 1 1 [] in 
  if debug then (Printf.printf "\n\t >> Results of unique states to new states mapping : \n\t";
  res_map |> List.iter Pp.pp_raw_pair_of_state_pairs; Printf.printf "\n");
  res_map

(** Intersection of tree automata *)
let intersect (a1: ta) (a2: ta) (verSyms: (string * int list) list) (debug_print: bool): ta =
  let open Printf in
  printf "\nIntersect the following 2 TAs:\n\n  (1) First TA:\n";
  Pp.pp_ta a1; printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; printf "\n";
  if debug_print then (printf "\n  >> Versatile symbol list: [ "; 
  verSyms |> List.map fst |> List.iter (fun x -> printf "%s " x); printf "]\n");
  (* Consider symbols excluding epsilon or Boolean for I *)
  let syms = a1.alphabet in (* TODO: Add a sanity check on alphabet based on set equality *)
  let syms_wo_epsilon = syms |> List.filter (fun s -> not (syms_equals s epsilon_symb)) in
  let syms_wo_epsilon_or_bool = syms_wo_epsilon |> List.filter (fun s -> not (syms_equals s ("B", 0))) 
  in
  (* Find I := I_1 x I_2 first w/o epsilon or bool *)
  let start_states: (state * state) = (a1.start_state, a2.start_state) 
  in
  (* Get transitions that start from I *)
  let raw_init_trans_ls: ((state * state) * (symbol * (state * state) list)) list = 
    (if debug_print then printf "\n*** Find initial states-starting transitions : \n");
    cartesian_product_trans_from start_states a1.transitions a2.transitions syms_wo_epsilon_or_bool debug_print 
  in
  (* Find reachable states based on I-starting transitions *)
  let reachable_states: (state * state) list = 
    (if debug_print then printf "\n*** Find reachable states based on initial states-starting transitions : \n");
    find_reachable_states start_states raw_init_trans_ls debug_print 
  in
  (* Based on (Ei, Ej) in list of reachable states, find transitions starting from (Ei, Ej) *)
  let raw_trans_from_reachables: ((state * state) * (symbol * (state * state) list)) list = 
    (if debug_print then printf "\n*** Find transitions starting from the state in reachable states : \n");
    reachable_states |> List.fold_left (fun acc (st1, st2) -> 
      let cross_product_trans_from_states_pair = 
        find_transitions_from_state_pairs (st1, st2) a1.transitions a2.transitions debug_print in 
        cross_product_trans_from_states_pair @ acc) [] in
  let init_trans_reachable_trans: ((state * state) * (symbol * (state * state) list)) list = 
    raw_init_trans_ls @ raw_trans_from_reachables 
  in 
  (* Write the 'raw_trans_from_reachables' in blocks for better comparison *)
  let raw_trans_in_blocks_sorted: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    (if debug_print then printf "\n*** Putting raw transitions in blocks of transitions : \n");
    (start_states :: reachable_states) |> List.fold_left (fun acc (s1, s2) -> 
      let block = collect_raw_trans_for_states_pair (s1, s2) init_trans_reachable_trans in acc @ [(s1, s2), block]) []
      |> List.stable_sort (fun (_, trans_ls1) (_, trans_ls2) -> 
        Int.compare (List.length trans_ls2) (List.length trans_ls1))
  in 
  if debug_print then raw_trans_in_blocks_sorted |> Pp.pp_raw_trans_blocks;
  (* Find a list of duplicate states pairs *)
  let dup_states_pair_ls: ((state * state) * (state * state)) list = 
    (if debug_print then printf "\n*** Finding duplicate raw_states pairs : \n");
    find_duplicate_state_pairs_in_trans_blocks raw_trans_in_blocks_sorted debug_print
  in
  (* Remove transitions based on 'dup_states_pair_ls' *)
  let trans_in_blocks_cleaned: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    (if debug_print then printf "\n*** Removing transition blocks baesd on duplicate states : \n");
    remove_transitions_of_duplicate_states dup_states_pair_ls raw_trans_in_blocks_sorted debug_print
  in
  (* Replace state names based on 'dup_states_pair_ls'  *)
  let trans_in_blocks_replaced: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    (if debug_print then printf "\n*** Replacing duplicate state names in transition blocks : \n");
    replace_dup_state_names dup_states_pair_ls trans_in_blocks_cleaned debug_print
  in
  (* Rename states and populate Q, \Delta *)
  let states_renaming_map: ((state * state) * (state * state)) list =
    (if debug_print then printf "\n*** Collecting unique states and map to new states : \n");
    collect_unique_states_and_map_to_new_states trans_in_blocks_replaced debug_print in
  let start_renamed: state = 
    (find_renamed_state start_states states_renaming_map) |> state_pair_append in
  let state_pairs_renamed: (state * state) list = states_renaming_map |> List.map snd in 
  let res_states: state list = state_pairs_renamed |> List.map state_pair_append in
  let trans_in_blocks_renamed: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    rename_trans_blocks states_renaming_map trans_in_blocks_replaced debug_print
  in
  (* Introduce epsilon transitions to simplify \Delta *)
  let trans_in_blocks_simplified_with_epsilon_trans: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    (if debug_print then printf "\n*** Simplifying transition blocks with epsilon transitions : \n");
    simplify_trans_blocks_with_epsilon_transitions trans_in_blocks_renamed (List.rev state_pairs_renamed) debug_print
  in 
  let res_trans: transition list = 
    (if debug_print then printf "\n*** Rewriting transition blocks as transitions : \n");
    raw_trans_in_blocks_to_trans trans_in_blocks_simplified_with_epsilon_trans debug_print
  in 
  let res_ta: ta = { states = res_states @ [epsilon_state] ; alphabet = syms ; 
                 start_state = start_renamed ; transitions = res_trans; trivial_sym_nts = [] } in
  printf "\nResult of TA intersection: \n"; Pp.pp_ta res_ta; 
  res_ta (*|> rename_w_parser_friendly_states_in_ta debug_print *)


