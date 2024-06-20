open Ta
open Treeutils

let cartesian_product_trans (states1: state list) (states2: state list) (trans1: transition list) (trans2: transition list) 
  (syms: symbol list) (verSyms: (string * int list) list) (debug_print: bool): transition list =
  let open List in
  let open Printf in
  if debug_print then (printf "\n  >> Cross product of transitions:\n\tFirst transitions:\n"; 
  Pp.pp_transitions trans1; printf "\n\tSecond transitions:\n"; Pp.pp_transitions trans2);
  (** helpers *)
  let epsSym, parenSym = ("ε", 1), ("()", 1) in
  let vers_symNames = verSyms |> map fst in
  let remove_epsilon ls = filter (fun x -> not (x = "ϵ")) ls in
  let stats1, stats2 = remove_epsilon states1, remove_epsilon states2 in
  if debug_print then (printf "\n\tGiven two lists of states:\n\t"; Pp.pp_states stats1; printf "\t"; Pp.pp_states stats2);
  (** cartesian_tuples : combine two lists of states and make ((state1 * state2) * state1state2) list *)
  let cartesian_tuples l l': ((state * state) * state) list = 
    let is_there_one_cond s s': bool = (is_cond_expr s && not (is_cond_expr s')) || (not (is_cond_expr s) && is_cond_expr s') in
    concat (map (fun e -> (map (fun e' -> ((e, e'), e^e'))) l') l) |> filter (fun ((s, s'), _) -> not (is_there_one_cond s s')) in
  (** find_rhs_states : based on lhs_state and sym, find corresonding 'Some rhs_states'
      and return 'None' if it doesn't find corresponding list
      when it's one of the varsatile symbols, treat it differently so it generates correct rhs_states *)
  let find_rhs_states (lhs_state: state) (sym: symbol) (trans: transition list) (debug: bool): state list option = 
    if debug then printf "\n\tFor state '%s' and symbol \"%s\", we get the following RHS states:\n" lhs_state (fst sym);
    let rec traverse_trans lhs_stat ls: state list option =
      match ls with
      | [] -> None (* reached end and found none *)
      | (lhs, (s, rhs_states)) :: tl ->
        if ((lhs = lhs_stat) && (syms_equals s sym) && not (mem (fst sym) vers_symNames)) then (Some rhs_states) else 
        if ((lhs = lhs_stat) && (syms_equals s sym) && (mem (fst sym) vers_symNames) && (length rhs_states = (snd sym)))
        then (Some rhs_states) else (* record_counter := !record_counter + 1; *)
        if ((lhs = lhs_stat) && (syms_equals s sym) && (mem (fst sym) vers_symNames) && not (length rhs_states = (snd sym)))
        then (traverse_trans lhs tl) else
        (* assuming ε-tran is happening before all others -> TODO: re-arrange before running *)
        (* if ((lhs = lhs_stat) && (sym_equals s "ε") && (length rhs_states = 1)) then traverse_trans (hd rhs_states) tl else *)
        traverse_trans lhs_stat tl
    in let res_trav = traverse_trans lhs_state trans in if debug then 
    (match res_trav with None -> printf "\t\tNo matching RHS states\n" | Some l -> printf "\t"; Pp.pp_states l); res_trav 
  in
  let find_rhs_states_eps_sym (lhs_state: state) (trans: transition list) (debug: bool): (state list) list option =
    if debug then printf "\n\tFor state'%s' and \"ε\" symbol, we get the following list of RHS states:\n" lhs_state;
    let rec traverse_trans lhs_stat ls acc: (state list) list =
      match ls with [] -> rev acc
      | (lhs, (s, rhs_states)) :: tl -> 
        if ((lhs = lhs_stat) && (syms_equals s epsSym)) 
        then traverse_trans lhs_stat tl (rhs_states :: acc) 
        else traverse_trans lhs_stat tl acc
    in let res_trav = traverse_trans lhs_state trans [] 
    in match res_trav with [] -> (if debug then printf "\t\tNo match RHS states\n"); None
       | ls -> (if debug then ls |> iter (fun l -> printf "\t"; Pp.pp_states l; printf "\n")); Some ls
  in
  let states_tuples: ((state * state) * state) list = cartesian_tuples stats1 stats2 in
  (** cartesian_trans : take cartesian products of two sets of transitions *)
  let rec cartesian_rhs_states ls1 ls2 res: state list = 
    match ls1, ls2 with [], [] -> rev res 
    | (h1::tl1), (h2::tl2) -> 
      if ((h1 = "ϵ") || (h2 = "ϵ")) then cartesian_rhs_states tl1 tl2 ("ϵ" :: res) 
    else cartesian_rhs_states tl1 tl2 ((h1^h2) :: res)
    | _, [] | [], _ -> raise (Invalid_argument "RHS states do not match!")
  in
  let rec cartesian_trans l acc: transition list =
    match l with [] -> acc
    | ((s1, s2), s1s2) :: tl -> 
      let trans_ls: transition list = syms |> fold_left (fun acc_lst sym -> 
        if (not (sym_equals sym "ε")) then
        begin match (find_rhs_states s1 sym trans1 debug_print), (find_rhs_states s2 sym trans2 debug_print) with 
        | Some rhs_states1, Some rhs_states2 ->
          let rhs_states_comb: state list = cartesian_rhs_states rhs_states1 rhs_states2 [] in (s1s2, (sym, rhs_states_comb))::acc_lst
        | Some _, None | None, Some _ | None, None -> ("dummy", (sym, []))::acc_lst
        end else 
        (* there can be multiple epsilon transitions, so treat them differently *)
        begin match (find_rhs_states_eps_sym s1 trans1 debug_print), (find_rhs_states_eps_sym s2 trans2 debug_print) with 
        | Some rhs_statesls1, Some rhs_statesls2 -> 
          (* flatten (state list) list to state list, knowing that ε-trans will have 1 rhs_state *)
          let eps_trans: transition list = 
            let interm: state list = List.fold_left (fun acc rstats1 -> 
                let intermediate = fold_left (fun acc' rstats2 -> 
                  (rstats1^rstats2) :: acc') [] (flatten rhs_statesls2) in 
                  intermediate @ acc) [] (flatten rhs_statesls1)
            in fold_left (fun res_acc rhs_states -> 
              (rhs_states, (("()", 1), [s1s2]))::(s1s2, (("ε", 1), [rhs_states]))::res_acc) [] interm 
            in eps_trans @ acc_lst
          (* TODO: Will have to filter out the one that has same lhs and rhs with epsilon transition! *)
        | Some _, None | None, Some _ | None, None -> ("dummy", (sym, []))::acc_lst 
        end) [] |> filter (fun (lhs, (sym, rhs_states)) -> 
          not (lhs = "dummy") && not (syms_equals sym epsSym && (hd rhs_states) = lhs)
          && not (syms_equals sym parenSym && (hd rhs_states) = lhs)) 
        in cartesian_trans tl (acc @ trans_ls)
  in let res_trans: transition list = cartesian_trans states_tuples [] in
  if debug_print then (printf "\n  >> Result of trans X trans:\n"; 
  Pp.pp_transitions res_trans); res_trans

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
    | [] -> syms_ls, []
    | interm_hd :: interm_tl -> 
      Printf.printf "\n\tNow looking for symbols starting from %s \n" interm_hd;
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
  (if debug then Printf.printf "\n\t\t >> .. Found Symbols: "; syms_res |> List.iter Pp.pp_symbol);
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
  let raw_trans_in_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    (if debug_print then printf "\n*** Putting raw transitions in blocks of transitions : \n");
    (start_states :: reachable_states) |> List.fold_left (fun acc (s1, s2) -> 
      let block = collect_raw_trans_for_states_pair (s1, s2) init_trans_reachable_trans in acc @ [(s1, s2), block]) []
  in 
  if debug_print then raw_trans_in_blocks |> Pp.pp_raw_trans_blocks;
  (* Find a list of duplicate states pairs *)
  let dup_states_pair_ls: ((state * state) * (state * state)) list = 
    (if debug_print then printf "\n*** Finding duplicate raw_states pairs : \n");
    find_duplicate_state_pairs_in_trans_blocks raw_trans_in_blocks debug_print
  in
  (* Remove transitions based on 'dup_states_pair_ls' *)
  let trans_in_blocks_cleaned: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    (if debug_print then printf "\n*** Removing transition blocks baesd on duplicate states : \n");
    remove_transitions_of_duplicate_states dup_states_pair_ls raw_trans_in_blocks debug_print
  in
  (* Replace state names based on 'dup_states_pair_ls'  *)
  let  trans_in_blocks_replaced: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    (if debug_print then printf "\n*** Replacing duplicate state names in transition blocks : \n");
    replace_dup_state_names dup_states_pair_ls trans_in_blocks_cleaned debug_print
  in
  (* Rename states and populate Q, \Delta *)
  let _renamed_states_map: ((state * state) * (state * state)) list =
    (if debug_print then printf "\n*** Collecting unique states and map to new states : \n");
    collect_unique_states_and_map_to_new_states trans_in_blocks_replaced debug_print
  in
  (* Introduce epsilon transitions to simplify \Delta *)
  (* 
  let stats1, stats2 = a1.states, a2.states in
  let stats = cartesian_product_states stats1 stats2 debug_print in
   *)
  (* 
  let _(* trans*) = 
    cartesian_product_trans stats1 stats2 a1.transitions a2.transitions syms verSyms debug_print in
   *)
  let res_ta = { states= [] ; alphabet=syms ; start_state= (state_pair_append start_states) ; transitions= [] } in
  printf "\nResult of TA intersection: \n"; Pp.pp_ta res_ta; 
  res_ta |> rename_states debug_print


