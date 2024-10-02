open Ta
open Treeutils
open Cfg

(* --- helper --- *)
let accessible_symbols_for_state (init_st: state) (trans_tbl: ((state * symbol), sigma list list) Hashtbl.t) 
  (syms_ls: symbol list) (debug: bool): symbol list =    
  let rec interm_sts_loop (interm_sts': state list) (syms_ls: symbol list): symbol list * state list = 
    match interm_sts' with 
    | [] -> (remove_dup_symbols syms_ls), [] 
    | interm_hd :: interm_tl -> 
      Printf.printf "\n\t -->> Now looking for symbols starting from %s \n" interm_hd;
      let i_syms, next_interm_ls = find_accessible_symbols interm_hd [] syms_ls
      in if (next_interm_ls = []) 
         then interm_sts_loop interm_tl (i_syms @ syms_ls) 
         else interm_sts_loop next_interm_ls (i_syms @ syms_ls)
  and 
  find_accessible_symbols (from_st: state) (_interm_sts: state list) (syms_acc: symbol list): symbol list * state list =
    let curr_st_syms: symbol list = 
      syms_ls |> List.fold_left (fun acc sym -> if (exist_in_tbl from_st sym trans_tbl) then sym::acc else acc) [] 
    in
    (* collect intermediate states that are linked by eps-trans, save in 'interm_sts' and traverse til the end *)
    let curr_st_interm_sts: state list = 
      curr_st_syms |> List.fold_left (fun acc sym -> 
        if (syms_equals epsilon_symb sym)
        then 
          (let rhs_nonterms: sigma list = Hashtbl.find_all trans_tbl (from_st, epsilon_symb) |> List.flatten |> List.flatten in
           let rhs_sts: state list = rhs_nonterms 
           |> List.map (fun s -> match s with Nt s' -> s' | T _ -> raise No_terminal_possible) 
           |> List.filter (fun x -> not (String.equal epsilon_state x)) in
           rhs_sts @ acc)
        else acc) [] 
    in 
    if (List.is_empty curr_st_interm_sts) 
    then 
      (if debug then Printf.printf "\n\tInterm states empty! so terminate\n";
      (curr_st_syms @ syms_acc), [])
    else 
      (if debug then (Printf.printf "\n\tInterm states not empty: "; Pp.pp_states curr_st_interm_sts); 
      interm_sts_loop curr_st_interm_sts (curr_st_syms @ syms_acc))
  in let syms_res, _ = find_accessible_symbols init_st [] [] in 
  (if debug then Printf.printf "\n\t For state %s -->> .. Found Symbols: " init_st; 
    syms_res |> List.iter Pp.pp_symbol; Printf.printf "\n\n");
  syms_res

let cartesian_product_trans_from (starting_states: (state * state) list) 
  (trans_tbl1: ((state * symbol), sigma list list) Hashtbl.t) (trans_tbl2: ((state * symbol), sigma list list) Hashtbl.t) 
  (nontriv_syms: symbol list) (debug: bool): (((state * state) * symbol) * (sigma * sigma) list list) list = 
  let open Printf in
  (* --- helper to traverse alphabet and find transitions --- *)
  let find_rhs_in_tbl (sym: symbol) (st: state) (tbl: ((state * symbol), sigma list list) Hashtbl.t) = 
    let res = match Hashtbl.find_opt tbl (st, sym) with None -> []
    | Some sig_ls -> sig_ls in
    if debug then (printf "\n\tFinding rhs in tbl for (State %s, symbol " st; Pp.pp_symbol sym; printf ")\n\t";
      res |> List.iter Pp.pp_sigma_list2
      (* *** (debugging) *** *)
      (* printf "\t ---> in transitions table:\n"; Pp.pp_transitions_tbl tbl *)
      ); res
  in
  let find_transitions (st1: state) (st2: state): (((state * state) * symbol) * (sigma * sigma) list list) list =
    let rec interm_sts_loop (interm_sts': (state * state) list) (trans_ls: (((state * state) * symbol) * (sigma * sigma) list list) list): 
      (((state * state) * symbol) * (sigma * sigma) list list) list * (state * state) list = 
      match interm_sts' with 
      | [] -> trans_ls, []
      | (interm_st1, interm_st2) :: interm_sts_tl -> 
        if debug then (printf "\n\t -->>> Now looking for transitions from states ( %s, %s )\n" interm_st1 interm_st2);
        let syms1 = accessible_symbols_for_state interm_st1 trans_tbl1 nontriv_syms debug in 
        let syms2 = accessible_symbols_for_state interm_st2 trans_tbl2 nontriv_syms debug in 
        let syms_smaller = take_smaller_symbols_list syms1 syms2 debug in
        let i_trans, next_interm_sts_ls = traverse_symbols_and_find_trans interm_st1 interm_st2 syms_smaller trans_ls
        in
          if (List.is_empty next_interm_sts_ls)
          then interm_sts_loop interm_sts_tl (i_trans @ trans_ls)
          else interm_sts_loop next_interm_sts_ls (i_trans @ trans_ls)
    and 
    traverse_symbols_and_find_trans (from_st1: state) (from_st2: state) (s_ls: symbol list) 
      (trans_acc: (((state * state) * symbol) * (sigma * sigma) list list) list): 
      (((state * state) * symbol) * (sigma * sigma) list list) list * (state * state) list = 
      match s_ls with [] -> trans_acc, [] (* to change! *)
      | (hd_sym: symbol) :: syms_tl -> 
        let rsig_lsls1: sigma list list = find_rhs_in_tbl hd_sym from_st1 trans_tbl1 in
        let rsig_lsls2: sigma list list = find_rhs_in_tbl hd_sym from_st2 trans_tbl2 in
        let new_rht_sts: (sigma * sigma) list list = cross_product_raw_sigma_lsls rsig_lsls1 rsig_lsls2 debug 
        in
          if (List.is_empty new_rht_sts) 
          then traverse_symbols_and_find_trans from_st1 from_st2 syms_tl trans_acc
          else
            begin
              let curr_st_syms1: symbol list = 
                s_ls |> List.fold_left (fun acc sym -> if (exist_in_tbl from_st1 sym trans_tbl1) then sym::acc else acc) [] in
              let curr_st_syms2: symbol list = 
                s_ls |> List.fold_left (fun acc sym -> if (exist_in_tbl from_st2 sym trans_tbl2) then sym::acc else acc) [] in
              let curr_st_interm_sts1: state list = 
                curr_st_syms1 |> List.fold_left (fun acc sym -> 
                  if (syms_equals epsilon_symb sym)
                  then 
                    (let rhs_nonterms: sigma list = Hashtbl.find_all trans_tbl1 (from_st1, epsilon_symb) |> List.flatten |> List.flatten in 
                    let rhs_sts: state list = rhs_nonterms 
                    |> List.map (fun s -> match s with Nt s' -> s' | T _ -> raise No_terminal_possible)
                    |> List.filter (fun x -> not (String.equal epsilon_state x)) in
                    rhs_sts @ acc)
                  else acc) []
              in
              let curr_st_interm_sts2: state list = 
                curr_st_syms2 |> List.fold_left (fun acc sym -> 
                  if (syms_equals epsilon_symb sym)
                  then 
                    (let rhs_nonterms: sigma list = Hashtbl.find_all trans_tbl2 (from_st2, epsilon_symb) |> List.flatten |> List.flatten in 
                    let rhs_sts: state list = rhs_nonterms 
                    |> List.map (fun s -> match s with Nt s' -> s' | T _ -> raise No_terminal_possible)
                    |> List.filter (fun x -> not (String.equal epsilon_state x)) in
                    rhs_sts @ acc)
                  else acc) []
              in
              let curr_interm_sts_pairs: (state * state) list =  curr_st_interm_sts1 |> List.map (fun s1 -> 
                curr_st_interm_sts2 |> List.map (fun s2 -> (s1, s2))) |> List.flatten
              in
              if (List.is_empty curr_interm_sts_pairs)
              then 
                begin
                (if debug then printf "\n\t ***** Interm states empty!\n" )
                
                end
              else 
                (if debug then printf "\n\t ***** Interm states not empty : "; curr_interm_sts_pairs |> List.iter (fun (x1, x2) -> 
                  printf "(%s, %s) " x1 x2));
                let new_trans: ((state * state) * symbol) * (sigma * sigma) list list = 
                  (((from_st1, from_st2), hd_sym), new_rht_sts) in 
                interm_sts_loop curr_interm_sts_pairs (new_trans :: trans_acc)
            end
    in let trans_res, _ = traverse_symbols_and_find_trans st1 st2 nontriv_syms [] in 
    (if debug then printf "\n\t *** (debugging) For (State %s, State %s) \n\t *** (debugging) ----->> ... Found raw transitions \n" st1 st2; 
      Pp.pp_raw_transitions_new trans_res);
    trans_res
  in
  let rec cartesian_loop (starts: (state * state) list) (acc: (((state * state) * symbol) * (sigma * sigma) list list) list) = 
    match starts with [] -> acc 
    | (st1, st2) :: tl -> 
      let trans_to_acc: (((state * state) * symbol) * (sigma * sigma) list list) list = 
        find_transitions st1 st2 in
        cartesian_loop tl (trans_to_acc @ acc)
  in let res_st1_st2_trans: (((state * state) * symbol) * (sigma * sigma) list list) list = 
    cartesian_loop starting_states [] in
  let open Printf in
  if debug then (printf "\n   -->> Transitions starting from states [ "; 
    starting_states |> List.iter (fun (s1, s2) -> printf "(%s, %s) " s1 s2); printf "] : \n"; 
    Pp.pp_raw_transitions_new res_st1_st2_trans); res_st1_st2_trans
  
let find_reachable_states (from_sts: (state * state) list) (trans_ls: (((state * state) * symbol) * (sigma * sigma) list list) list) 
  (debug: bool): (state * state) list = 
  let rec get_state_pairs (sig_pairs_ls: (sigma * sigma) list) (stp_acc: (state * state) list): (state * state) list = 
    match sig_pairs_ls with [] -> stp_acc
    | (T _, T _) :: tl -> get_state_pairs tl stp_acc 
    | (Nt s1, Nt s2) :: tl -> 
      if (s1 = epsilon_state) || (s2 = epsilon_state)
      then get_state_pairs tl stp_acc
      else get_state_pairs tl ((s1, s2)::stp_acc)
    | (T _, Nt _) :: _ | (Nt _, T _) :: _ -> raise Reachable_states_not_matching 
  in 
  let rec find_loop (st1: state) (st2: state) (tls: (((state * state) * symbol) * (sigma * sigma) list list) list) 
    (find_acc: (state * state) list) = 
    match tls with [] -> find_acc
    | (((st1', st2'), _), sig_sig_ls_ls) :: tl -> 
      if (st1 = st1') && (st2 = st2')
      then 
        (let sig_pairs_ls = sig_sig_ls_ls |> List.flatten in 
         let state_pairs_ls: (state * state) list = get_state_pairs sig_pairs_ls [] in 
         find_loop st1 st2 tl (state_pairs_ls @ find_acc))
      else
        (find_loop st1 st2 tl find_acc)
  in 
  let rec traverse_from_states (ls: (state * state) list) (acc: (state * state) list) = 
    match ls with [] -> acc 
    | (st1, st2) :: tl -> 
      let state_pairs_ls: (state * state) list = find_loop st1 st2 trans_ls [] in
      let state_pairs_wo_itself: (state * state) list = state_pairs_ls 
        |> List.filter (fun (s1, s2) -> (not (String.equal s1 st1)) || (not (String.equal s2 st2))) in
      traverse_from_states tl (state_pairs_wo_itself @ acc)
  in let res = traverse_from_states from_sts [] in
  let open Printf in 
  (if debug then printf "\n  >> Reachable states from init state_pairs"; 
  Pp.pp_raw_states from_sts; Pp.pp_raw_states res); res

  

let find_transitions_from_state_pairs (st1: state) (st2: state) 
  (trans_tbl1: ((state * symbol), sigma list list) Hashtbl.t) (trans_tbl2: ((state * symbol), sigma list list) Hashtbl.t) 
  (nontriv_syms: symbol list) (debug: bool): (((state * state) * symbol) * (sigma * sigma) list list) list =
  (* To start from here! *)
  cartesian_product_trans_from [(st1, st2)] trans_tbl1 trans_tbl2 nontriv_syms debug 



(** Intersection of tree automata *)
let intersect (a1: ta2) (a2: ta2) (verSyms: (string * int list) list) (trivSyms: symbol list) (debug_print: bool): ta2 =
  let open Printf in
  printf "\nIntersect the following 2 TAs:\n\n  (1) First TA:\n";
  Pp.pp_ta2 a1; printf "\n  (2) Second TA:\n"; Pp.pp_ta2 a2; printf "\n";
  if debug_print then (printf "\n  >> Versatile symbol list: [ "; 
  verSyms |> List.map fst |> List.iter (fun x -> printf "%s " x); printf "]\n");
  (* Consider symbols excluding trivial symbols *)
  let syms = a1.alphabet in (* TODO: Add a sanity check on alphabet based on set equality *)
  let syms_nontrivial = syms |> List.filter (fun s -> not (List.mem s trivSyms)) in
  (* Find I := I_1 x I_2 *)
  let start_states_raw: (state * state) list = 
    a1.start_states |> List.map (fun s1 -> a2.start_states |> List.map (fun s2 -> (s1, s2))) |> List.flatten
  in
  (* Get transitions for nontriv symbols that start from I *)
  let raw_init_trans_ls: (((state * state) * symbol) * (sigma * sigma) list list) list = 
    cartesian_product_trans_from start_states_raw a1.transitions a2.transitions syms_nontrivial debug_print 
  in
  (if debug_print then printf "\n *** Find initial states-starting transitions : \n"; 
    Pp.pp_raw_transitions_new raw_init_trans_ls);
  (* Find reachable states based on I-starting transitions *)
  let init_reachable_states: (state * state) list = 
    find_reachable_states start_states_raw raw_init_trans_ls debug_print 
  in
  (if debug_print then printf "\n *** Found reachable states based on initial states-starting transitions : \n"; 
    Pp.pp_raw_states init_reachable_states);
  (* Based on (Ei, Ej) in list of reachable states, find transitions starting from (Ei, Ej) *)
  
  
  (* To resume from here! *)
  (* As I find more reachable states, need to collect transitions from the reachable states
   * Then, combination of the transitions will be initial raw transitions! *)
  (* 
  let raw_trans_from_reachables: (((state * state) * symbol) * (sigma * sigma) list list) list = 
    init_reachable_states |> List.fold_left (fun acc (st1, st2) -> 
      let alph1: symbol list = accessible_symbols_for_state st1 a1.transitions syms_nontrivial debug_print in
      let alph2: symbol list = accessible_symbols_for_state st2 a2.transitions syms_nontrivial debug_print in
      let alph_new: symbol list = take_smaller_symbols_list alph1 alph2 debug_print in
      let cross_product_trans_from_states_pair = 
        find_transitions_from_state_pairs st1 st2 a1.transitions a2.transitions alph_new debug_print in 
        cross_product_trans_from_states_pair @ acc) [] in
  (if debug_print then printf "\n *** Find transitions from reachable states : \n"; 
    Pp.pp_raw_transitions_new raw_trans_from_reachables);
   *)
  
  let rec collect_all_raw_trans (states_reachable_left: (state * state) list) (states_reachabe_acc)
    (trans_acc: (((state * state) * symbol) * (sigma * sigma) list list) list): 
    (((state * state) * symbol) * (sigma * sigma) list list) list = 
    match states_reachable_left with [] -> trans_acc
    | (st_hd1, st_hd2) :: reachable_states_tl -> 
      (* --- find transitions from the curr states pair --- *)
      let alph1: symbol list = accessible_symbols_for_state st_hd1 a1.transitions syms_nontrivial debug_print in 
      let alph2: symbol list = accessible_symbols_for_state st_hd2 a2.transitions syms_nontrivial debug_print in 
      let alph_overlapped: symbol list = take_smaller_symbols_list alph1 alph2 debug_print 
      in 
      let curr_raw_trans_from_states_pair = 
        cartesian_product_trans_from [(st_hd1, st_hd2)] a1.transitions a2.transitions alph_overlapped debug_print
        (* find_transitions_from_state_pairs st_hd1 st_hd2 a1.transitions a2.transitions alph_overlapped debug_print  *)
      in
      (* --- collect reachable states from the curr states pair --- *)
      let curr_reachable_states = 
        find_reachable_states [(st_hd1, st_hd2)] curr_raw_trans_from_states_pair debug_print 
      in
      (* --- pass in as new 'states_reachable', the ones that do not already appeared --- *)
      let new_states_reachable_to_add: (state * state) list = 
        curr_reachable_states |> List.filter (fun x -> not (List.mem x states_reachabe_acc))
      in 
        collect_all_raw_trans 
          (reachable_states_tl @ new_states_reachable_to_add) 
          (states_reachabe_acc @ new_states_reachable_to_add)
          (curr_raw_trans_from_states_pair @ trans_acc)
  in let all_raw_trans_from_all_reachables = 
    collect_all_raw_trans init_reachable_states init_reachable_states [] in 
  (if debug_print then printf "\n *** All the raw trasitions from all the reachable states : \n"; 
    Pp.pp_raw_transitions_new all_raw_trans_from_all_reachables);


  (*   
  let init_trans_reachable_trans: ((state * state) * (symbol * (state * state) list)) list = 
    raw_init_trans_ls @ raw_trans_from_reachables 
  in *)
  
  
  (* 
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
  let _res_ta_old: ta = { states = res_states @ [epsilon_state] ; alphabet = syms ; 
                 start_state = start_renamed ; transitions = res_trans; trivial_sym_nts = [] } in
  *)
  (*  *)
  let starts: state list = start_states_raw |> List.map (fun (s1, s2) -> s1 ^ "_" ^ s2) in 
  let res_ta: ta2 = { states = [] ; alphabet = syms ; 
                    start_states = starts ; transitions = Hashtbl.create 0 ; trivial_sym_nts = [] } in
  printf "\nResult of TA intersection: \n"; Pp.pp_ta2 res_ta; 
  res_ta (*|> rename_w_parser_friendly_states_in_ta debug_print *)








(*

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
 *)







  