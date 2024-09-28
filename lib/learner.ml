open Ta
open Treeutils
open Cfg

exception No_state_for_sym_order
exception Max_level_state

let get_states (op_ls: restriction list): ((int * state) list) * state = 
  let default_states = [(0, "C"); (-1, "ϵ")] in
  let gen_states: (int * state) list = 
    let num_levels = levels_in_op_ls op_ls in
    let rec gen_loop lvl curr_idx acc =
      if (lvl = 0) then List.rev acc
      else (let new_state = "E" ^ (string_of_int curr_idx)
            in gen_loop (lvl-1) (curr_idx+1) ((curr_idx, new_state)::acc))
    in gen_loop num_levels 1 []
  in
  (gen_states @ default_states), (List.hd gen_states |> snd)

let get_transitions (oa_ls: restriction list) (op_ls: restriction list) 
  (_o_bp_tbl: (int, symbol list) Hashtbl.t) (sigma_lhs_ls: (sigma * state) list)
  (a: symbol list) (lvl_state_pairs: (int * state) list) 
  (start: state) 
  (_transitions_tbl: ((state * symbol), sigma list list) Hashtbl.t)
  (debug: bool): transition list =
  let open Printf in
  let trivial_syms = a |> List.filter (fun (_, rnk) -> (rnk = 0) ) in (* || (rnk = 1) *)
  let nontrivial_syms = a |> List.filter (fun (_, rnk) -> not (rnk = 0) ) (* && not (rnk = 1) *)
    (* |> List.map (fun (s, rnk) -> if (s = "+") then ("PLUS", rnk) else if (s = "*") then ("MUL", rnk) else (s, rnk))  *)
  in
  (if debug then 
    printf "\nTrivial symbols :\n\t"; trivial_syms |> List.iter (fun s -> Pp.pp_symbol s); printf "\n";
    printf "\nNontrivial symbols :\n\t"; nontrivial_syms |> List.iter (fun s -> Pp.pp_symbol s)); printf "\n";
  (* --- helper 'last_state' for 'gen_trans_trivials' --- *)
  let max_lvl = lvl_state_pairs |> List.map fst |> List.fold_left max 1 in
  let last_state: state = match (List.assoc_opt max_lvl lvl_state_pairs) with None -> raise Max_level_state
                   | Some st -> st in 
  let last_paren_to_fst = (last_state, (("LPARENRPAREN", 1), [start])) 
  in
  let find_lhs (sym: symbol): state = 
    let sym_sigma = sigma_lhs_ls |> List.assoc_opt (T (fst sym)) in
    match sym_sigma with None -> raise Not_found
    | Some st -> st
  in 
  (* transitions for trivial symbols (eg, (), INT, BOOL) *)
  let rec gen_trans_trivials sym_ls acc: transition list = 
    match sym_ls with [] -> acc 
    | sym :: tl -> 
      let triv_state = find_lhs sym in
      gen_trans_trivials tl ((triv_state, (sym, [epsilon_state]))::acc) in 
  let trans_trivials: transition list = gen_trans_trivials trivial_syms [last_paren_to_fst]
  in 
  if debug then printf "\n  >> Trivial transitions \n"; Pp.pp_transitions trans_trivials;
  (* *** debug WIP *** *)
  (* --- helper 'get_sym_state' for 'gen_trans_nontrivials' --- *)
  let get_sym_state (s: symbol) = 
    let sym_order = (order_in_op_lst s op_ls) + 1 in
    match (List.assoc_opt sym_order lvl_state_pairs) with 
    | None -> raise No_state_for_sym_order | Some st -> st 
  in
  let rec gen_trans_nontrivials sym_ls acc: transition list =
    match sym_ls with [] -> acc
    | sym :: tl -> 
      (Printf.printf "\n\tLooking at <%s, %i>\n" (fst sym) (snd sym));
      let sym_state = get_sym_state sym in
      if sym_in_oa_lst sym oa_ls
      then (let higher_state: state = get_higher_state sym_state in 
              if is_left_assoc sym oa_ls
              then (let trans = (sym_state, (sym, [sym_state; higher_state])) 
                    in gen_trans_nontrivials tl (trans::acc))
              else (let trans = (sym_state, (sym, [higher_state; sym_state]))
                    in gen_trans_nontrivials tl (trans::acc)))
      else 
        (let rhs_states: state list = gen_rhs_states sym sym_state
         in let trans = (sym_state, (sym, rhs_states))
         in gen_trans_nontrivials tl (trans::acc))
  in let trans_nontrivals: transition list = gen_trans_nontrivials nontrivial_syms [] 
  in 
  if debug then printf "\n  >> Nontrivial transitions \n"; Pp.pp_transitions trans_nontrivals;
  let rec gen_epsilon_trans num_levels i acc = 
    if num_levels = 1 then acc 
    else begin 
      let (left_st, right_st): state * state = "E" ^ (string_of_int i), "E" ^ (string_of_int (i+1)) in 
      let new_eps_trans: transition = (left_st, (epsilon_symb, [right_st])) in 
      gen_epsilon_trans (num_levels - 1) (i+1) (new_eps_trans::acc)
    end
  in let eps_trans: transition list = gen_epsilon_trans (levels_in_op_ls op_ls) 1 []
  (* (TODO) add epsilon transitions *)
  in trans_nontrivals @ trans_trivials @ eps_trans

let learn_ta (oa_ls: restriction list) (op_ls: restriction list) (o_bp_tbl: (int, symbol list) Hashtbl.t) 
  (sigma_state_ls: (sigma * state) list) (a: symbol list) 
  (transitions_tbl: ((state * symbol), sigma list list) Hashtbl.t) (debug_print: bool): ta = 
  let open Printf in 
  if debug_print then (printf "\n\nLearn a tree automaton based on:\n\tO_a: ";
  Pp.pp_restriction_lst oa_ls; printf "\n\tO_p: "; Pp.pp_restriction_lst op_ls; 
  printf "\n\tAlphabet: { "; a |> List.iter Pp.pp_symbol; printf "}\n");
  let (lvl_state_pairs, init_state): (int * state) list * state = get_states op_ls in
  let state_ls: state list = lvl_state_pairs |> List.map snd in
  let raw_trans_ls: transition list = 
    get_transitions oa_ls op_ls o_bp_tbl sigma_state_ls a lvl_state_pairs init_state transitions_tbl debug_print in
  let ordered_trans_ls = order_trans_ls state_ls raw_trans_ls in
  let ta_res = { states = state_ls; alphabet = a; start_state = init_state; transitions = ordered_trans_ls; trivial_nts=[] } in 
  printf "\n\nLearned TA:\n"; Pp.pp_ta ta_res; ta_res


