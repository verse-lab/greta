open Ta
open Treeutils

exception No_state_for_sym_order
exception Max_level_state

let get_states (op_ls: restriction list): ((int * state) list) * state = 
  let default_states = [(0, "C"); (-1, "ϵ")] in
  let gen_states: (int * state) list = 
    let num_levels = List.length op_ls in
    let rec gen_loop lvl curr_idx acc =
      if (lvl = 0) then List.rev acc
      else (let new_state = "E" ^ (string_of_int curr_idx)
            in gen_loop (lvl-1) (curr_idx+1) ((curr_idx, new_state)::acc))
    in gen_loop num_levels 1 []
  in
  (gen_states @ default_states), (List.hd gen_states |> snd)

let get_transitions (oa_ls: restriction list) (op_ls: restriction list) (a: symbol list)
  (_(* versatiles *): (string * int list) list) (lvl_state_pairs: (int * state) list) (start: state) (debug: bool): transition list =
  let trivial_syms = a |> List.filter (fun (_, rnk) -> (rnk = 0) || (rnk = 1)) in
  let nontrivial_syms = a |> List.filter (fun (_, rnk) -> not (rnk = 0) && not (rnk = 1))
    |> List.map (fun (s, rnk) -> if (s = "+") then ("PLUS", rnk) else if (s = "*") then ("MUL", rnk) else (s, rnk)) 
  in
  (if debug then 
    Printf.printf "\nTrivial symbols :\n\t"; trivial_syms |> List.iter (fun s -> Pp.pp_symbol s);
    Printf.printf "\nNontrivial symbols :\n\t"; nontrivial_syms |> List.iter (fun s -> Pp.pp_symbol s));
  (* helper 'get_sym_state' for 'gen_trans_nontrivials' *)
  let get_sym_state (s: symbol) = 
    let sym_order = (order_in_op_lst s op_ls) + 1 in
    match (List.assoc_opt sym_order lvl_state_pairs) with 
    | None -> raise No_state_for_sym_order | Some st -> st in
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
  in let trans_nontrivals: transition list = gen_trans_nontrivials nontrivial_syms [] in 
  (* helper 'last_state' for 'gen_trans_trivials' *)
  let max_lvl = lvl_state_pairs |> List.map fst |> List.fold_left max 1 in
  let last_state: state = match (List.assoc_opt max_lvl lvl_state_pairs) with None -> raise Max_level_state
                   | Some st -> st in 
  let rec gen_trans_trivials sym_ls acc: transition list = 
    match sym_ls with [] -> acc 
    | sym :: tl -> 
      if (sym_equals sym "()") 
      then gen_trans_trivials tl ((last_state, (sym, [start]))::acc)
      else if (sym_equals sym "N")
      then gen_trans_trivials tl ((last_state, (sym, ["ϵ"]))::acc)
      else if (sym_equals sym "B")
      then gen_trans_trivials tl (("C", (sym, ["ϵ"]))::acc)
      else if (syms_equals sym epsilon_symb)
      then gen_trans_trivials tl acc (* epsilon transitions to gen separately below *)
      else raise No_other_trivial_symbols
  in let trans_trivials: transition list = gen_trans_trivials trivial_syms []
  (* (TODO) add epsilon transitions *)
  in trans_nontrivals @ trans_trivials

let learn_ta (oa_ls: restriction list) (op_ls: restriction list) (a: symbol list) 
  (versatile_syms: (string * int list) list) (debug_print: bool): ta = 
  let open Printf in 
  if debug_print then (printf "\n\nLearn a tree automaton based on:\n\tO_a: ";
  Pp.pp_restriction_lst oa_ls; printf "\n\tO_p: "; Pp.pp_restriction_lst op_ls; 
  printf "\n\tAlphabet: { "; a |> List.iter Pp.pp_symbol; printf "}\n");
  let (lvl_state_pairs, init_state): (int * state) list * state = get_states op_ls in
  let state_ls: state list = lvl_state_pairs |> List.map snd in
  let trans_ls: transition list = get_transitions oa_ls op_ls a versatile_syms lvl_state_pairs init_state debug_print in
  let ta_res = { states = state_ls; alphabet = a; start_state = init_state; transitions = trans_ls } in 
  printf "\n\nLearned TA:\n"; Pp.pp_ta ta_res; ta_res


  