open Ta
open Treeutils

exception No_state_for_sym_order
exception Max_level_state

(** redefine_tree : generate states, start_state, redefine e as e' wrt these states *)
let redefine_tree (e: tree) (debug_print: bool): state list * state * state * state * tree =
  if debug_print then (Printf.printf "\n  >> Redefining tree.. \n\tInput: "; Pp.pp_tree e); 
  let in_the_syms (elems: symbol list) (ls: symbol list): bool = 
    elems |> List.exists (fun x -> List.mem x ls) in
  (* if there's any conditional state in example tree, pass the correct name for 'trans_non_example' *)
  let states_res, start_res, cond_state, dirchild_res = ref ["ϵ"], ref "", ref "Cond_expr_1", ref "" in
  let syms_e: symbol list ref = ref [] in
  let rec traverse_loop e dep =
    match e with
    | Leaf s ->
      (* gen states so it can differentiate cond_expr from expr *)
      let s' = if (is_cond_expr s) then (let new_s = (String.capitalize_ascii s) ^ "_1" in cond_state := new_s; new_s) 
      else (String.capitalize_ascii s) ^ "_" ^ string_of_int dep in
      (* update the start state accordingly *)
      if (dep = 1) then start_res := s' else if (dep = 2) then dirchild_res := s' else
      if (dep = 0) then (start_res := s'; Printf.printf "Trivial tree with only 1 leaf");
      if not (List.mem s' !states_res) then states_res := s' :: !states_res; Leaf s'
    | Node (sym, ts) ->
      let ts' = ts |> List.map (fun x-> traverse_loop x (dep + 1)) in
      syms_e := sym :: !syms_e; Node (sym, ts')
  in let e' = traverse_loop e 0 in
  (* add a state associated with boolean *)
  if not (in_the_syms [("B", 0); ("IF", 2); ("IF", 3)] !syms_e) 
    then states_res := ("Cond_expr_1") :: !states_res;
  if debug_print then (Printf.printf "\n\tRedefined: "; Pp.pp_tree e');
  !states_res, !start_res, !cond_state, !dirchild_res, e'

(** gen_transitions : traverse e and gen Σ_e-, ε-, ()- trans per parent-child *)
let gen_transitions (t: tree) (a: symbol list) (root_st: state) (versatiles: (string * int list) list) 
  (cond_state: state) (debug_print: bool): transition list =
  let open List in
  let open Printf in
  if debug_print then (printf "\n\n  >> Generating transitions..\n");
  (** helpers *)
  let h = height t in
  let vers_symNames = versatiles |> map fst in
  let res_dep: int ref = ref 0 in
  let versatile_used_arity = ref ~-1 in
  let find_next_unused_arity (vers: (string * int list) list) (symName: string) (used_arity: int): int =
    assoc symName vers |> filter (fun x -> not (x = used_arity)) |> hd in
  (* traverse example tree to generate trans using Σ_ex,ε,() *)
  let rec traverse_example t dep parent trans_acc syms_acc: transition list * symbol list =
    match t with Leaf _ -> trans_acc, syms_acc
    | Node (sym, ts) ->
      res_dep := !res_dep + 1;
      if (mem (fst sym) vers_symNames) then versatile_used_arity := (length ts);
      let lhs_state: state = ts |> filter (fun x -> 
        is_leaf x && not (is_conditional_leaf x)) |> hd |> return_state in
      let rhs_states: state list = subts_state_list sym ts lhs_state vers_symNames cond_state in
      let tran_sym = lhs_state, (sym, rhs_states) in
      let trans_subts: transition list = ts |> map (fun subt -> 
        fst (traverse_example subt (dep+1) lhs_state trans_acc syms_acc)) |> flatten in
      let syms_subts: symbol list = ts |> map (fun subt -> 
        snd (traverse_example subt (dep+1) lhs_state trans_acc syms_acc)) |> flatten in
      if (dep > 0 && dep <= h) then 
        let tran_eps = parent, (("ε", 1), [lhs_state]) in
        let tran_paren = lhs_state, (("()", 1), [parent]) in
        (trans_subts @ tran_sym::tran_eps::tran_paren::trans_acc), (syms_subts @ sym::syms_acc)
      else (tran_sym::[] @ trans_acc @ trans_subts), (sym::syms_acc @ syms_subts)
  in let (trans_example, syms_example) = traverse_example t 0 "" [] [] in
  if debug_print then (printf "\tΣ_ex : { "; syms_example |> iter (fun x -> Pp.pp_symbol x); printf "}\n");
  let syms_non_example: symbol list = a |> filter (fun x -> if (!res_dep > 1) 
    then not (mem x syms_example) && not (fst x = "()") && not (fst x = "ε") 
    else not (mem x syms_example) && not (fst x = "()")) in
  if debug_print then (printf "\tΣ\\{Σ_ex,ε,()}: { "; syms_non_example |> iter (fun s -> Pp.pp_fst s); printf "}\n");
  (* gen conservative trans -- eg E1 ->_{sym} E1 E1 .. -- for Σ\{Σ_ex,ε,()} *)
  let trans_non_example: transition list = syms_non_example |> map (fun s -> 
    let rhs_states': state list = 
      if (arity s <> 0 && (mem (fst s) vers_symNames) && not (!versatile_used_arity = ~-1))
      then 
        (let unused_arity = find_next_unused_arity versatiles (fst s) !versatile_used_arity in
        init (unused_arity) (fun _ -> root_st))
      else if (arity s <> 0 && (mem (fst s) vers_symNames) && (!versatile_used_arity = ~-1))
      then init (arity s) (fun _ -> root_st)
      else if (arity s <> 0 && not (mem (fst s) vers_symNames)) 
      then init (arity s) (fun _ -> root_st)
      else init 1 (fun _ -> "ϵ") in
    (* TODO: Differentiate IF's conditional based on info from original TA, incorporate to Algo *)
      if (mem (fst s) vers_symNames) (* (sym_equals s "IF") *)
      then (let rhs_states'' = rhs_states' |> mapi (fun i x -> 
        if (i=0) then cond_state else x) in root_st, (s, rhs_states''))
      else if (sym_equals s "B")
      then cond_state, (s, rhs_states') 
      else root_st, (s, rhs_states'))
  in let eps_trans = [root_st, (("ε", 1), [root_st])] in
  let trans_res = trans_example @ trans_non_example @ eps_trans
  in if debug_print then (Pp.pp_transitions trans_res);
  trans_res

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

(* Previous version *)
let learner (e: tree) (a: symbol list) (versatiles: (string * int list) list) (debug_print: bool): ta =
  let open Printf in
  if debug_print then (printf "\n\nLearn a tree automaton from an example where inputs are\n\tExample: ";
  Pp.pp_tree e; printf "\n\tAlphabet: { "; a |> List.iter Pp.pp_symbol; printf "}\n");
  let state_ls, strt, cond_state, _, e' = redefine_tree e debug_print in
  let trans = gen_transitions e' a strt versatiles cond_state debug_print in
  let ta_res = { states = state_ls; alphabet = a; start_state = strt; transitions = trans } in
  printf "\n\nLearned TA:\n"; Pp.pp_ta ta_res; ta_res
