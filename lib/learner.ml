open Ta

(** redefine_tree : generate states, start_state, redefine e as e' wrt these states *)
let redefine_tree (e: tree) (debug_print: bool): state list * state * state * tree =
  if debug_print then (Printf.printf "\nRedefining tree.. \n\tInput: "; Pp.pp_tree e); 
  let states_res, start_res, dirchild_res = ref ["ϵ"], ref "", ref "" in
  let rec loop e dep =
    match e with
    | Leaf s ->
      (* gen states so it can differentiate cond_expr from expr *)
      let s' = (String.capitalize_ascii s) ^ "_" ^ string_of_int dep in
      (* update the start state accordingly *)
      if (dep = 1) then start_res := s' else if (dep = 2) then dirchild_res := s' else
      if (dep = 0) then (start_res := s'; Printf.printf "Trivial tree with only 1 leaf");
      if not (List.mem s' !states_res) then states_res := s' :: !states_res; Leaf s'
    | Node (sym, ts) ->
      let ts' = ts |> List.map (fun x-> loop x (dep + 1)) in
      Node (sym, ts')
  in let e' = loop e 0
  in if debug_print then (Printf.printf "\n\tRedefined: "; Pp.pp_tree e');
  !states_res, !start_res, !dirchild_res, e'

(** gen_transitions : traverse e and gen Σ_e-, ε-, ()- trans per parent-child *)
let gen_transitions (t: tree) (a: symbol list) (root_st: state) (dirchild_st: state) (debug_print: bool): transition list =
  let open List in
  let open Printf in
  if debug_print then (printf "\n\nGenerating transitions..\n");
  let h = height t in
  let rec traverse_example t dep parent trans_acc syms_acc: transition list * symbol list =
    match t with Leaf _ -> trans_acc, syms_acc
    | Node (sym, ts) ->
      let lhs_state: state = filter is_leaf ts |> hd |> return_state in
      let rhs_states: state list = init (arity sym) (fun _ -> lhs_state) in
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
  let syms_non_example: symbol list = a |> filter (fun x -> not (mem x syms_example) && not (fst x = "()")) in
  if debug_print then (printf "\tΣ\\{Σ_ex,ε,()}: { "; syms_non_example |> iter (fun s -> Pp.pp_fst s); printf "}\n");
  let trans_non_example: transition list = syms_non_example |> map (fun s -> 
    let rhs_states': state list = let arity_s = arity s in 
    if (arity_s <> 0) then init (arity s) (fun _ -> dirchild_st) else init 1 (fun _ -> "ϵ") in
    (* TODO: Differentiate IF's conditional based on info from original TA, incorporate to Algo *)
    if (sym_equals s "IF") then root_st, (s, rhs_states') else root_st, (s, rhs_states')) in
let trans_res = trans_example @ trans_non_example
in if debug_print then (Pp.pp_transitions trans_res);
trans_res

let learner (e: tree) (a: symbol list): ta =
  let open Printf in
  let debug_print = true in
  if debug_print then (printf "\nInputs are... \n\tExample: "; Pp.pp_tree e; 
  printf "\n\tAlphabet: { "; a |> List.iter Pp.pp_symbol; printf "}\n");
  let state_ls, strt, dir_child, e' = redefine_tree e debug_print in
  let trans = gen_transitions e' a strt dir_child debug_print in
  let ta_res = { states = state_ls; alphabet = a; start_state = strt; transitions = trans } in
  if debug_print then (printf "\nLearned TA:\n"; Pp.pp_ta ta_res); ta_res




