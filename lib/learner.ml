open Ta

(** height : find the height (maximum depth) of tree *)
let height (e: tree): int =
  let max_ls ls = List.fold_left max 0 ls in 
  let rec loop t acc = match t with 
    | Leaf _ -> acc
    | Node (_, ts) -> ts
      |> List.map (fun x -> loop x (acc+1)) 
      |> max_ls
  in loop e 0

(** redefine_tree : generate states, start_state, redefine e as e' wrt these states *)
let redefine_tree (e: tree) (debug_print: bool): state list * state * tree =
  if debug_print then (Printf.printf "\nRedefining tree.. \n\tInput: "; Pp.pp_tree e); 
  let states_res, start_res = ref [], ref "" in
  let rec loop e dep =
    match e with
    | Leaf s ->
      (* gen states so it can differentiate cond_expr from expr *)
      let s' = (String.capitalize_ascii s) ^ "_" ^ string_of_int dep in
      (* update the start state accordingly *)
      if (dep = 1) then start_res := s' else
      if (dep = 0) then (start_res := s'; Printf.printf "Trivial tree with only 1 leaf");
      if not (List.mem s' !states_res) then states_res := s' :: !states_res; Leaf s'
    | Node (sym, ts) ->
      let ts' = ts |> List.map (fun x-> loop x (dep + 1)) in
      Node (sym, ts')
  in let e' = loop e 0
  in if debug_print then (Printf.printf "\n\tRedefined: "; Pp.pp_tree e');
  !states_res, !start_res, e'

(** gen_transitions : generate transitions based on example tree and symbols *)
let gen_transitions (t: tree) (a: symbol list) (debug_print: bool): transition list =
  if debug_print then (Printf.printf "\n\nGenerating transitions..\n");
  let (*rec*) traverse_example t (*dep*) trans_acc syms_acc: transition list * symbol list =
    (* \Sigma_ex, ε, () transitions per parent-children *)
    match t with Leaf _ -> List.rev trans_acc, List.rev syms_acc
    | Node (sym, ts) ->
      let lhs_state: state = List.filter is_leaf ts |> List.hd |> return_state in
      let rhs_states: state list = List.init (arity sym) (fun _ -> lhs_state) in
      let tran_sym = lhs_state, (sym, rhs_states) in
      let tran_eps = lhs_state, (("ε", 1), []) in
      (* let tran_paren =  in *)
      (tran_sym :: tran_eps :: trans_acc), (sym :: syms_acc)
  in let (trans_example, syms_example) = traverse_example t (*0*) [] [] in
  let (*syms_non_example*)_: symbol list = a |> List.filter (fun x -> not (List.mem x syms_example)) in
  let trans_non_example = [] (*syms_non_example |> List.map ()*)
in let trans_res = trans_example @ trans_non_example
in if debug_print then (Pp.pp_transitions trans_res);
trans_res

let learner (e: tree) (a: symbol list): ta =
  let open Printf in
  let debug_print = true in
  if debug_print then (printf "\nInputs are... \n\tExample: "; Pp.pp_tree e; 
  printf "\n\tAlphabet: { "; a |> List.iter Pp.pp_symbol; printf "}\n");
  let state_ls, strt, e' = redefine_tree e debug_print in
  let trans = gen_transitions e' a debug_print in
  let ta_res = { states = state_ls; alphabet = a; start_state = strt; transitions = trans } in
  if debug_print then (printf "\nLearned TA:\n"; Pp.pp_ta ta_res); ta_res




