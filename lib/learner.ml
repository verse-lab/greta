open Ta
open Treeutils
open Cfg

exception No_state_for_sym_order
exception Max_level_state
exception No_lhs_state

let get_states (op_ls: restriction list): ((int * state) list) * state = 
  let default_states = [(0, "C"); (-1, "ϵ")] in
  let gen_states: (int * state) list = 
    let num_levels = levels_in_op_ls op_ls in
    let rec gen_loop lvl curr_idx acc =
      if (lvl = 0) then List.rev acc
      else (let new_state = "e" ^ (string_of_int curr_idx)
            in gen_loop (lvl-1) (curr_idx+1) ((curr_idx, new_state)::acc))
    in gen_loop num_levels 1 []
  in
  (gen_states @ default_states), (List.hd gen_states |> snd)

let get_transitions (_oa_ls: restriction list) (_op_ls: restriction list) 
  (o_bp_tbl: (int, symbol list) Hashtbl.t) (sym_lhs_ls: (symbol * state) list)
  (a: symbol list) (lvl_state_pairs: (int * state) list) (start: state) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_nonterms: (symbol * state) list) (debug: bool): 
  ((state * symbol), sigma list list) Hashtbl.t =
  let open Printf in
  let open Hashtbl in
  let trivial_syms = a |> List.filter (fun (_, rnk) -> (rnk = 0) ) in
  let nontrivial_syms = a |> List.filter (fun (_, rnk) -> not (rnk = 0) )
  in
  (if debug then 
    printf "\nTrivial symbols :\n\t"; trivial_syms |> List.iter (fun s -> Pp.pp_symbol s); printf "\n";
    printf "\nNontrivial symbols :\n\t"; nontrivial_syms |> List.iter (fun s -> Pp.pp_symbol s)); printf "\n";
  let trans_tbl : ((state * symbol), sigma list list) Hashtbl.t = 
    create (length o_bp_tbl) (* size guessed wrt. # of transitions in o_bp_tbl *) in
  (* --- helpers --- *)
  let find_lhs (sym: symbol): state = 
    if debug then printf "\n\tlooking for lhs state of symbol "; Pp.pp_symbol sym;
    let sym_sigma = 
      let correct_sym = if (sym_equals sym "LBRACE") then ("LBRACERBRACE", 1) else sym in
      sym_lhs_ls |> List.assoc_opt correct_sym in
    match sym_sigma with None -> raise No_lhs_state
    | Some st -> st in 
  let max_lvl = 
    lvl_state_pairs |> List.map fst |> List.fold_left max 1 in
  let last_state: state = 
    match (List.assoc_opt max_lvl lvl_state_pairs) with 
    None -> raise Max_level_state | Some st -> st in 
  let find_rhs_lst_lst (s: symbol) (o: int): sigma list list = Utils.assoc_all s o sym_ord_rhs_ls debug in
  let is_terminal (x: sigma) = 
    match x with T _ -> true | Nt _ -> false
  in
  (* --------------- *)
  (* Step 1 - add last state ->_{<(), 1>} start state to 'trans_tbl' *)
  add trans_tbl (last_state, ("LPARENRPAREN", 1)) [[(Nt start)]];
  (* Step 2 - add epsilon transitions *)
  for i = 1 to (max_lvl-1) do 
    begin 
      let (left_st, right_st): state * state = "e" ^ (string_of_int i), "e" ^ (string_of_int (i+1)) in 
      add trans_tbl (left_st, epsilon_symb) [[(Nt right_st)]]
    end done;
  (* Step 3 - add transitions for trivial symbols (eg, INT, BOOL) *)
  trivial_syms |> List.iter (fun sym -> let triv_state = find_lhs sym 
    in add trans_tbl (triv_state, sym) [[(Nt epsilon_state)]]);
  if debug then printf "\n  >> After adding trivial transitions \n"; 
    Pp.pp_transitions_tbl trans_tbl;
  (* Step 4 - add transitions for nontrivial symbols level by level *)
  let is_trivial_nonterm (x: sigma) = 
    let trivial_nonterms = triv_nonterms |> List.map snd in
      match x with T _ -> false 
      | Nt x' -> (List.mem x' trivial_nonterms)
  in
  let rec match_collect (ls: sigma list) (curr_st: state) (acc: sigma list): 
    sigma list =
    match ls with [] -> List.rev acc
    | h :: tl -> 
      (* if h is terminal then keep, if nonterminal then replace with curr level state *)
      if (is_terminal h) || (sigmas_equal h (Nt epsilon_state)) then match_collect tl curr_st (h::acc)
      else if (is_trivial_nonterm h) then match_collect tl curr_st (h::acc)
      else
        match_collect tl curr_st ((Nt curr_st)::acc)
  in
  let rec run_for_each_level lvl: unit =
    if (lvl <= max_lvl-1)
    then 
      (if debug then printf "\n\n\t >> Now considering level %i >> \n" (lvl+1);
      let sym_ls_ls : symbol list list = find_all o_bp_tbl lvl in 
      printf "\n\t >> Length of syms --> %i" (List.length sym_lhs_ls);
      let curr_st = "e" ^ (string_of_int (lvl+1)) in
      let run_for_sym_ls ls = 
        ls |> List.iter (fun sym -> 
        let sym_rhs_ls_ls : sigma list list = find_rhs_lst_lst sym lvl in
        let sym_rhs_lsls_learned = 
          sym_rhs_ls_ls |> List.fold_left (fun acc rhs_ls -> 
          let sym_rhs_ls_learned = match_collect rhs_ls curr_st []
          in sym_rhs_ls_learned :: acc ) [] 
        in
          if debug then printf "\n\tAdding transition for (State %s, " curr_st; 
          Pp.pp_symbol sym; printf ")";
          add trans_tbl (curr_st, sym) sym_rhs_lsls_learned) 
      in 
        sym_ls_ls |> List.iter run_for_sym_ls;
        run_for_each_level (lvl+1))
      else 
        printf "done"
  in run_for_each_level 0;
  if debug then printf "\n  >> After adding nontrivial transitions \n"; 
    Pp.pp_transitions_tbl trans_tbl;
  (* 
  let rec gen_trans_nontrivials lvl acc = 
    if debug then printf "\n >> Gen transitions at level %i" lvl;
    if lvl > max_lvl then acc 
    else 
      (* use o_bp_tbl to look up relevant symbols in curr lvl *)
      let sym_ls = Hashtbl.find o_bp_tbl lvl in 
      let curr_transitions = sym_ls |> List.fold_left (fun sym_acc s -> 
        (gen_transition_for_symbol lvl s)::sym_acc) [] in
      gen_trans_nontrivials (lvl+1) acc @ curr_transitions 
  in
  let trans_nontrivials = gen_trans_nontrivials 1 [] in
   
  let get_sym_state (s: symbol) = 
    let sym_order = (order_in_op_lst s op_ls) + 1 in
    match (List.assoc_opt sym_order lvl_state_pairs) with 
    | None -> raise No_state_for_sym_order | Some st -> st 
  in
  let rec _gen_trans_nontrivials_prev sym_ls acc: transition list =
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
   *)
  trans_tbl

let learn_ta (oa_ls: restriction list) (op_ls: restriction list) (o_bp_tbl: (int, symbol list) Hashtbl.t) 
  (sym_state_ls: (symbol * state) list) (a: symbol list) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_nonterms: (symbol * state) list) (debug_print: bool): ta2 = 
  let open Printf in 
  if debug_print then (printf "\n\nLearn a tree automaton based on:\n\tO_a: ";
  Pp.pp_restriction_lst oa_ls; printf "\n\tO_p: "; Pp.pp_restriction_lst op_ls; 
  printf "\n\tAlphabet: { "; a |> List.iter Pp.pp_symbol; printf "}\n");
  let (lvl_state_pairs, init_state): (int * state) list * state = get_states op_ls in
  let state_ls: state list = lvl_state_pairs |> List.map snd in
  let raw_trans_ls: ((state * symbol), sigma list list) Hashtbl.t = 
    get_transitions oa_ls op_ls o_bp_tbl sym_state_ls a lvl_state_pairs init_state sym_ord_rhs_ls triv_nonterms debug_print in
  (* let ordered_trans_ls = order_trans_ls state_ls raw_trans_ls in *)
  let ta_res: ta2 = { states = state_ls; alphabet = a; start_state = init_state; 
  transitions = raw_trans_ls; trivial_sym_nts=[] } in 
  printf "\n\nLearned TA:\n"; Pp.pp_ta2 ta_res; ta_res


