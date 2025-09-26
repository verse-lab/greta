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

let more_than_one_transitions (s: symbol) (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) = 
  let sym_ords = sym_ord_rhs_ls |> List.fold_left (fun acc ((sym, ord), _sig_ls) -> if (syms_equals s sym) then (ord::acc) else acc) [] 
  in (List.length sym_ords) > 1

let get_transitions (oa_ls: restriction list) (op_ls: restriction list) 
  (o_bp_tbl: (int, symbol list) Hashtbl.t) (sym_lhs_ls: (symbol * state) list)
  (a: symbol list) (lvl_state_pairs: (int * state) list) (start: state) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_syms_nonterms: (symbol * state) list) 
  (opt: optimization) (debug: bool): 
  ((state * symbol), sigma list list) Hashtbl.t =
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let open Hashtbl in
  let trivial_syms = triv_syms_nonterms |> List.map fst in
  let nontrivial_syms = a |> List.filter (fun (_, rnk) -> not (rnk = 1) ) in 
  let versatile_syms = a |> List.filter (fun s -> more_than_one_transitions s sym_ord_rhs_ls) in
  (wrapped_printf "\n Trivial symbols :\n\t"; trivial_syms |> List.iter (fun s -> Pp.pp_symbol s); wrapped_printf "\n";
   wrapped_printf "\n Nontrivial symbols :\n\t"; nontrivial_syms |> List.iter (fun s -> Pp.pp_symbol s); wrapped_printf "\n";
   wrapped_printf "\n Versatile symbols :\n\t"; versatile_syms |> List.iter (fun s -> Pp.pp_symbol s); wrapped_printf "\n");
    
  let (eps_opt, paren_opt, triv_opt) = opt.eps_opt, opt.paren_opt, opt.triv_opt
  in
  let trans_tbl : ((state * symbol), sigma list list) Hashtbl.t = 
    create (length o_bp_tbl) (* size guessed wrt. # of transitions in o_bp_tbl *) in
  (* --- helpers --- *)
  let find_lhs (sym: symbol): state = 
    if debug then wrapped_printf "\n\tlooking for lhs state of symbol "; Pp.pp_symbol sym;
    let sym_sigma = 
      let correct_sym = if (sym_equals sym "LBRACE") then ("LBRACERBRACE", 1) else sym in
      sym_lhs_ls |> List.assoc_opt correct_sym in
    match sym_sigma with None -> raise No_lhs_state
    | Some st -> st 
  in 
  (* --- helper --- *)
  (* if sym specified in oas happens to have the max ord (o_m), *)
  (* then increase all other sym's of this level to (o_m + 1)   *)
  let update_orders_wrt_oa_if_necessary (sym_ord_ls: (symbol * int) list) (oas: restriction list): 
    (symbol * int) list = 
    if (List.is_empty oas) then sym_ord_ls else 
    begin 
      wrapped_printf "\n\t\t O_a not empty!";
      let syms_oa = 
        oas |> List.map (fun r -> match r with Assoc (s, _) -> s | Prec _ -> raise No_prec_possible) in
      let max_ord = 
        sym_ord_ls |> List.map snd |> List.fold_left max 0 in 
      let to_update = ref false in
        sym_ord_ls |> List.iter (fun (s', o') -> 
          if (List.mem s' syms_oa) && (o' = max_ord) then to_update := true);
      if !to_update 
      then (sym_ord_ls |> List.map (fun (sym, ord) -> 
            if (List.mem sym syms_oa) then (sym, ord) else 
            if (ord = max_ord) then (sym, (ord + 1)) else (sym, ord)) )
      else sym_ord_ls
    end
  in

  let sym_ord_ls_wrt_op: (symbol * int) list = 
    (* let sos_op = ref [] in
    o_bp_tbl |> Hashtbl.iter (fun o syms -> syms |> List.iter (fun s -> sos_op := (s, o) :: !sos_op)); !sos_op  *)
  
    op_ls |> List.map (fun x -> match x with Assoc _ -> raise No_assoc_possible | Prec (s, o) -> (s, o)) 
    |> List.fold_left cons_uniq [] |> List.rev (* removing dups here *)
    |> List.filter (fun (sym, _op) -> not (List.mem sym trivial_syms)) (* ==> Note! Remove trivial symbols here *)
  
  in
  let sym_ord_ls_wrt_op_new: (symbol * int) list = 
    update_orders_wrt_oa_if_necessary sym_ord_ls_wrt_op oa_ls
  in
    if debug then (wrapped_printf "\n\t Symbols' orders before update \n\t  "; sym_ord_ls_wrt_op |> List.iter (fun (s, o) -> 
      Pp.pp_symbol s; wrapped_printf " Ord %d  " o); wrapped_printf "\n\n";
      wrapped_printf "\n\t Symbols' orders after update \n\t  "; sym_ord_ls_wrt_op_new |> List.iter (fun (s, o) -> 
      Pp.pp_symbol s; wrapped_printf " Ord %d  " o); wrapped_printf "\n\n");
  
  let syms_op = sym_ord_ls_wrt_op_new |> List.map fst 
  in
    if debug then wrapped_printf "\n\t Syms_op is \t"; List.iter Pp.pp_symbol syms_op; wrapped_printf "\n\n";
  
  let max_lvl = 
    let max_starting_from_zero = sym_ord_ls_wrt_op_new |> List.map snd |> List.fold_left max 0 
    in max_starting_from_zero + 1
  in
  (* wrapped_printf "\n\n\t\t\t MAX IS %d\n\n" max_lvl; *)
  let lvl_state_pairs_new = 
    let update_lvl_state_pairs (lvl_st_pairs: (int * nonterminal) list) (old_max: int) (new_max: int): 
      (int * nonterminal) list = 
      let rec loop (curr: int) (upto: int) acc =
        if (curr > upto) then List.rev acc
        else (let state_new = "e" ^ (string_of_int curr) in 
              let to_add = (curr, state_new) in 
              loop (curr+1) upto (to_add::acc))
      in let lvl_stats_to_add = loop (old_max+1) new_max []
      in lvl_st_pairs @ lvl_stats_to_add
    in 
    let old_max = lvl_state_pairs |> List.map fst |> List.fold_left max 0 in 
    if (max_lvl > old_max) 
    then update_lvl_state_pairs lvl_state_pairs old_max max_lvl
    else lvl_state_pairs
  in 
  let last_state: state = 
    match (List.assoc_opt max_lvl lvl_state_pairs_new) with 
    None -> raise Max_level_state | Some st -> st 
  in 
  (* Step 1 - add last state ->_{<(), 1>} start state to 'trans_tbl' *)
  add trans_tbl (last_state, ("LPARENRPAREN", 1)) [[(T "LPAREN"); (Nt start); (T "RPAREN")]];
  (* Step 2 - add epsilon transitions *)
  for i = 1 to (max_lvl-1) do 
    begin 
      let (left_st, right_st): state * state = "e" ^ (string_of_int i), "e" ^ (string_of_int (i+1)) in 
      add trans_tbl (left_st, epsilon_symb) [[(Nt right_st)]]
    end done;
  (* Step 3 - add transitions for trivial symbols (eg, INT, BOOL) *)
  trivial_syms |> List.iter (fun sym -> let triv_state = find_lhs sym 
    in add trans_tbl (triv_state, sym) [[(T (fst sym))]]); (* [prev] (Nt epsilon_state) *)
  if debug then wrapped_printf "\n  >> After adding trivial transitions \n"; 
    Pp.pp_transitions_tbl trans_tbl;
  (* Step 4 - add transitions for nontrivial symbols level by level *)
  
  let original_order s: int = 
    let sym_ord_ls = sym_ord_rhs_ls |> List.map (fun ((s, o), _sls) -> (s, o)) in
    List.assoc s sym_ord_ls in
  let order_of_sym s = List.assoc s sym_ord_ls_wrt_op_new in 
  let max_order = sym_ord_ls_wrt_op_new |> List.map snd |> List.fold_left max min_int in
  let different_order_in_obp (s: symbol) (o': int) (curr_lvl: int) (max_o: int): bool = 
    if debug then (wrapped_printf "\n\t Sym <%s, %d> order %d " (fst s) (snd s) o');
    (* if (eps_opt = true) && (curr_lvl = max_o) && (syms_equals s epsilon_symb) then true else *)
    if (eps_opt = true) && (curr_lvl != max_o) && (syms_equals s epsilon_symb) then false else
    let ords_in_bp: int list ref = ref [] in 
    o_bp_tbl |> iter (fun o sym_ls -> if (List.mem s sym_ls) then ords_in_bp := o::!ords_in_bp); 
    if debug then (wrapped_printf " Orders of the sym in O_bp : "; !ords_in_bp |> List.iter (fun x -> wrapped_printf "%d " x));
    not (List.mem o' !ords_in_bp)
  in
  
  (* --- helper --- *)
  let _find_levels_in_op_tbl (sym: symbol) (tbl: (int, symbol list) Hashtbl.t): int list = 
    let lvls = ref [] in
    Hashtbl.iter (fun lvl sls -> if (List.mem sym sls) then lvls := (lvl::!lvls)) tbl;
    wrapped_printf "\n\t  --- For symbol "; Pp.pp_symbol sym; wrapped_printf " found "; List.iter (fun d -> wrapped_printf "%d " d) !lvls ;
    !lvls
  in

  (*  **********************************************************************  *)
  (* *** simpler and correct version of updated o_bp_table *** *)
  let updated_op_tbl: (int, symbol list) Hashtbl.t = Hashtbl.create (Hashtbl.length o_bp_tbl) in
  sym_ord_ls_wrt_op_new |> List.iter (fun (s, o) -> 
    match Hashtbl.find_opt updated_op_tbl o with 
    | Some exist_syms -> 
      if (not (List.mem s exist_syms)) 
      then Hashtbl.replace updated_op_tbl o (s::exist_syms)
    | None -> Hashtbl.add updated_op_tbl o (s::[]) 
  );
  (* ---------------------------------------------------------- *)
  if debug then (wrapped_printf "\n *** Before updating o_bp_tbl \n"; Pp.pp_obp_tbl o_bp_tbl;
  wrapped_printf "\n >> Updated O_p table \n"; Pp.pp_obp_tbl updated_op_tbl);

  (* --- helpers for this step --- *)
  let is_trivial_nonterm (x: sigma) = 
    let trivial_nonterms = 
      triv_syms_nonterms |> List.map snd |> List.append [epsilon_state] in  
      match x with T _ -> false 
      | Nt x' -> (List.mem x' trivial_nonterms)
  in
  let is_lbrace_terminal (x:sigma): bool = 
    match x with T "LBRACE" ->  true | _ -> false 
  in
  let is_cond_terminal (x:sigma): bool = 
    match x with T "IF" ->  true | _ -> false 
  in
  let lbrace_order: int = 
    let lb_ord: int ref = ref 1000 in
    Hashtbl.iter (fun o sym_ls -> if (List.mem ("LBRACE", 3) sym_ls) 
      (* [Note! G2e 00000 => 00 scenario had a start nonterminal in btw so required -1 below] *)
      then (lb_ord := o) else ()) updated_op_tbl; !lb_ord
  in
  let cond_order: int = 
    let cd_ord: int ref = ref 1000 in 
    Hashtbl.iter (fun o symls -> if ((List.mem ("IF", 4) symls) || (List.mem ("IF", 6) symls))
                   then (if o < !cd_ord then cd_ord := o else ()) else ()) updated_op_tbl; !cd_ord
  in
  let rec match_collect (sym: symbol) (ls: sigma list) (curr_st: state) (is_lbrace_trans: bool) (is_cond_trans: bool) (acc: sigma list): sigma list =
    let syms_wrt_oa = oa_ls |> List.map (fun x -> match x with 
      Assoc (s, _) -> s | Prec _ -> raise No_prec_possible) in 
    if ((arity sym) = 3) && (List.mem sym syms_wrt_oa) 
    then 
      (* TODO: Do this separately *)
      (* account for associativity *)
      (let higher_state = get_higher_state curr_st in 
       if (is_left_assoc sym oa_ls)
       then [(Nt curr_st); (T (fst sym)); (Nt higher_state)]
       else [(Nt higher_state); (T (fst sym)); (Nt curr_st)]) 
    else
      (match ls with [] -> List.rev acc
      | h :: tl -> 
        (* if h is terminal then keep, if nonterminal then replace with curr level state *)
        if (is_terminal h) then 
          (if (is_lbrace_terminal h) then match_collect sym tl curr_st true is_cond_trans (h::acc) else 
           if (is_cond_terminal h) then match_collect sym tl curr_st is_lbrace_trans true (h::acc) else 
            match_collect sym tl curr_st is_lbrace_trans is_cond_trans (h::acc))
        else if (is_trivial_nonterm h) then match_collect sym tl curr_st is_lbrace_trans is_cond_trans (h::acc)
        else if is_lbrace_trans then 
          (let corr_st = if (opt.onoff_opt ) then "e1" else "e" ^ (string_of_int lbrace_order) (* curr_st *)
           in match_collect sym tl curr_st is_lbrace_trans is_cond_trans ((Nt corr_st)::acc))
        else if is_cond_trans then 
          (let corr_st = if (opt.onoff_opt && opt.eps_opt) then "e" ^ (string_of_int cond_order) else curr_st
           in match_collect sym tl curr_st is_lbrace_trans is_cond_trans ((Nt corr_st)::acc))
        else 
          match_collect sym tl curr_st is_lbrace_trans is_cond_trans ((Nt curr_st)::acc))
  in

  let find_rhs_lst_lst (s: symbol) (o: int) (max_o: int): sigma list list = 
    (* *** NOTE! *** *)
    (* [new_new_fix] added another condition for max_o for G2e 0000 -> 00 case *)
    (* if (List.mem s versatile_syms) then (Printf.wrapped_printf "\n\t\t ALRIGHT\n\n";Utils.assoc_all s o_versatile sym_ord_rhs_ls debug) else  *)
    if (triv_opt = false) then Utils.assoc_all s o sym_ord_rhs_ls debug else 
    if (eps_opt = true) then (if (syms_equals s epsilon_symb) then 
      (if (o = max_o) then Utils.assoc_all s o sym_ord_rhs_ls debug else []) else Utils.assoc_all s o sym_ord_rhs_ls debug)
    else Utils.assoc_all s o sym_ord_rhs_ls debug
  in

  (* --- helper --- *)
  let run_for_sym_ls lvl curr_st sym_ls: ((state * symbol) * sigma list list) list = 
    let ver_sym_corr_ord_pairs = ref [] in
    sym_ls |> List.fold_left (fun acc sym -> 
      let sym_rhs_ls_ls : sigma list list = 
        if ((List.mem sym syms_op) && (different_order_in_obp sym (order_of_sym sym) lvl max_order))
        then 
          (let original_lvl = original_order sym in
          if (List.mem sym versatile_syms) then (ver_sym_corr_ord_pairs := (sym, (lvl-1))::!ver_sym_corr_ord_pairs);
          let corr_ord = 
            if (List.mem sym versatile_syms) 
            then (match List.assoc_opt sym !ver_sym_corr_ord_pairs with None -> (lvl-1) | Some o -> o) else (lvl-1) 
          in
          find_rhs_lst_lst sym original_lvl corr_ord)
        else 
          find_rhs_lst_lst sym lvl max_order
      in
      let sym_rhs_lsls_learned: sigma list list = 
        if ((List.mem sym syms_op) && (different_order_in_obp sym (order_of_sym sym) lvl max_order))
        then 
          (if debug then (wrapped_printf "\n Different? *** Part of Syms_O_p \t"; Pp.pp_symbol sym;
            wrapped_printf "\n Found rhs sigma ls ls "; Pp.pp_sigma_listlist sym_rhs_ls_ls);
           sym_rhs_ls_ls |> List.fold_left (fun acc rhs_ls ->
           let new_lvl = List.assoc sym sym_ord_ls_wrt_op_new in 
           let new_st = "e" ^ (string_of_int (new_lvl+1)) in
           let sym_rhs_ls_learned = match_collect sym rhs_ls new_st false false []
            in 
           sym_rhs_ls_learned :: acc) [])
        else 
          (if debug then (wrapped_printf "\n Not different OR not part of Syms_op OR epsilon symb\t"; Pp.pp_symbol sym;
            wrapped_printf "\n Rhs sigma ls ls "; Pp.pp_sigma_listlist sym_rhs_ls_ls);
            sym_rhs_ls_ls |> List.fold_left (fun acc rhs_ls -> 
          let sym_rhs_ls_learned = match_collect sym rhs_ls curr_st false false []
          in sym_rhs_ls_learned :: acc ) [])
      in
      (wrapped_printf "\n\t\t Rhs sigma lsls learned\n\t\t";
      sym_rhs_lsls_learned |> List.iter (fun sym_rhs_ls -> Pp.pp_sigma_list2 sym_rhs_ls); wrapped_printf "\n");
      ((curr_st, sym), sym_rhs_lsls_learned):: acc      
    ) []
  in
  (* ---------------------------------------------------------- *)
  let rec run_for_each_level lvl (acc: ((state * symbol) * sigma list list) list): 
    ((state * symbol) * sigma list list) list =
    if (lvl = max_lvl) then acc
    else 
      begin 
        if debug then wrapped_printf "\n\n\t >> Now considering level %i >> \n" (lvl+1);
        (* Collect nontrivial symbols per level [note: lvl starts from 0 to max-1] *)
        let sym_ls : symbol list = 
          let init_sym_ls = List.flatten (find_all updated_op_tbl lvl) in 
          optimize_sym_list_new init_sym_ls eps_opt paren_opt lvl max_order debug
        in
          (wrapped_printf "\n\t >> For level %d Length of syms is %i ---> " (lvl+1) (List.length sym_ls);
          sym_ls |> List.iter Pp.pp_symbol);
          let curr_st = "e" ^ (string_of_int (lvl+1)) in 
            let to_acc = sym_ls |> run_for_sym_ls lvl curr_st in 
            run_for_each_level (lvl+1) (to_acc @ acc)
      end
  in let learned_lhsst_sym_siglsls = run_for_each_level 0 [] 
  in learned_lhsst_sym_siglsls |> List.iter (fun ((lhs, sym), siglsls) -> 
    wrapped_printf "\n\tFor LHS %s " lhs; Pp.pp_symbol sym; 
    (* if List.is_empty siglsls then wrapped_printf "\n\tEMPTY!\n"; *)
    siglsls |> List.iter (fun sigls -> Pp.pp_sigma_list2 sigls));
    wrapped_printf "\n\n";
    wrapped_printf "\n\t\t Length is %d\n\n" (List.length learned_lhsst_sym_siglsls);
  let new_trans_tbl = Hashtbl.create 100 in 
    learned_lhsst_sym_siglsls |> List.iter (fun ((lhs, sym), siglsls) -> 
      if not (List.is_empty siglsls) then add new_trans_tbl (lhs, sym) siglsls);
  (* NOTE: 
   *       Merge trans_tbl and new_trans_tbl so that both trvi transitions and nontriv transitions
   *       are combined. If I just work with only one trans_tbl and try adding everything there, 
   *       it gets stuck in some inifinite loop. This is a work around. 
   *)
  let final_trans = Utils.merge ~into:new_trans_tbl trans_tbl in
  (* Now go through 'sym_ord_ls_wrt_op_new' and update trans_tbl *)
  if debug then wrapped_printf "\n  >> After adding nontrivial transitions \n"; 
    Pp.pp_transitions_tbl final_trans;
  final_trans

let learn_ta (example_trees: (string list * tree * (bool * bool) * restriction list) list) (o_bp_tbl: (int, symbol list) Hashtbl.t) 
  (sym_state_ls: (symbol * state) list) (a: symbol list) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_syms_nonterms: (symbol * state) list) 
  (opt: optimization) (debug_print: bool): ta2 = 
  let wrapped_printf fmt =
    if debug_print then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in
  
  let o_bp: restriction list = Hashtbl.fold (fun o syms acc -> 
    let to_add = syms |> List.fold_left (fun acc' s -> (Prec (s, o))::acc') [] in to_add @ acc) o_bp_tbl [] 
  in
  let oa_ls: restriction list = collect_oa_restrictions example_trees debug_print in 
  let o_tmp: restriction list = collect_op_restrictions example_trees debug_print in 
  let op_ls: restriction list = combine_op_restrictions_in_pairs o_bp o_tmp debug_print in 
    
  ( 
   (wrapped_printf "\n\nLearn a tree automaton based on:\n";
    wrapped_printf "\n O_a list: \n\t"; Pp.pp_restriction_lst oa_ls; 
    wrapped_printf "\n O_p list: \n\t"; Pp.pp_restriction_lst op_ls; 
    (* wrapped_printf "\nO_a list :\n\t"; oa_ls |> List.iter (fun r -> match r with Assoc (s, a) -> Pp.pp_symbol s; wrapped_printf " assoc %s" a | Prec _ -> ()); wrapped_printf "\n";
    wrapped_printf "\nO_p list :\n\t"; op_ls |> List.iter (fun r -> match r with Prec (s, o) -> Pp.pp_symbol s; wrapped_printf " order %d" o | Assoc _ -> ()); wrapped_printf "\n"; *)
    wrapped_printf "\n Alphabet: { "; a |> List.iter Pp.pp_symbol; wrapped_printf "}\n")
  );
  
  let (lvl_state_pairs, init_state): (int * state) list * state = get_states op_ls in
  let state_ls: state list = lvl_state_pairs |> List.map snd in
  let raw_trans_ls: ((state * symbol), sigma list list) Hashtbl.t = 
    get_transitions oa_ls op_ls o_bp_tbl sym_state_ls a lvl_state_pairs init_state sym_ord_rhs_ls triv_syms_nonterms opt debug_print in
  (* let ordered_trans_ls = order_trans_ls state_ls raw_trans_ls in *)
  let ta_res: ta2 = { states = state_ls; alphabet = a; start_states = [init_state]; 
  transitions = raw_trans_ls; trivial_sym_nts=triv_syms_nonterms } in 
  wrapped_printf "\n\nLearned TA:\n"; Pp.pp_ta2 ta_res; ta_res


