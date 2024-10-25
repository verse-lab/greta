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

let get_transitions (oa_ls: restriction list) (op_ls: restriction list) 
  (o_bp_tbl: (int, symbol list) Hashtbl.t) (sym_lhs_ls: (symbol * state) list)
  (a: symbol list) (lvl_state_pairs: (int * state) list) (start: state) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_nonterms: (symbol * state) list) (debug: bool): 
  ((state * symbol), sigma list list) Hashtbl.t =
  let open Printf in
  let open Hashtbl in
  let trivial_syms = a |> List.filter (fun (_, rnk) -> (rnk = 0) ) in
  let nontrivial_syms = a |> List.filter (fun (_, rnk) -> not (rnk = 0) )
  in
  (* printf "\nO_bp Table\n"; Pp.pp_obp_tbl o_bp_tbl; *)
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
  (* printf "\n\nMAX LEVEL IS >> %d\n\n" max_lvl; *)
  let last_state: state = 
    match (List.assoc_opt max_lvl lvl_state_pairs) with 
    None -> raise Max_level_state | Some st -> st in 
  (* *** printing for debugging *** *)
  (* 
  printf "\n\n\nWHAT THE...?!\n\n"; sym_ord_rhs_ls 
  |> List.iter (fun ((s, i), sigls) -> Pp.pp_symbol s; printf "order %d --> " i; sigls |> Pp.pp_sigma_list2; printf "\n");
  *)
  let find_rhs_lst_lst (s: symbol) (o: int): sigma list list = Utils.assoc_all s o sym_ord_rhs_ls debug in
  (*   
  let is_terminal (x: sigma) = 
    match x with T _ -> true | Nt _ -> false
  in *)
  
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
  
  (* --- helpers for this step --- *)
  let is_trivial_nonterm (x: sigma) = 
    let trivial_nonterms = 
      (* *** (debugging) *** recent fix! *)
      triv_nonterms |> List.map snd |> List.append [epsilon_state] in  
      (* 
      printf "\nWHAT ARE TRIV NONTERMS??\n"; trivial_nonterms |> Pp.pp_states; 
      *)
      match x with T _ -> false 
      | Nt x' -> (List.mem x' trivial_nonterms)
  in
  
  let rec match_collect (sym: symbol) (ls: sigma list) (curr_st: state) (acc: sigma list): sigma list =
    let syms_wrt_oa = oa_ls |> List.map (fun x -> match x with 
      Assoc (s, _) -> s | Prec _ -> raise No_prec_possible) in 
    if ((arity sym) = 2) && (List.mem sym syms_wrt_oa) 
    then 
      (* TODO: Do this separately *)
      (* account for associativity *)
      (let higher_state = get_higher_state curr_st in 
       if (is_left_assoc sym oa_ls)
       then [(Nt higher_state); (T (fst sym)); (Nt curr_st)]
       else [(Nt curr_st); (T (fst sym)); (Nt higher_state)]) 
    else
      (match ls with [] -> List.rev acc
      | h :: tl -> 
        (* if h is terminal then keep, if nonterminal then replace with curr level state *)
        if (is_terminal h) then match_collect sym tl curr_st (h::acc) (* [fix!!] condition :  || (sigmas_equal h (Nt epsilon_state)) *)
        else if (is_trivial_nonterm h) then match_collect sym tl curr_st (h::acc)
        else
          match_collect sym tl curr_st ((Nt curr_st)::acc))
  in
 
  let sym_ord_ls_wrt_op: (symbol * int) list = op_ls |> List.map (fun x -> match x with 
    Assoc _ -> raise No_assoc_possible | Prec (s, o) -> (s, o)) in
  
  let syms_op = sym_ord_ls_wrt_op |> List.map fst in
  let original_order s: int = 
    let sym_ord_ls = sym_ord_rhs_ls |> List.map (fun ((s, o), _sls) -> (s, o)) in
    List.assoc s sym_ord_ls in
  let order_of_sym s = List.assoc s sym_ord_ls_wrt_op in 
  let different_order_in_obp (s: symbol) (o': int): bool = 
    if (syms_equals s epsilon_symb) then false else
    let ord_in_bp: int ref = ref 999 in (* some random initial number *)
    o_bp_tbl |> iter (fun o sym_ls -> if (List.mem s sym_ls) then ord_in_bp := o); 
    !ord_in_bp != o'
  in
  

  (* --- helper --- *)
  let _find_levels_in_op_tbl (sym: symbol) (tbl: (int, symbol list) Hashtbl.t): int list = 
    let lvls = ref [] in
    Hashtbl.iter (fun lvl sls -> if (List.mem sym sls) then lvls := (lvl::!lvls)) tbl;
    printf "\n\t  --- For symbol "; Pp.pp_symbol sym; printf " found "; List.iter (fun d -> printf "%d " d) !lvls ;
    !lvls
  in

  (*  **********************************************************************  *)
  (* 
  (* Something needs to be done here!  *)
  let new_op_tbl: (int, symbol list) Hashtbl.t = Hashtbl.copy o_bp_tbl in 
  (* update op_tbl based on op_ls *)  
  sym_ord_ls_wrt_op |> List.iter (fun (s, ord) -> 
    printf "\n\t *** (debugging) For symbol!"; Pp.pp_symbol s;
    (* 
    
    let sym_lvls = find_levels_in_op_tbl s new_op_tbl in
    if (List.length sym_lvls) = 1 
    then 
      (let lvl = List.hd sym_lvls 
       in if (lvl != ord) 
        then ())
    else ();
     *)
    (* quick fix for below *)
    let _epsilon_sym (sym: symbol) = match sym with (s, _) -> s = (fst epsilon_symb)
    in 
    (* first remove the symbol from the existing hashtbl if ord != lvl *)
    new_op_tbl |> Hashtbl.iter (fun lvl sls' -> 
      if (List.mem s sls') && (lvl != ord) 
      then 
        (let new_sls' = List.filter (fun x -> not (syms_equals s x)) sls' 
        in Hashtbl.replace new_op_tbl lvl new_sls';
        (* then add the symbol corresponding to the new order *)
        let existing = match Hashtbl.find_opt new_op_tbl ord with Some ls -> ls | None -> [] in
        let new_sym_ls = s :: existing in
        Hashtbl.replace new_op_tbl ord new_sym_ls)
      else 
        (printf "\n\t --- For the symbol "; Pp.pp_symbol s; 
        printf "\n\t --- not (a member of syms for order %d) || (level %d == order %d)" ord lvl ord)
    ));
 *)
  (*  **********************************************************************  *)
  (* *** simpler and correct version of updated o_bp_table *** *)
  let updated_op_tbl: (int, symbol list) Hashtbl.t = Hashtbl.create (Hashtbl.length o_bp_tbl) in
  sym_ord_ls_wrt_op |> List.iter (fun (s, o) -> 
    match Hashtbl.find_opt updated_op_tbl o with 
    | Some exist_syms -> 
      if (not (List.mem s exist_syms)) 
      then Hashtbl.replace updated_op_tbl o (s::exist_syms)
    | None -> Hashtbl.add updated_op_tbl o (s::[]) 
  );
  (* ---------------------------------------------------------- *)
  (* *** debug *** *)
  if debug then (printf "\n *** Before updating o_bp_tbl \n"; Pp.pp_obp_tbl o_bp_tbl;
  (* printf "\n *** After updating new_op_tbl \n"; Pp.pp_obp_tbl new_op_tbl; *)
  printf "\n >>> WHat about this?! \n"; Pp.pp_obp_tbl updated_op_tbl);

  (* --- helper --- *)
  let run_for_sym_ls lvl curr_st sym_ls: ((state * symbol) * sigma list list) list = 
    sym_ls |> List.fold_left (fun acc sym -> 
      let sym_rhs_ls_ls : sigma list list = 
        if ((List.mem sym syms_op) && (different_order_in_obp sym (order_of_sym sym)))
        then 
          (let original_lvl = original_order sym in
          find_rhs_lst_lst sym original_lvl)
        else 
          find_rhs_lst_lst sym lvl
      in
      let sym_rhs_lsls_learned: sigma list list = 
        if ((List.mem sym syms_op) && (different_order_in_obp sym (order_of_sym sym)))
        then 
          (printf "\n Different? *** Part of Syms_O_p \t"; Pp.pp_symbol sym;
          sym_rhs_ls_ls |> List.fold_left (fun acc rhs_ls ->
          let new_lvl = List.assoc sym sym_ord_ls_wrt_op in 
          let new_st = "e" ^ (string_of_int (new_lvl+1)) in
          let sym_rhs_ls_learned = match_collect sym rhs_ls new_st []
          in 
          sym_rhs_ls_learned :: acc) [])
        else 
          (sym_rhs_ls_ls |> List.fold_left (fun acc rhs_ls -> 
          let sym_rhs_ls_learned = match_collect sym rhs_ls curr_st []
          in sym_rhs_ls_learned :: acc ) [])
      in
      (printf "\n\t\t Rhs sigma lsls learned\n\t\t";
      sym_rhs_lsls_learned |> List.iter (fun sym_rhs_ls -> Pp.pp_sigma_list2 sym_rhs_ls); printf "\n");
      ((curr_st, sym), sym_rhs_lsls_learned):: acc
      (* if debug then printf "\n\tAdding transition for (State %s, " curr_st; Pp.pp_symbol sym; printf ")";
      if ((List.mem sym syms_op) && (different_order_in_obp sym (order_of_sym sym)))
      then 
        (let _old_st = curr_st in
        let new_lvl = List.assoc sym sym_ord_ls_wrt_op in 
        let new_st = "e" ^ (string_of_int (new_lvl+1)) in
        add trans_tbl (new_st, sym) sym_rhs_lsls_learned)
      else
        (add trans_tbl (curr_st, sym) sym_rhs_lsls_learned) *)      
    ) []
  in
  (* ---------------------------------------------------------- *)
  let rec run_for_each_level lvl (acc: ((state * symbol) * sigma list list) list): 
    ((state * symbol) * sigma list list) list =
    if (lvl = max_lvl) then acc
    else 
      begin 
        if debug then printf "\n\n\t >> Now considering level %i >> \n" (lvl+1);
        (* Collect nontrivial symbols per level [note: lvl starts from 0 to max-1] *)
        let sym_ls : symbol list = 
          List.flatten (find_all updated_op_tbl lvl)    (* *** (debugging) new_op_tbl *)
        in
          (printf "\n\t >> For level %d Length of syms is %i ---> " (lvl+1) (List.length sym_ls);
          sym_ls |> List.iter Pp.pp_symbol);
          let curr_st = "e" ^ (string_of_int (lvl+1)) in 
            let to_acc = sym_ls |> run_for_sym_ls lvl curr_st in 
            run_for_each_level (lvl+1) (to_acc @ acc)
      end
  in let learned_lhsst_sym_siglsls = run_for_each_level 0 [] 
  in printf "\n\t\t HERE! \n\n"; learned_lhsst_sym_siglsls |> List.iter (fun ((lhs, sym), siglsls) -> 
    printf "\n\tFor LHS %s " lhs; Pp.pp_symbol sym; siglsls |> List.iter (fun sigls -> Pp.pp_sigma_list2 sigls));
    printf "\n\n";
    printf "\n\t\t LENGTH IS %d\n\n" (List.length learned_lhsst_sym_siglsls);
  let rec triv_eps_trans i (acc: ((state * symbol) * sigma list list) list) = 
    if i > max_lvl - 1 then acc
    else 
      (let (left_st, right_st) = "e" ^ (string_of_int i), "e" ^ (string_of_int (i+1)) in
       let to_acc = (left_st, epsilon_symb), [[(Nt right_st)]] 
       in triv_eps_trans (i+1) (to_acc::acc))
  in 
  let triv_eps_trans = triv_eps_trans 1 [] in 
  printf "\n\n\t\tTRIVIAL STATE TRANSITIONS\n\n";
  triv_eps_trans |> List.iter (fun ((lhs, sym), siglsls) -> printf "LHS state %s " lhs; 
  Pp.pp_symbol sym; siglsls |> List.iter (fun sigls -> Pp.pp_sigma_list2 sigls));
  let new_trans_tbl = Hashtbl.create 100 in 
    learned_lhsst_sym_siglsls |> List.iter (fun ((lhs, sym), siglsls) -> 
      add new_trans_tbl (lhs, sym) siglsls);
  (* NOTE: 
   *       Merge trans_tbl and new_trans_tbl so that both trvi transitions and nontriv transitions
   *       are combined. If I just work with only one trans_tbl and try adding everything there, 
   *       it gets stuck in some inifinite loop. This is a work around. 
   *)
  let final_trans = Utils.merge ~into:new_trans_tbl trans_tbl in
    (* triv_eps_trans |> List.iter (fun ((lhs, sym), siglsls) -> 
      add new_trans_tbl (lhs, sym) siglsls); *)
  (* Now go through sym_ord_ls_wrt_op and update trans_tbl *)
  if debug then printf "\n  >> After adding nontrivial transitions \n"; 
    Pp.pp_transitions_tbl final_trans;
  final_trans

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
  let ta_res: ta2 = { states = state_ls; alphabet = a; start_states = [init_state]; 
  transitions = raw_trans_ls; trivial_sym_nts=[] } in 
  printf "\n\nLearned TA:\n"; Pp.pp_ta2 ta_res; ta_res


