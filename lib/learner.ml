open Ta
open Treeutils
(* open Cfg *)

exception No_state_for_sym_order
exception Max_level_state
exception No_lhs_state

let wrapped_printf debug fmt =
  if debug then Printf.printf fmt
  else Printf.ifprintf stdout fmt

let learn_oa_pos (tree_examples: (string list * tree * (bool * bool * bool) * restriction list) list) (debug_print: bool): 
  restriction list = 
  let oa_res = 
    tree_examples |> List.fold_left (fun acc (_, _, (oa_pos, _oa_neg, _), rls) -> if oa_pos then rls @ acc else acc) [] in 
  if debug_print then (wrapped_printf debug_print "\n  Collected O_a positives : "; Pp.pp_restriction_lst oa_res); 
  oa_res

let learn_oa_neg (tree_examples: (string list * tree * (bool * bool * bool) * restriction list) list) (debug_print: bool): 
  restriction list = 
  let oa_res = 
    tree_examples |> List.fold_left (fun acc (_, _, (_oa_pos, oa_neg, _), rls) -> if oa_neg then rls @ acc else acc) [] in 
  if debug_print then (wrapped_printf debug_print "\n  Collected O_a negatives : "; Pp.pp_restriction_lst oa_res); 
  oa_res


let learn_op (o_bp_tbl: (int, symbol list) Hashtbl.t) (tree_examples: (string list * tree * (bool * bool * bool) * restriction list) list) 
  (debug_print: bool): (int, symbol list) Hashtbl.t = 
  let open List in
  let op_related_ls: restriction list list = 
    tree_examples |> filter (fun (_sls, _t, (_, _, op), _rls) -> op) |> map (fun (_sls, _t, (_, _, _), rls) -> rls)
  in 
  let o_tmp_ls: (restriction * restriction) list = 
    op_related_ls |> map (fun rls -> if (length rls) = 2 
      then (nth rls 0), (nth rls 1) else raise (Failure "op_relatd_ls should contain only 2 restrictions"))
  in 
  if debug_print then (wrapped_printf debug_print "\n\t O_tmp pair list:\n\t"; 
    o_tmp_ls |> List.iter (fun (r1, r2) -> Pp.pp_restriction r1; Pp.pp_restriction r2; wrapped_printf debug_print "\n\t"); 
    wrapped_printf debug_print "\n\n");
  let res_op_tbl: (int, symbol list) Hashtbl.t = 
    o_tmp_ls |> fold_left (fun op_tbl_acc (r1, r2) -> 
      begin 
        let sym1 = sym_of_restrction r1 in (* r1 and r2 symbols should have same order *)
        let sym2 = sym_of_restrction r2 in
        let orders_ls: int list = orders_of_sym_in_op_tbl sym1 sym2 op_tbl_acc debug_print in 
        if (List.length orders_ls) = 1 
        then 
          begin 
            let ord: int = orders_ls |> hd in 
            let sym_top, sym_bot = sym_top_sym_bot_of_restrictions r1 r2 debug_print in
            update_op_tbl_per_syms sym_top sym_bot ord op_tbl_acc debug_print
          end
        else 
          (* If there are more than 1 orders for these symbols, then run in reverse order *)
          op_tbl_acc
      end
    ) o_bp_tbl in
  res_op_tbl

(* (* let op_ls: restriction list = combine_op_restrictions_in_pairs o_bp o_tmp debug_print in  *)
  let op_ls: restriction list = combine_op_restrictions_new o_bp o_tmp sts_order_syms_lsls debug_print in  
  *)
  

(* 
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
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_syms_nonterms: (symbol * state) list) 
  (sts_order_syms_lsls: ((int * state) * symbol list) list) (debug: bool): 
  ((state * symbol), sigma list list) Hashtbl.t =
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in
  
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
  (* Step 1 - add last state ->_{<LPAREN, 3>} for <(), 3> start state to 'trans_tbl' *)
  add trans_tbl (last_state, ("LPAREN", 3)) [[(T "LPAREN"); (Nt start); (T "RPAREN")]];
  (* Step 2 - add epsilon transitions *)
  for i = 1 to (max_lvl-1) do 
    begin 
      let (left_st, right_st): state * state = "e" ^ (string_of_int i), "e" ^ (string_of_int (i+1)) in 
      add trans_tbl (left_st, epsilon_symb) [[(Nt right_st)]]
    end done;
  (* Step 3 - add transitions for trivial symbols (eg, INT, BOOL) *)
  trivial_syms |> List.iter (fun sym -> let triv_state = find_lhs sym 
    in add trans_tbl (triv_state, sym) [[(T (fst sym))]]); (* [prev] (Nt epsilon_state) *)
  if debug then wrapped_printf "\n  --->> After adding trivial transitions \n"; 
    Pp.pp_transitions_tbl trans_tbl;
  (* Step 4 - add transitions for nontrivial symbols level by level *)
  
  let original_order s: int = 
    let sym_ord_ls = sym_ord_rhs_ls |> List.map (fun ((s, o), _sls) -> (s, o)) in
    List.assoc s sym_ord_ls in
  let order_of_sym s = List.assoc s sym_ord_ls_wrt_op_new in 
  let max_order = sym_ord_ls_wrt_op_new |> List.map snd |> List.fold_left max min_int in
 
  let updated_op_tbl: (int, symbol list) Hashtbl.t = Hashtbl.create (Hashtbl.length o_bp_tbl) in
  sym_ord_ls_wrt_op_new |> List.iter (fun (s, o) -> 
    match Hashtbl.find_opt updated_op_tbl o with 
    | Some exist_syms -> 
      if (not (List.mem s exist_syms)) 
      then Hashtbl.replace updated_op_tbl o (s::exist_syms)
    | None -> Hashtbl.add updated_op_tbl o (s::[]) 
  );
  
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
  
  let final_trans = Utils.merge ~into:new_trans_tbl trans_tbl in
  (* Now go through 'sym_ord_ls_wrt_op_new' and update trans_tbl *)
  if debug then wrapped_printf "\n  >> After adding nontrivial transitions \n"; 
    Pp.pp_transitions_tbl final_trans;
  final_trans

let learn_ta (example_trees: (string list * tree * (bool * bool) * restriction list) list) (o_bp_tbl: (int, symbol list) Hashtbl.t) 
  (sym_state_ls: (symbol * state) list) (a: symbol list) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_syms_nonterms: (symbol * state) list) 
  (sts_order_syms_lsls: ((int * state) * symbol list) list)
  (debug_print: bool): ta = 
  
  let o_bp: restriction list = Hashtbl.fold (fun o syms acc -> 
    let to_add = syms |> List.fold_left (fun acc' s -> (Prec (s, o))::acc') [] in to_add @ acc) o_bp_tbl [] 
  in
    
  wrapped_printf "\n O_p list: \n\t"; Pp.pp_restriction_lst op_ls; 
  (* wrapped_printf "\nO_a list :\n\t"; oa_ls |> List.iter (fun r -> match r with Assoc (s, a) -> Pp.pp_symbol s; wrapped_printf " assoc %s" a | Prec _ -> ()); wrapped_printf "\n";
  wrapped_printf "\nO_p list :\n\t"; op_ls |> List.iter (fun r -> match r with Prec (s, o) -> Pp.pp_symbol s; wrapped_printf " order %d" o | Assoc _ -> ()); wrapped_printf "\n"; *)
  
  let (lvl_state_pairs, init_state): (int * state) list * state = get_states op_ls in
  let state_ls: state list = lvl_state_pairs |> List.map snd in
  let raw_trans_ls: ((state * symbol), sigma list list) Hashtbl.t = 
    get_transitions oa_ls op_ls o_bp_tbl sym_state_ls a lvl_state_pairs init_state sym_ord_rhs_ls triv_syms_nonterms sts_order_syms_lsls debug_print in
  (* let ordered_trans_ls = order_trans_ls state_ls raw_trans_ls in *)
  (* To update terminals later! *)
  let ta_res: ta = { states = state_ls; alphabet = a; final_states = [init_state]; terminals=[];
  transitions = raw_trans_ls; trivial_sym_nts=triv_syms_nonterms } in 
  wrapped_printf "\n\nLearned TA:\n"; Pp.pp_ta ta_res; ta_res

 *)
