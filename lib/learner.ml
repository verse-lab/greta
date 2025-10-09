open Ta
open Cfg
open Treeutils

exception No_state_for_sym_order
exception Max_level_state
exception No_lhs_state

let wrapped_printf debug fmt =
  if debug then Printf.printf fmt
  else Printf.ifprintf stdout fmt

let rec replace_nth n x lst =
  match lst with
  | [] -> raise (Failure "replace_nth : no nth element")
  | _ :: tl when n = 0 -> x :: tl
  | hd :: tl -> hd :: replace_nth (n - 1) x tl

let increment_suffix (s: state): state =
  let re: Str.regexp = Str.regexp "^\\(.*[^0-9]\\)\\([0-9]+\\)$" in
  if Str.string_match re s 0 then
    let prefix: state = Str.matched_group 1 s in
    let num = int_of_string (Str.matched_group 2 s) in
    prefix ^ string_of_int (num + 1)
  else
    s ^ "1"

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

let update_op_per_ord_amb_symsls (op_tbl: (int, symbol list) Hashtbl.t) (curr_ord: int) (amb_syms_ordered: symbol list list) 
  (_order_symlsls_ls: (int * (symbol list) list) list) (debug: bool): (int, symbol list) Hashtbl.t = 
  
  (* 1. Find how much to push - depends on largest size of the 'symbol list' element in 'amb_syms_ordered' *)
  let push_n: int = 
    let max_len = 
      amb_syms_ordered |> List.fold_left (fun len_acc sym_ls -> 
        let len = (List.length sym_ls) in if len > len_acc then len else len_acc) 0 
    in max_len
  in 
  
  (* 2. Find S \ { amb_syms } based on all the symbols in curr order \ all amb symbols in curr order *)
  let all_syms_in_curr_ord: symbol list = 
      match (Hashtbl.find_opt op_tbl curr_ord) with Some sls -> sls | None -> raise (Failure "no syms found for order in Op_tbl")
  in
  let all_amb_syms: symbol list = 
    amb_syms_ordered |> List.flatten
  in 
  
  (* 'temp_others' refers to S \ { amb_syms } *)
  let temp_others: symbol list = 
    all_syms_in_curr_ord |> List.filter (fun sym -> not (List.mem sym all_amb_syms)) 
  in

  (* 
  if debug then 
    (wrapped_printf debug "\t * Push_n  =  %d \n\t * S in Order %d : " push_n curr_ord; all_amb_syms |> Pp.pp_symbol_list; 
      wrapped_printf debug "\n\t * S \\ syms : "; temp_others |> Pp.pp_symbol_list);  
  *)
    
  (* 3. Replace Order -> original symbol list    with     Order -> tmp_others *)
  Hashtbl.remove op_tbl curr_ord;
  Hashtbl.add op_tbl curr_ord temp_others;
  
  (* 
  if debug then 
    (wrapped_printf debug "\n\n\t -- After replacing Order -> temp_others : "; temp_others |> Pp.pp_symbol_list ; 
    Pp.pp_obp_tbl op_tbl);  
  *)
  
  (* 4. Pusy by 'push_n - 1' (largest amb_sym list element's length) *)
  push_keys_if_gte_order_by curr_ord op_tbl (push_n - 1);
  
  (* Insert 'tmp_others' as Ord -> tmp_others    Ord+1 -> tmp_others   Ord+(push_n - 1)  -> tmp_others *)
  for i = 0 to (push_n - 1) do 
    let all_amb_syms_to_add_at_nth_precedence: symbol list = 
      find_nth_ambs_from_symlsls i amb_syms_ordered
    in 
      Hashtbl.add op_tbl (curr_ord + i) (temp_others @ all_amb_syms_to_add_at_nth_precedence)
  done;
  
  if debug then 
    (wrapped_printf debug "\n\t * After updating for amb syms list : "; Pp.pp_symbol_list all_amb_syms; 
    Pp.pp_obp_tbl op_tbl); 
  op_tbl


let learn_op (o_bp_tbl: (int, (symbol list) list) Hashtbl.t) (oa_ls: Ta.restriction list) 
  (oa_op_ordered_sym_lsls: (int * (symbol list) list) list) (debug_print: bool): (int, symbol list) Hashtbl.t = 
  let open List in
  
  let sorted_ord_amb_symlsls_ls: (int * (symbol list) list) list = 
    oa_op_ordered_sym_lsls |> List.filter (fun (_o, symlsls) -> not (List.is_empty symlsls))
  in 
  sorted_ord_amb_symlsls_ls |> List.iter (fun (o, symlsls) -> 
    wrapped_printf debug_print "\n\t\t Order %d    [ " o; 
    symlsls |> List.iter Pp.pp_symbol_list; wrapped_printf debug_print " ] \n" ; 
    );

  (* 
  wrapped_printf debug_print "\n\t Ordered symbol list list (symbols wrt. precedence order specification):"; 
  ordered_sym_lsls |> List.iter (fun symls -> wrapped_printf debug_print "\n\t"; Pp.pp_symbol_list symls);
  wrapped_printf debug_print "\n\n"; 
  *)
  (* Group the restriction_pair list based on the associated symbols' group paird with order *)
  let grouped_symbols: (int * (symbol list)) list = 
    Hashtbl.fold (fun o symls_ls o_symls_acc -> 
      let to_acc: (int * (symbol list)) list = 
        symls_ls |> List.map (fun symls -> (o, symls)) 
      in
      to_acc @ o_symls_acc) o_bp_tbl []
  in 
  (* if debug_print then (wrapped_printf debug_print "\n\tGrouped symbol list:\n"; 
    grouped_symbols |> List.iter (fun (i, symls) -> wrapped_printf debug_print "\tOrder %d   =>  " i; 
    symls |> Pp.pp_symbol_list; wrapped_printf debug_print " \n")); *)

  let order_symlsls_ls: (int * (symbol list) list) list = 
    grouped_symbols 
    (* Sort the restriction based on length (symbols list) from the same group *)
    |> group_sym_ls_by_order 
    (* Sort the (o, sym list list) from highest 'o' to lowest 'o' *)
    |> sort_assoc_desc
    (* Sort the 'sym list list' in (o, sym list list) based on Length (sym list) from largets to smallest *)
    |> sort_inner_by_length_desc
  in  
  if debug_print then (wrapped_printf debug_print "\n\tGrouped symbol list list per order:\n"; 
    order_symlsls_ls |> List.iter (fun (o, symlsls) -> wrapped_printf debug_print "\tOrder %d    =>    " o;
    symlsls |> List.iter Pp.pp_symbol_list; wrapped_printf debug_print " \n\n")
  );

  (* if debug_print then (wrapped_printf debug_print 
    "\n\tAmbig symbols total-ordered from lowest to highest (listed from highest order to lowest order):\n\t";
    sorted_ord_amb_symlsls_ls |> List.iter (fun (o, symlsls) -> wrapped_printf debug_print "\n\tFor order %d  " o; 
      symlsls |> iter Pp.pp_symbol_list; wrapped_printf debug_print "\n"); wrapped_printf debug_print "\n\n");
   *)
  
  (* Flatten the o_bp_tbl from (int, symbol list list) Hashtbl.t to (int, symbol list) Hashtbl.t *)
  let all_ords = 
    Hashtbl.length o_bp_tbl in 
  let op_tbl: (int, symbol list) Hashtbl.t = 
    Hashtbl.create all_ords 
  in 
  
  for i = 0 to (all_ords - 1) do 
    let exist_symlsls: symbol list list = 
      Hashtbl.find o_bp_tbl i in 
    let new_symls: symbol list = 
      exist_symlsls |> List.flatten in 
    Hashtbl.add op_tbl i new_symls
  done; 

  (* Pp.pp_obp_tbl op_tbl; *)
  
  let res_tbl_wrt_op: 
    (int, symbol list) Hashtbl.t = 
    sorted_ord_amb_symlsls_ls |> 
    List.fold_left (fun op_tbl_acc ord_amb_symlsls -> 
      let ord, amb_syms_total_ordered_ls = (fst ord_amb_symlsls), (snd ord_amb_symlsls) in 
      update_op_per_ord_amb_symsls op_tbl_acc ord amb_syms_total_ordered_ls order_symlsls_ls debug_print
      ) op_tbl
  
  in
  (* Now update op_tbl wrt. oa_ls *)
  let _syms_oa: symbol list = oa_ls |> map sym_of_oa_restriction in 
  let _res_tbl_wrt_op_oa: (int, symbol list) Hashtbl.t = Hashtbl.create 0
  (*
    syms_oa |> fold_left (fun op_tbl_acc _sym ->  
      op_tbl_acc
      
            
      let orders_ls: int list = orders_of_sym_in_op_tbl sym dummy_sym op_tbl_acc debug_print in
      if (List.length orders_ls) = 1
      then 
        (let curr_ord = orders_ls |> hd in
         update_op_tbl_per_oa_sym sym curr_ord op_tbl_acc debug_print)
      else 
        if (List.length orders_ls) > 1
        then 
          ((* If there are multiple orders for these symbols, then run in reverse order *) 
          let orders_sorted_decr = 
            orders_ls |> List.sort (fun x y -> Int.compare y x) 
          in orders_sorted_decr |> List.fold_left (fun tbl_acc curr_ord -> 
            update_op_tbl_per_oa_sym sym curr_ord tbl_acc debug_print) op_tbl_acc)
        else
          (* If length is not >= 1, simply pass op_tbl_acc *)
          op_tbl_acc
          
     
      
      ) res_tbl_wrt_op
         *)
  in
  (* if debug_print then (wrapped_printf debug_print "\n O_p map after updating wrt O_a: \n"; 
    Pp.pp_obp_tbl res_tbl_wrt_op);  *)
  (* res_tbl_wrt_op_oa *)
  res_tbl_wrt_op

let populate_trans_tbl_with (trans_tbl: ((state * symbol), beta list) Hashtbl.t) (curr_st: state) (sym: symbol) (prod: production) = 
  let sig_ls = snd prod in 
  let trans: beta list = 
    sig_ls |> Cfgutils.sigma_list_to_beta_list |> List.map (fun b -> match b with | T t -> T t | S _old_st -> S curr_st) in
  Hashtbl.add trans_tbl (curr_st, sym) trans

let update_beta_list_at_index_with_lhs_st (old_beta_ls: beta list) (lhs_st: state) (ind: int): beta list = 
  let higher_state: state = increment_suffix lhs_st in 
  replace_nth ind (S higher_state) old_beta_ls

let update_oa_sym_prod_for_index (sym: symbol) (ind: int) (trans_tbl: ((state * symbol), beta list) Hashtbl.t)
  (debug: bool) = 
  trans_tbl |> Hashtbl.iter (fun (lhs_st, curr_sym) old_beta_ls ->
    if (syms_equals curr_sym sym) 
    then (
      if debug then (wrapped_printf debug "\n\tFound same symbol in Trans_tbl => "; Pp.pp_symbol curr_sym);
      let new_beta_ls = update_beta_list_at_index_with_lhs_st old_beta_ls lhs_st ind
          in Hashtbl.replace trans_tbl (lhs_st, curr_sym) new_beta_ls))

let learn_ta (op_learned: (int, symbol list) Hashtbl.t) (oa_neg: restriction list) (prods_map: (int * production) list) 
  (high_to_lows: (symbol * int * int) list) (debug_print: bool): ta = 

  let states_res: state list ref = ref [] in 
  let alph: symbol list ref = ref [] in
  let max_lvl = (Hashtbl.length op_learned) - 1 in 
  let trans_tbl: ((state * symbol), beta list) Hashtbl.t = 
    Hashtbl.create (List.length prods_map) 
  in
    Hashtbl.iter (fun lvl sls ->
      let curr_state = "e" ^ (string_of_int lvl) 
      in 
        states_res := (curr_state):: !states_res;
        alph := !alph @ sls;
        (* Learn wrt. O_p learned *)
        sls |> List.iter (fun sym -> 
          let sym_prod = production_of_id (id_of_sym sym) prods_map in
          populate_trans_tbl_with trans_tbl curr_state sym sym_prod);

          (* Also connect each state to next until max level *)
          if (lvl < max_lvl) then 
            (let higher_state: state = increment_suffix curr_state in 
            let corr_beta_ls: beta list = [(S higher_state)] in
            Hashtbl.add trans_tbl (curr_state, epsilon_sym) corr_beta_ls)
    ) op_learned;
  
  let alph_res = !alph |> remove_dup_symbols in
  oa_neg |> List.iter (fun r -> 
    match r with Prec _ -> raise (Failure "update trans wrt. oa_neg : o_p not possible")
    | Assoc (sym, ind) -> update_oa_sym_prod_for_index sym ind trans_tbl debug_print);

  Printf.printf "\n\nHigh to lows: ";
  high_to_lows |> List.iter (fun (s, max, min) -> Printf.printf "\n Symbol: "; Pp.pp_symbol s; Printf.printf " Max level: %d Min level: %d" max min);
  high_to_lows |> List.iter (fun (sym, max, min) ->
    let (_, rhs) = production_of_id (id_of_sym sym) prods_map in
    let beta_ls = rhs |> List.map (fun r -> match r with 
      | Term t -> T t 
      | Nt _ -> S ("e" ^ string_of_int min)) in
    Hashtbl.add trans_tbl ("e" ^ string_of_int max, sym) beta_ls
  );

  let res_ta: ta = 
  { states = !states_res ; alphabet = alph_res ; final_states = ["e0"] ;
    terminals = []; transitions = trans_tbl }
  in if debug_print then (wrapped_printf debug_print "\n\nLearned TA: \n"; Pp.pp_ta res_ta);
  res_ta


(* 
let get_transitions (oa_ls: restriction list) (op_ls: restriction list) 
  (o_bp_tbl: (int, symbol list) Hashtbl.t) (sym_lhs_ls: (symbol * state) list)
  (a: symbol list) (lvl_state_pairs: (int * state) list) (start: state) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_syms_nonterms: (symbol * state) list) 
  (sts_order_syms_lsls: ((int * state) * symbol list) list) (debug: bool): 
  ((state * symbol), sigma list list) Hashtbl.t =
  
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
  let max_lvl = 
    let max_starting_from_zero = sym_ord_ls_wrt_op_new |> List.map snd |> List.fold_left max 0 
    in max_starting_from_zero + 1

 *)
