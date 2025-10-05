open Ta
open Cfg
open Treeutils

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
  let res_tbl_wrt_op: (int, symbol list) Hashtbl.t = 
    o_tmp_ls |> fold_left (fun op_tbl_acc (r1, r2) -> 
      begin 
        let sym1 = sym_of_op_restriction r1 in (* r1 and r2 symbols should have same order *)
        let sym2 = sym_of_op_restriction r2 in
        let orders_ls: int list = orders_of_sym_in_op_tbl sym1 sym2 op_tbl_acc debug_print in 
        let sym_top, sym_bot = sym_top_sym_bot_of_restrictions r1 r2 debug_print in
        if (List.length orders_ls) = 1 
        then 
          (let curr_ord: int = orders_ls |> hd in 
          update_op_tbl_per_op_syms sym_top sym_bot curr_ord op_tbl_acc debug_print)
        else 
          if (List.length orders_ls) > 1 
          then
            ((* If there are multiple orders for these symbols, then run in reverse order *) 
            let orders_sorted_decr = 
              orders_ls |> List.sort (fun x y -> Int.compare y x) 
            in orders_sorted_decr |> List.fold_left (fun tbl_acc curr_ord -> 
              update_op_tbl_per_op_syms sym_top sym_bot curr_ord tbl_acc debug_print) op_tbl_acc)
          else 
            (* If length is not >= 1, simply pass op_tbl_acc *) 
            op_tbl_acc
      end
    ) o_bp_tbl 
  in
  (* Now update op_tbl wrt. oa_ls *)
  let oa_ls: restriction list = learn_oa_neg tree_examples debug_print in
  let syms_oa: symbol list = oa_ls |> List.map sym_of_oa_restriction in 
  let res_tbl_wrt_op_oa: (int, symbol list) Hashtbl.t = 
    syms_oa |> List.fold_left (fun op_tbl_acc sym -> 
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
  in
  (* if debug_print then (wrapped_printf debug_print "\n O_p map after updating wrt O_a: \n"; 
    Pp.pp_obp_tbl res_tbl_wrt_op);  *)
  res_tbl_wrt_op_oa

let learn_ta (_op_learned: (int, symbol list) Hashtbl.t) (_oa_neg: restriction list) (_prods_map: (int * production) list) 
  (_debug_print: bool): ta = 
  null_ta


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
  let max_lvl = 
    let max_starting_from_zero = sym_ord_ls_wrt_op_new |> List.map snd |> List.fold_left max 0 
    in max_starting_from_zero + 1
  in
let learn_ta (example_trees: (string list * tree * (bool * bool) * restriction list) list) (o_bp_tbl: (int, symbol list) Hashtbl.t) 
  (sym_state_ls: (symbol * state) list) (a: symbol list) 
  (sym_ord_rhs_ls: ((symbol * int) * sigma list) list) (triv_syms_nonterms: (symbol * state) list) 
  (sts_order_syms_lsls: ((int * state) * symbol list) list)
  (debug_print: bool): ta = 
  let o_bp: restriction list = Hashtbl.fold (fun o syms acc -> 
    let to_add = syms |> List.fold_left (fun acc' s -> (Prec (s, o))::acc') [] in to_add @ acc) o_bp_tbl [] 
  in
  let ta_res: ta = { states = state_ls; alphabet = a; final_states = [init_state]; terminals=[];
 *)
