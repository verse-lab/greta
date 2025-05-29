open Ta
open Cfg

exception Leaf_has_no_symbol
exception No_assoc_possible
exception No_prec_possible
exception Op_has_trivial_symbol
exception No_sig_ls_in_sig_lsls
exception No_cross_product_sigls_possible
exception Reachable_states_not_matching
exception No_terminal_possible
exception Not_possible

(** is_cond_expr : check if state is representing boolean state *)
let is_cond_expr (s: state): bool =
  let open Str in
  string_match (regexp "cond") s 0 || string_match (regexp "Cond") s 0  

let is_cond_state (s: state): bool =
  String.starts_with ~prefix:"con" s || String.starts_with ~prefix:"C" s

let trees_equal (e1: tree) (e2: tree) (debug_print: bool): bool =
  let booltostr x = if x then "true" else "false" in
  let open Printf in 
  if debug_print then (printf "\n  >> Are the following trees equal?\n\t";
  Pp.pp_tree e1; printf "\n\t"; Pp.pp_tree e2); 
  let rec loop t1 t2 =
    match t1, t2 with
    | Leaf _, Node _ | Node _, Leaf _ -> false
    | Leaf v1, Leaf v2 -> v1 = v2
    | Node (sym1, subts1), Node (sym2, subts2) ->
      syms_equals sym1 sym2 && 
      List.fold_left2 (fun acc subt1 subt2 -> 
        acc && loop subt1 subt2) true subts1 subts2
  in let res = loop e1 e2 in 
  if debug_print then (printf "\n  >> Result of equality:\t%s\n" (booltostr res)); 
  res

let is_leaf (t: tree): bool =
  match t with Leaf _ -> true
  | Node (_, _) -> false

let is_conditional_leaf (t: tree): bool =
  match t with Leaf v -> (is_cond_expr v)
  | Node (_, _) -> false

(** return_state : return state (load of Leaf) *)
let return_state (t: tree): state =
  match t with Leaf v -> v
  | Node (_, _) -> "Error: Not a leaf"

let gen_state_list (sym_arity: int) (st: state): state list = 
  List.init sym_arity (fun _ -> st)

let gen_rhs_states (sym: symbol) (st: state): state list =
  let sym_arity = snd sym in 
  if syms_equals sym ("IF", 2)
  then ["C"; st]
  else if syms_equals sym ("IF", 3)
  then ["C"; st; st]
  else List.init sym_arity (fun _ -> st)

let subts_state_list (sym: symbol) (ts: tree list) (default_state: state) 
  (versatile_syms: string list) (cond_state: state): state list =
  let rec loop ts acc =
    match ts with [] -> List.rev acc
    | h :: tl -> 
      if (is_leaf h) then loop tl ((return_state h) :: acc) 
      else loop tl (default_state :: acc)
  in let stats = loop ts [] 
  in if (List.mem (fst sym) versatile_syms) 
    then stats |> List.mapi(fun i x -> if (i = 0) then cond_state else x)
    else stats

(** height : find the height (maximum depth) of tree *)
let height (e: tree): int =
  let max_ls ls = List.fold_left max 0 ls in 
  let rec loop t acc = match t with 
    | Leaf _ -> acc
    | Node (_, ts) -> ts
      |> List.map (fun x -> loop x (acc+1)) 
      |> max_ls
  in loop e 0

let node_symbol (e: tree): string =
  match e with Leaf _ -> "dummy"
  | Node (s, _) -> fst s

let node_symbol_full (e: tree): symbol =
  match e with Leaf _ -> ("no_symbol", 0)
  | Node (s, _) -> s

let change_node_symbol_with (e: tree) (sym: string): tree =
  match e with Leaf v -> Leaf v
  | Node ((_, ar), subts) -> Node ((sym, ar), subts)

let branches_of_tree (e:tree): int =
  match e with Leaf _ -> 0
  | Node (_, subts) -> List.length subts

let is_there_node (ts: tree list): bool =
  ts |> List.exists (fun t -> match t with 
  | Leaf _ -> false | Node _ -> true)

(** return_node_index : assume there is node in the 'ts', if not throw error *)
let return_node_index (ts: tree list): int =
  let rec loop ls ind =
    match ls with [] -> raise (Failure "Not found")
    | h :: tl -> 
      if (is_leaf h) then loop tl (ind+1) else ind
  in loop ts 0

let replace_node_wleaf (ts: tree list): tree list =
  let open Str in
  let is_cond_expr (s: string): bool =
    string_match (regexp "cond") s 0 || string_match (regexp "Cond") s 0 in
  let strip_treestruct (t: tree): string = 
    match t with Leaf exp -> exp | Node _ -> "dummy" in
  let rec loop ls prev cnt acc =
    match ls with [] -> List.rev acc
    | h :: tl -> 
      let expr' = strip_treestruct h in
      if (is_leaf h) then loop tl expr' cnt (h::acc)
      else if (cnt > 1) then raise (Failure "Trees with >1 node")
      (* if it's a first-occuring node replace with leaf *)
      else if (is_cond_expr prev) 
      then loop tl prev (cnt+1) ((Leaf "expr")::acc) 
      else loop tl prev (cnt+1) ((Leaf prev)::acc)
  in loop ts ("dummy") 0 []

(** combine_trees_aux : combine 'up_t' as upper and 'lo_t' as lower trees *)
let combine_trees_aux (up_t: tree) (lo_t: tree): tree = 
  match up_t, lo_t with
  | Leaf _, _ | _, Leaf _ -> 
    Printf.printf "Cannot combine: either of the trees is Leaf!"; Leaf "dummy"
  | Node (up_sym, up_subts), Node (lo_sym, lo_subts) ->
    let last_ind = (List.length up_subts) - 1 in
    let up_subts_new = List.mapi (fun i subt -> 
      if (i = last_ind) then Node (lo_sym, lo_subts) else subt) up_subts in
    Node (up_sym, up_subts_new)

(** rewrite_syms : rewrite "PLUS" / "MUL" with "+" / "*" respectively *)
let rewrite_syms (tts: (tree * tree) list): (tree * tree) list =
  let rec rewrite_syms_aux (t: tree): tree = match t with 
    | Leaf v -> Leaf v
    | Node (sym, subts) -> 
      let sym_new = 
        if (sym_equals sym "PLUS") then ("+", 2) else 
        if (sym_equals sym "MUL") then ("*", 2) else sym in
      let subts_new = subts |> List.map (fun t' -> rewrite_syms_aux t') in
      Node (sym_new, subts_new) in
  tts |> List.map (fun (t1, t2) -> rewrite_syms_aux t1, rewrite_syms_aux t2)

(** rename_states : rename states in ta_res from /\ *)
let rename_w_parser_friendly_states_in_ta (debug_print: bool) (inp_ta: ta): ta =
  let open Printf in
  if debug_print then (printf "\nRename the following tree automaton:\n"; Pp.pp_ta inp_ta);
  let start_old, start_new = inp_ta.start_state, "expr1" in
  let states_mapping_init: (state * state) list = (start_old, start_new) :: [] in
  let eind, cind = ref 2, ref 1 in
  let states_mapping: (state * state) list = inp_ta.states |> List.fold_left (fun acc st_curr ->
    if (st_curr = start_old) then acc else if (st_curr = "ϵ") then (st_curr, st_curr)::acc else 
    if (is_cond_state st_curr) then let st_new = "cond_expr" ^ string_of_int !cind in cind := !cind+1; (st_curr, st_new)::acc 
    else let st_new = "expr" ^ string_of_int !eind in eind := !eind + 1; (st_curr, st_new) :: acc) 
    states_mapping_init in
  let replace_with_new (stat_old: state): state = match List.assoc_opt stat_old states_mapping with
    | Some v -> v | None -> raise (Failure "Old state is not found") in
  let states_new = inp_ta.states |> List.map (fun st -> if (st = "ϵ") then st else replace_with_new st) in
  let trans_new = inp_ta.transitions |> List.map (fun (stlhs, (sym, stsrhs)) ->
    let stlhs_new = replace_with_new stlhs in 
    let stsrhs_new = stsrhs |> List.map replace_with_new in (stlhs_new, (sym, stsrhs_new))) in
  let ta_res: ta = {states=states_new; alphabet=inp_ta.alphabet; start_state=start_new; transitions=trans_new; trivial_sym_nts=[]} in 
  if debug_print then (printf "\nResult of renaming:\n"; Pp.pp_ta ta_res; printf "\n");
  ta_res

(* let subts_to_list (subts: tree list):  *)

(** tree_to_expr : helper to make the dual tree expression easier
 *                 works for the tree combining 2 trees each of which has height 1
 *                 needed to differentiate two expressions when they both use the same operator
 *                 e.g., (expr + expr) + expr   vs.   expr + (expr + expr) *)
let tree_to_expr (t: tree) : string list =
  let open List in
  (* 
    (* *** debug *** *)
    let open Printf in 
    (printf "\n\n\t\t >> Tree_to_expr "; Pp.pp_tree t; printf "\n\n");
  *)
  let is_empty_leaf (ts: tree list) =
    match (hd ts, length ts) with (Leaf "ϵ"), 1 -> true | _ -> false in
  let rec tree_loop t: string list =
    match t with Leaf s -> [s]
    | Node (sym, subts) ->
      let s', rnk = fst sym, snd sym in 
      match s', rnk with 
      | _, 0 -> 
        if is_empty_leaf subts then [s'] 
        else raise (Failure "tree_to_expr : no trivial case should be presented as a tree")
      | "LPARENRPAREN", 1 -> 
        ["("; "LPAREN"] @ tree_loop (nth subts 1) @ ["RPAREN"; ")"]
      | "LBRACERBRACE", 1 -> 
        ["("; "LBRACE"] @ tree_loop (nth subts 1) @ ["RBRACE"; ")"]
        (* TODO: Make the rank below to be generalizable *)
      | _, 2 -> 
        ["("] @ tree_loop (nth subts 0) @ [s'] @ tree_loop (nth subts 1) @ [")"]
      | s, _ -> 
        let treeexprs_for_subts = subts |> fold_left (fun acc t -> acc @ tree_loop t) [] in 
        ["("; s] @ treeexprs_for_subts @ [")"] 
  in tree_loop t

(* (TODO) After fixing UI, remove 'gen_dual_expr' and 'present_tree_pair_single_operator' *)
(** gen_dual_expr : needed only when operators are the same in combined trees
 *                  e.g., (expr + expr) + expr vs. expr + (expr + expr) *)
let gen_dual_expr (strs: string list): string list =
  let is_operation (s: string): bool = (s = "+") || (s = "*") in
  let rec str_loop ls passed_outer to_switch fst_op inner_closed acc =
    match ls with [] -> acc
    | h :: tl -> 
      if (h = ")" && not passed_outer) 
      then str_loop tl true to_switch fst_op inner_closed (h::acc)
      else if (h = ")" && passed_outer)
      then str_loop tl passed_outer true fst_op inner_closed acc
      else if (is_operation h && to_switch && fst_op)
      then str_loop tl passed_outer to_switch false inner_closed (")"::h::acc)
      else if (h = "(" && not inner_closed)
      then str_loop tl passed_outer to_switch fst_op true acc
      else if (h = "(" && inner_closed)
      then str_loop tl passed_outer to_switch fst_op inner_closed ("("::h::acc)
      else str_loop tl passed_outer to_switch fst_op inner_closed (h::acc)
  in str_loop (List.rev strs) false false true false []

let present_tree_pair_single_operator (trees: tree * tree): unit =
  let open Printf in
  printf "\n\nChoose your preference! \n(Type either 0 or 1.)\n\n";
  let print_strs ls = printf "\t"; ls |> List.iter (printf "%s "); printf "\n" in 
  let expr1 = fst trees |> tree_to_expr in 
  let expr2 = gen_dual_expr expr1 in 
  printf "Option 0: \n"; print_strs expr1;
  printf "Option 1: \n"; print_strs expr2; printf "\n"
  (* ;printf "Option 2: \n\tNo preference"; printf "\n\n" *)

let present_tree_pair (trees: tree * tree): unit =
  let open Printf in
  printf "\n\nChoose your preference! \n(Type either 0 or 1.)\n\n";
  let print_strs ls = printf "\t"; ls |> List.iter (printf "%s "); printf "\n" in
  let expr1 = fst trees |> tree_to_expr in
  let expr2 = snd trees |> tree_to_expr in (* [fix] instead of 'gen_dual_expr' *)
  printf "Option 0: \n"; print_strs expr1;
  printf "Option 1: \n"; print_strs expr2; printf "\n"

(* (TODO) To remove redundancies wrt figuring out Oa-related trees *)
let tree_with_single_operator (e: tree): bool =
  let fst_sym = node_symbol_full e in
  let bool_ls ls = List.fold_left (fun x acc -> x && acc) true ls in
  let rec traverse t prev_sym res = 
    match t with Leaf _ -> res
    | Node (sym, subts) -> subts 
      |> List.map (fun t -> traverse t sym ((syms_equals sym prev_sym) && res))
      |> bool_ls
  in traverse e fst_sym true

let same_syms ls = 
  let fst_sym = List.hd ls in 
  let filtered = List.filter (fun s -> not (syms_equals s fst_sym)) ls in
  if filtered = [] then true else false

let subtrees_of (e:tree): tree list = 
  match e with Leaf _ -> []
  | Node (_, subts) -> subts

let tree_symbol (e: tree): symbol = 
  match e with Leaf _ -> ("dummy", -1) (*raise Leaf_has_no_symbol*)
  | Node (s, _) -> s

let collect_syms (e: tree): symbol list =
  let rec collect_loop t acc = 
    match t with Leaf _ -> acc
    | Node (s, subts) -> 
      let syms = subts |> List.fold_left (fun acc' subt -> 
        (collect_loop subt []) @ acc') []
      in s::acc @ syms
  in collect_loop e []
  
let check_oa_op (e: tree): bool * bool = 
  let t_syms: symbol list = collect_syms e 
  in if same_syms t_syms then (true, false) else (false, true)

let collect_oa_restrictions (example_trees: (string list * tree * (bool * bool) * restriction list) list) 
(debug_print: bool): restriction list = 
let res = example_trees 
  |> List.fold_left (fun acc (_, _, (oa, _), rls) -> if oa then rls @ acc else acc) [] 
in if debug_print then (Printf.printf "\n  Collected O_a : "; Pp.pp_restriction_lst res); res

let update_if_exist_overlapping_symbols_w_diff_order (so_ls: (symbol * int) list) 
  (acc_ls: (symbol * int) list list) (debug: bool): (symbol * int) list list = 
  let open Printf in 
  let flattened_acc: (symbol * int) list = acc_ls |> List.flatten in
  let syms_ls: symbol list = flattened_acc |> List.map fst in
  let find_ord s': int = List.assoc s' flattened_acc in
  let rec loop (ls: (symbol * int) list) (acc: (symbol * int) list list) = 
    match ls with [] -> acc 
    | (s, o) :: so_tl -> 
      if List.mem s syms_ls then 
        (let ord = find_ord s in 
        if (ord = o) then 
          (if debug then printf "\n\t Exist in acc so list but same order so leave intact\n"; loop so_tl acc)
        else 
          (* TODO: change logic here! *)
          (if debug then printf "\n\t"; loop so_tl acc)) 
      else 
        (if debug then printf "\n\t Not exist in acc so list\n"; loop so_tl acc)
  in loop so_ls acc_ls 

let refine_raw_rest_lsls_wrt_relativ_order (rlsls: restriction list list) (debug: bool): restriction list = 
  let sym_ord_lsls: (symbol * int) list list = 
    rlsls |> List.map (fun rls -> 
      let fst_elem, snd_elem = List.nth rls 0, List.nth rls 1 in
      match fst_elem, snd_elem with 
      | Prec (sym1, o1), Prec (sym2, o2) -> [(sym1, o1); (sym2, o2)]
      | _ -> raise No_assoc_possible) in 
  let rec refine_loop (ls: (symbol * int) list list) (map_acc: (symbol * int) list list): (symbol * int) list = 
    match ls with [] -> List.flatten map_acc 
    | sols_hd :: sols_tl -> 
      let new_acc = update_if_exist_overlapping_symbols_w_diff_order sols_hd map_acc debug 
      in refine_loop sols_tl new_acc
  in let sym_ord_ls = refine_loop sym_ord_lsls [] 
  in sym_ord_ls |> List.map (fun (s, o) -> Prec (s, o))

let collect_op_restrictions (example_trees: (string list * tree * (bool * bool) * restriction list) list) 
  (debug_print: bool): restriction list = 
  let raw_rest_lsls: restriction list list = example_trees 
    |> List.fold_left (fun acc (_, _, (_, op), rls) -> if op then rls :: acc else acc) [] 
    |> List.map (fun r_ls -> r_ls 
      |> List.map (fun r -> match r with Prec (sym, o) -> Prec (sym, (o-1)) | Assoc _ -> raise No_assoc_possible))
  in 
  let refined_rest_ls = refine_raw_rest_lsls_wrt_relativ_order raw_rest_lsls debug_print in
  (Printf.printf "\n\t Refined restriction list : "; Pp.pp_restriction_lst refined_rest_ls);
  let res = example_trees 
    |> List.fold_left (fun acc (_, _, (_, op), rls) -> if op then rls @ acc else acc) [] 
    |> List.map (fun r -> match r with Prec (sym, o) -> Prec (sym, (o-1)) | Assoc _ -> raise No_assoc_possible)
  in 
  if debug_print then (Printf.printf "\n  Collected O_p : "; Pp.pp_restriction_lst res); res

(* helper for 'combine_op_restrictions'
 - find all occurrences of (s, o) for sym 's' in o_tmp and combine all the matching o's *)
let find_in_o_tmp (sym: symbol) (bo: int) (o_tmp: restriction list): int =
  (* note: need to combine for all occurrences of sym's *)
  let rec loop ls acc =
    match ls with [] -> acc
    | Assoc (_, _) :: _ -> raise No_assoc_possible
    | Prec (s, o) :: tl -> 
      if (syms_equals s sym) then loop tl (o+acc) else loop tl acc
  in loop o_tmp bo

let reorder_op (o_p: restriction list): restriction list = 
  let orders = o_p |> List.map (fun op -> match op with Assoc (_, _) -> raise No_assoc_possible 
    | Prec (_, o) -> o) |> List.sort_uniq compare in
  let new_order_pairs = orders |> List.mapi (fun i o -> (o, i)) 
  in o_p |> List.map (fun op -> match op with Assoc (_, _) -> raise No_assoc_possible 
                      | Prec (s, o) -> 
                        let new_order = match (List.assoc_opt o new_order_pairs) with 
                        | None -> raise Not_found
                        | Some i -> i in Prec (s, new_order))

let combine_op_restrictions (o_bp: restriction list) (o_tmp: restriction list) (debug_print: bool): restriction list =
  let rec traverse_o_bp ls acc = 
    match ls with [] -> List.rev acc 
    | Assoc (_, _) :: _ -> raise No_assoc_possible
    | Prec (sym, bo) :: tl -> 
      let op_order_combined = find_in_o_tmp sym bo o_tmp
      in traverse_o_bp tl (Prec (sym, op_order_combined)::acc)
  in let combined_op = traverse_o_bp o_bp [] 
  in let reordered_combined_op = reorder_op combined_op in
  (if debug_print then Printf.printf "\n  Combined O_p : "; Pp.pp_restriction_lst reordered_combined_op); 
  reordered_combined_op

let update_op_w_restrictions (hi_sym: symbol) (lo_sym: symbol) (init_op: restriction list): restriction list = 
  let find_order (s: symbol): int = 
    init_op |> List.map (fun op -> match op with Assoc _ -> raise (Failure "update_op - Assoc not possible")
      | Prec (s', o') -> (s', o')) |> List.find (fun (sym, _o) -> syms_equals s sym) |> snd 
  in
  let (hi_o, lo_o) = find_order hi_sym, find_order lo_sym in
  (* if hi_sym's ord > lo_sym's ord then no need to update *)
  if (hi_o > lo_o) then init_op else 
    (* otherwise, push lo_sym below hi_sym *)
    begin 
      init_op |> List.map (fun rst -> match rst with Assoc _ -> raise (Failure "update_op - Assoc not possible")
      | Prec (s, o) -> 
        if (syms_equals s lo_sym) 
        then (let corr_lo_o = hi_o - 1 in Prec (s, corr_lo_o)) else Prec (s, o))
    end 

let combine_op_restrictions_in_pairs (o_bp: restriction list) (o_tmp: restriction list) (debug_print: bool): restriction list =
  let rec traverse_o_tmp_in_pairs ls op_acc = 
    match ls with [] -> op_acc 
    | Prec (sym1, bo1) :: Prec (sym2, bo2) :: tl ->
      let updated_op = if (bo1 > bo2) 
        then update_op_w_restrictions sym1 sym2 op_acc 
        else update_op_w_restrictions sym2 sym1 op_acc 
      in traverse_o_tmp_in_pairs tl updated_op
    | _ -> raise (Failure "Op restrictions in pairs")
  in let combined_op = traverse_o_tmp_in_pairs o_tmp o_bp
  in let reordered_combined_op = reorder_op combined_op in
  (if debug_print then Printf.printf "\n  Combined O_p : "; Pp.pp_restriction_lst reordered_combined_op); 
  reordered_combined_op

let sym_in_oa_lst (s: symbol) (oa_ls: restriction list): bool =
  let rec traverse_oa ls =
    match ls with [] -> false
    | Assoc (sym, _) :: tl -> 
      if (syms_equals s sym) then true 
      else traverse_oa tl
    | Prec (_, _) :: _ -> raise No_prec_possible
  in traverse_oa oa_ls

let is_left_assoc (s: symbol) (oa_ls: restriction list): bool = 
  let rec traverse_oa ls: bool =
    match ls with [] -> raise Assoc_either_left_or_right
    | Assoc (sym, a) :: tl -> 
      if (syms_equals s sym) then a = "l"
      else traverse_oa tl
    | Prec (_, _) :: _ -> raise No_prec_possible
  in traverse_oa oa_ls

let order_in_op_lst (s: symbol) (op_ls: restriction list): int =
  let rec traverse_op ls =
    match ls with [] -> raise Op_has_trivial_symbol
    | Prec (sym, o) :: tl -> 
      if (syms_equals s sym) then o
      else traverse_op tl
    | Assoc (_, _) :: _ -> raise No_assoc_possible
  in traverse_op op_ls

let rec chars_of_string ch = 
  match ch with
  | "" -> []
  | ch -> String.get ch 0 :: chars_of_string (String.sub ch 1 (String.length ch - 1))

(* Note! Below works under the assumption that you don't need more than 9 states *)
let get_higher_state (st: state): state = 
  let char_lst = chars_of_string st in 
  let last_idx = List.length char_lst - 1 in 
  let new_char_lst = char_lst |> List.mapi (fun i ch -> 
    if (i = last_idx) 
    then (let next_i: int = (int_of_string (Char.escaped ch)) + 1 
          in String.get (string_of_int next_i) 0)
    else ch) 
  in (new_char_lst) |> List.to_seq |> String.of_seq

let levels_in_op_ls (op_ls: restriction list): int = 
  op_ls |> List.map (fun op -> match op with Assoc (_, _) -> raise No_assoc_possible 
  | Prec (_, o) -> o) |> List.sort_uniq compare |> List.length

let find_all_trans_starting_from (st: state) (trans_ls: transition list) = 
  let rec loop ls acc = 
    match ls with [] -> List.rev acc
    | (lft_st, (_, _)) as tran :: tl -> 
      if (lft_st = st) then loop tl (tran::acc)
      else loop tl acc
  in loop trans_ls []

let order_trans_ls (st_ls: state list) (trans_ls: transition list): transition list = 
  st_ls |> List.fold_left (fun acc st ->  acc @ (find_all_trans_starting_from st trans_ls)) []

let find_rhs_states_from_state_with_sym (st: state) (sym: symbol) (trans_ls: transition list) (debug: bool): state list =
  if debug then Printf.printf "\nFinding transition starting from %s for symbol (%s, %i) ..\n" st (fst sym) (snd sym);
  let rec interm_sts_loop (ls': state list) (rhs_sts_ls: state list): state list * state list = 
    match ls' with 
    | [] -> rhs_sts_ls, []
    | interm_hd :: interm_tl -> 
      Printf.printf "\n\tNow looking for RHS states starting from %s for symbol (%s, %i)\n" interm_hd (fst sym) (snd sym);
      let i_rhs, next_interm_ls = find_rhs_states trans_ls interm_hd []
      in if (i_rhs = []) then interm_sts_loop interm_tl [] else i_rhs, next_interm_ls
  and 
  find_rhs_states (ls: transition list) (from_st: state) (interm_sts: state list): state list * state list =
    match ls with 
    | [] ->
      if interm_sts = []
      then [], []
      else (Printf.printf "\n\tInterm states not empty: "; Pp.pp_states interm_sts; 
           interm_sts_loop interm_sts [])
    | (lft_st, (s, rhs_sts)) :: tl -> 
      if (lft_st = from_st) && (syms_equals s sym)
      then rhs_sts, interm_sts
      else if (lft_st = from_st) && (syms_equals s epsilon_symb)
      then (let next_st = (List.hd rhs_sts) (* if eps-trans, save in 'interm_sts' and traverse til the end *)
            in find_rhs_states tl from_st (next_st::interm_sts))
      else find_rhs_states tl from_st interm_sts
  in let res_rhs_sts, _ (* res_interm_sts*) = find_rhs_states trans_ls st [] in
  (if debug then Printf.printf "\n\t\t >> .. Found RHS states: "; Pp.pp_states res_rhs_sts);
  res_rhs_sts

let cross_product_state_lists (st_ls1: state list) (st_ls2: state list): state list =
  let rec cross_loop ls1 ls2 acc = 
    match ls1, ls2 with 
    | [], [] -> List.rev acc
    | h1 :: tl1, h2 :: tl2 -> 
      let combined = if (h1 = epsilon_state) || (h2 = epsilon_state) 
        then epsilon_state else h1 ^ h2 in cross_loop tl1 tl2 (combined::acc)
    | _, [] | [], _ -> raise Invalid_state_lists
  in cross_loop st_ls1 st_ls2 []

let cross_product_raw_state_lists (st_ls1: state list) (st_ls2: state list): (state * state) list =
  let rec cross_loop ls1 ls2 acc = 
    match ls1, ls2 with 
    | [], [] -> List.rev acc
    | h1 :: tl1, h2 :: tl2 -> 
      let combined = (h1, h2) in cross_loop tl1 tl2 (combined::acc)
    | _, [] | [], _ -> raise Invalid_state_lists
  in cross_loop st_ls1 st_ls2 []

let find_corresponding_sigls (sig_ls: sigma list) (sig_lsls: sigma list list) (triv_states: state list): sigma list = 
  (* --- helper --- *)
  let are_same_triv_states (s1: sigma) (s2: sigma): bool = 
    match s1, s2 with 
    | Nt s1', Nt s2' -> if ((List.mem s1' triv_states) && (List.mem s2' triv_states)) then (String.equal s1' s2') else true
    | _, _ -> false
  in
  let rec corresponding_sig_lists (sig_ls1: sigma list) (sig_ls2: sigma list) (triv_states: state list) (acc: bool): bool = 
    match sig_ls1, sig_ls2 with 
    | _, [] | [], _ -> acc
    | sh1 :: stl1, sh2 :: stl2 -> 
      if (is_terminal sh1) && (is_terminal sh2)
      then (if sigmas_equal sh1 sh2
            then corresponding_sig_lists stl1 stl2 triv_states (true && acc)
            else corresponding_sig_lists stl1 stl2 triv_states (false && acc))
      else if (not (is_terminal sh1)) && (not (is_terminal sh2))
      then (if (are_same_triv_states sh1 sh2) 
            then corresponding_sig_lists stl1 stl2 triv_states (true && acc)
            else corresponding_sig_lists stl1 stl2 triv_states (false && acc))
      else false in
  let rec traverse lsls = 
    match lsls with [] -> []
    | hd_sig_ls :: tl -> 
      if (corresponding_sig_lists sig_ls hd_sig_ls triv_states true)
      then hd_sig_ls
      else traverse tl
  in traverse sig_lsls

let rec cross_product_siglsls (sig_ls1: sigma list) (sig_ls2: sigma list) (triv_states: state list) (acc: (sigma * sigma) list): 
  (sigma * sigma) list = 
  let open String in
  match sig_ls1, sig_ls2 with [], [] -> List.rev acc
  | T t1 :: stl1, T t2 :: stl2 -> cross_product_siglsls stl1 stl2 triv_states ((T t1, T t2)::acc)
  | Nt nt1 :: stl1, Nt nt2 :: stl2 -> 
    if ((equal nt1 epsilon_state) && (not (List.mem nt2 triv_states))) || ((equal nt2 epsilon_state) && (not (List.mem nt1 triv_states)))
    then cross_product_siglsls stl1 stl2 triv_states ((Nt epsilon_state, Nt epsilon_state)::acc) 
    else 
      if ((List.mem nt1 triv_states) && not (List.mem nt2 triv_states)) || ((List.mem nt2 triv_states) && not (List.mem nt1 triv_states))
      then cross_product_siglsls stl1 stl2 triv_states acc
      else cross_product_siglsls stl1 stl2 triv_states ((Nt nt1, Nt nt2)::acc)
  | (T _)::_, (Nt _)::_ | (Nt _)::_, (T _)::_ | _, [] | [], _ -> raise No_cross_product_sigls_possible

let cross_product_raw_sigma_lsls (sig_lsls_ls1: (sigma list list) list) (sig_lsls_ls2: (sigma list list) list) 
  (triv_states: state list) (debug: bool): (sigma * sigma) list list =
  let open List in
  let open Printf in 
  if debug then (printf "\n\tFinding cross product of sigma_lsls's first sig_lsls : \n"; 
    sig_lsls_ls1 |> List.iter Pp.pp_sigma_listlist; printf "\n\tThen second sig_lsls : \n"; sig_lsls_ls2 |> List.iter Pp.pp_sigma_listlist);
  (* --- helper --- *)
  let rec cross_loop lsls1 lsls2 (acc: (sigma * sigma) list list) = 
    match lsls1 with [] -> acc
    | sig_ls_hd1 :: tl1 -> 
      let sig_ls2: sigma list = find_corresponding_sigls sig_ls_hd1 lsls2 triv_states in
      (if debug then printf "\n\t --> corresponding sigma list: \t"; Pp.pp_sigma_list2 sig_ls2);
      if (is_empty sig_ls2) then cross_loop tl1 lsls2 acc
      else (let cross_product_siglsls = cross_product_siglsls sig_ls_hd1 sig_ls2 triv_states [] in
            if (mem cross_product_siglsls acc)
            then cross_loop tl1 lsls2 acc
            else cross_loop tl1 lsls2 (cross_product_siglsls::acc))
  in
  let len1, len2 = length (sig_lsls_ls1), length (sig_lsls_ls2) in
  let reslsls: (sigma * sigma) list list =
    if (is_empty sig_lsls_ls1) || (is_empty sig_lsls_ls2) then [[]]
    else 
      begin
        if (len1 = 1) && (len2 = 1) 
        then 
          (let sig_lsls1, sig_lsls2 = hd sig_lsls_ls1, hd sig_lsls_ls2 
           in cross_loop sig_lsls1 sig_lsls2 [])
        else 
          (if (len1 < len2) 
           then 
            (* assume sig_lsls_ls2 is longer *)
            (let sig_lsls1 = hd sig_lsls_ls1 in 
             sig_lsls_ls2 |> fold_left (fun acc lsls2 -> 
              acc @ (cross_loop sig_lsls1 lsls2 [])) [] 
              |> Utils.remove_dups)
           else 
            if (len1 > len2) 
            then 
              (let sig_lsls2 = hd sig_lsls_ls2 in 
               sig_lsls_ls1 |> fold_left (fun acc lsls1 -> 
                acc @ (cross_loop lsls1 sig_lsls2 [])) [] 
                |> Utils.remove_dups)
              else 
                ((* *** [new_fix] for G2b 010 case *** *)
                let sig_lsls_ls1' = sig_lsls_ls1 |> List.flatten in
                let sig_lsls_ls2' = sig_lsls_ls2 |> List.flatten in
                let len1', len2' = length (sig_lsls_ls1'), length (sig_lsls_ls2') in
                Printf.printf "\n\t\t What's new len(sig_lsls_ls1) %d vs. leng(sig_lsls_ls2) %d\n" len1' len2';
                if (len1' = len2') 
                then 
                  (let _sig_lsls1_hd = hd sig_lsls_ls1 in 
                    let res_here: (sigma * sigma) list list = 
                      sig_lsls_ls1 |> fold_left (fun acc' sig_lsls1 -> 
                        let res' = 
                          sig_lsls_ls2 |> fold_left (fun acc lsls2 -> 
                            let interm_res = (cross_loop sig_lsls1 lsls2 []) in
                            if (interm_res = [[]]) then acc else 
                            acc @ interm_res) [] 
                            |> Utils.remove_dups
                        in res' @ acc'
                        ) []
                      in
                      res_here |> List.iter Pp.pp_sigma_sigma_list; res_here)
                else 
                  (Printf.printf"\n\tCross product bug position!\n";
                   raise No_cross_product_sigls_possible)))
      end
    in 
    let reslsls_refined = reslsls |> filter (fun ls -> not (is_empty ls)) in 
    if debug then (printf "\n\n\n  >> Result of cross product:\n\t"; reslsls_refined |> iter Pp.pp_sigma_sigma_list; printf "\n\n\n"); 
      reslsls_refined

let exist_in_tbl (st: state) (sym: symbol) (tbl: ((state * symbol), sigma list list) Hashtbl.t): bool =
  (* Printf.printf "\n Is Symbol %s in tbl? \n" (fst sym); *)
  match Hashtbl.find_opt tbl (st, sym) with None -> (* Printf.printf "\tNO\n";*) false
  | Some _ -> (* Printf.printf "\tYES\n";*) true

let state_pair_append (st_pair: state * state): state = 
  let st1, st2 = (fst st_pair), (snd st_pair) in 
  if (st1 = epsilon_state) || (st2 = epsilon_state)
  then epsilon_state else st1 ^ st2

let state_pairs_equal (st_pair1: state * state) (st_pair2: state * state): bool = 
  (fst st_pair1) = (fst st_pair2) && (snd st_pair1) = (snd st_pair2)

let sig_pair_equals_state_pair (sig_pair: sigma * sigma) (st_pair: state * state): bool = 
  let st1, st2 = (fst st_pair), (snd st_pair) 
  in match sig_pair with 
    | Nt sig1, Nt sig2 -> (String.equal sig1 st1) && (String.equal sig2 st2)
    | _, _ -> false

let state_pairs_list_mem (st_pair: state * state) (st_pairs_ls: (state * state) list): bool =
  let comp_st1, comp_st2 = (fst st_pair), (snd st_pair) in
  let rec traverse_pairs ls =
    match ls with [] -> false
    | (st1, st2) :: tl ->
      if (st1 = comp_st1) && (st2 = comp_st2) then true else traverse_pairs tl
  in traverse_pairs st_pairs_ls

let cons_uniq xs x = if List.mem x xs then xs else x :: xs

let remove_dup_symbols (sym_ls: symbol list): symbol list = 
  List.rev (List.fold_left cons_uniq [] sym_ls)

let take_smaller_symbols_list (a1: symbol list) (a2: symbol list) (debug: bool): symbol list = 
  let check_subset_of_fst_in_snd (syms1: symbol list) (syms2: symbol list): symbol list = 
    let rec loop ls acc = 
      match ls with [] -> acc
      | hsym :: tl -> 
        if List.mem hsym syms1 then loop tl (hsym::acc) 
        else loop tl acc
          (* (Pp.pp_symbol hsym; raise Invalid_symbol_list) *)
    in loop syms2 []
  in 
  let a1_len, a2_len = List.length a1, List.length a2 in 
  let res = if (a1_len > a2_len) then check_subset_of_fst_in_snd a1 a2
            else check_subset_of_fst_in_snd a2 a1 in 
  if debug then (Printf.printf "\n\t >> Smaller symbols : "; res |> List.iter Pp.pp_symbol; Printf.printf "\n\n");res

let collect_raw_trans_for_states_pair (states_pair: state * state) (raw_trans_ls: ((state * state) * (symbol * (state * state) list)) list): 
  ((state * state) * (symbol * (state * state) list)) list =
  let rec loop ls acc = 
    match ls with [] -> List.rev acc
    | (st_pr, _) as raw_tran :: tl ->
      if state_pairs_equal states_pair st_pr then loop tl (raw_tran::acc) else loop tl acc
  in loop raw_trans_ls [] 

(* helper to collect RHS of raw transition list, ie, (sym, state pairs list) *)
let sym_and_rhs_sigma_pairs (raw_trans: ((state * state) * (symbol * (sigma * sigma) list)) list): 
  (symbol * (sigma * sigma) list) list =
  raw_trans |> List.map (fun (_, (s, rhs_sg_pair_ls)) -> (s, rhs_sg_pair_ls))

let same_sym_and_rhs_sigma_pairs (sym_rhs_stat_pairs1: (symbol * (sigma * sigma) list) list) 
  (sym_rhs_stat_pairs2: (symbol * (sigma * sigma) list) list): bool =
  if not ((List.length sym_rhs_stat_pairs1) = (List.length sym_rhs_stat_pairs2))
  then false
  else (let rec traverse ls = match ls with [] -> true 
        | sym_rhs_states :: tl -> 
          (if (List.mem sym_rhs_states sym_rhs_stat_pairs2)
           then traverse tl
           else false)
        in traverse sym_rhs_stat_pairs1)

let find_renamed_state (st_pair: state * state) 
  (renaming_map: ((state * state) * (state * state)) list): state * state = 
  match (List.assoc_opt st_pair renaming_map) with 
    | None -> 
      ((fst st_pair), "")
    | Some matched_sts -> matched_sts

let rename_trans_blocks (states_renaming_map: ((state * state) * (state * state)) list)
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
  let rec rename_raw_trans ls' acc' =
    match ls' with [] -> List.rev acc'
    | (lhs_st_pair, (sym, rhs_sig_pair_ls)) :: tl' -> 
      let lhs_renamed = find_renamed_state lhs_st_pair states_renaming_map in 
      let rhs_renamed: (sigma * sigma) list = 
        rhs_sig_pair_ls |> List.map (fun sig_pr -> 
          match sig_pr with 
          | Nt s1, Nt s2 -> let (new_st1, new_st2) = find_renamed_state (s1, s2) states_renaming_map in 
            (Nt new_st1, Nt new_st2)
          | T t1, T _t2 -> (T t1, T "")
          | _, _ -> raise Not_possible) in
      let renamed_tran = (lhs_renamed, (sym, rhs_renamed)) in 
      rename_raw_trans tl' (renamed_tran :: acc')
  in
  let rec rename_blocks_loop ls acc =
    match ls with [] -> List.rev acc 
    | (st_pair, trans_ls) :: tl -> 
      let renamed_raw_trans = rename_raw_trans trans_ls [] in 
      let to_acc = ((find_renamed_state st_pair states_renaming_map), renamed_raw_trans) in 
      rename_blocks_loop tl (to_acc :: acc)
  in let res_trans_blocks = rename_blocks_loop trans_blocks [] in 
  if debug then (Printf.printf "\n\t >> Results of renaming in trans in blocks : \n"; 
  res_trans_blocks |> Pp.pp_raw_trans_blocks);
  res_trans_blocks

let remove_dup_trans_for_each_block (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
  let rec remove_dup_trans ls' acc' = 
    match ls' with [] -> List.rev acc'
    | tran :: tl' -> 
      if (List.mem tran acc')
      then remove_dup_trans tl' acc'
      else remove_dup_trans tl' (tran::acc')
  in
  let rec remove_dups_blocks ls acc = 
    match ls with [] -> List.rev acc 
    | (st_pair, trans_ls) :: tl -> 
      let removed_dups_trans = remove_dup_trans trans_ls [] in 
      remove_dups_blocks tl ((st_pair, removed_dups_trans) :: acc)
  in let res_trans_blocks = remove_dups_blocks trans_blocks [] in 
  if debug then (Printf.printf "\n\t >> Results of removing duplicates in trans in blocks : \n"; 
  res_trans_blocks |> Pp.pp_raw_trans_blocks);
  res_trans_blocks

(* let remove_dup_trans_after_eps_intro (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list) 
  (debug: bool): ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
  let rec *)


let find_trans_block_for_states_pair (st_pair: (state * state)) 
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list):
  ((state * state) * (symbol * (sigma * sigma) list)) list =
  match List.assoc_opt st_pair trans_blocks with None -> raise Invalid_transitions
  | Some ls -> (Printf.printf "\n--- debug ----\n"; 
  ls |> List.iter (fun ((st1, st2), (sym, sig_sig_ls)) -> 
    Printf.printf "\n\t Sts %s %s ->_" st1 st2; Pp.pp_symbol sym; Pp.pp_sigma_sigma_list sig_sig_ls); ls)
  
let trans_mem (sym_rhs_sts: symbol * (sigma * sigma) list) (trans_rhs_lst: (symbol * (sigma * sigma) list) list): bool = 
  let exists_in_sigsigls (sig_pair: sigma * sigma) (sig_sig_ls: (sigma * sigma) list): bool = 
    let (fst_sig, snd_sig) = (fst sig_pair), (snd sig_pair) in 
    let rec loop ls ac = 
      match ls with [] -> ac
      | (s1, s2) :: tl -> 
        (if (sigmas_equal s1 fst_sig) && (sigmas_equal s2 snd_sig)
        then loop tl (true || ac)
        else loop tl ac)
    in loop sig_sig_ls false
  in
  let (sym, ssls) = (fst sym_rhs_sts), (snd sym_rhs_sts) in
  match Utils.sig_sig_assoc_all sym trans_rhs_lst with 
  | [] -> false
  | sig_sig_ls -> 
    ssls |> List.for_all (fun sig_pair -> (exists_in_sigsigls sig_pair sig_sig_ls))


let st1_transblock_subset_of_st2_transblock (st_pair1: (state * state)) (st_pair2: (state * state))
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): bool = 
  if debug then Printf.printf "\n\tIs (%s, %s) subset of (%s, %s)?\n" (fst st_pair1) (snd st_pair1) (fst st_pair2) (snd st_pair2);
  let st1_trans_rhs_lst: (symbol * (sigma * sigma) list) list =
    find_trans_block_for_states_pair st_pair1 trans_blocks |> List.map (fun (_, rhs_lst) -> rhs_lst) in
  let st2_trans_rhs_lst: (symbol * (sigma * sigma) list) list =
    find_trans_block_for_states_pair st_pair2 trans_blocks |> List.map (fun (_, rhs_lst) -> rhs_lst) in 
  let res_bool = 
    if (List.length st1_trans_rhs_lst) > (List.length st2_trans_rhs_lst)
    then false else 
    let rec traverse_rhs (ls: (symbol * (sigma * sigma) list) list): bool = 
      match ls with [] -> true
      | hsym_rhs_sts :: tl -> 
        Printf.printf "\n\t symbol %s and rhs \n" (fst (fst hsym_rhs_sts)); Pp.pp_sigma_sigma_list (snd hsym_rhs_sts);
        if (trans_mem hsym_rhs_sts st2_trans_rhs_lst) (*(List.mem hsym_rhs_sts st2_trans_rhs_lst)*)
        then (Printf.printf "\n\tYES MEM\n";traverse_rhs tl)
        else (Printf.printf "\n\tNO, NOT MEM\n"; false)
    in traverse_rhs st1_trans_rhs_lst 
  in if debug then (Printf.printf "\t %b" res_bool); 
  res_bool

let simplify_trans_blocks_with_epsilon_transitions 
  (input_trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list) 
  (st_pair_ls: (state * state) list) (debug: bool): 
  ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
  let replace_st1_transkblock_in_st2_transblock_with_eps (st1: state * state) (st2: state * state) 
    (blocks_ls: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list): 
    ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list = 
    let st1_sym_rhs_ls: (symbol * (sigma * sigma) list) list = 
      blocks_ls |> List.filter (fun (ss, _) -> (state_pairs_equal ss st1)) 
      |> List.hd |> snd |> List.map snd 
    in 
      blocks_ls |> List.map (fun ((st_pair, st_raw_trans_block): (state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) -> 
        if (state_pairs_equal st_pair st2)
        then 
          let new_st_raw_trans_block: ((state * state) * (symbol * (sigma * sigma) list)) list = 
            st_raw_trans_block |> List.filter (fun (_st_pair, sym_sig_sig_ls) -> not (List.mem sym_sig_sig_ls st1_sym_rhs_ls))
            |> List.append [(st2, (epsilon_symb, [(Nt (fst st1), Nt (snd st1))]))]
          in (st_pair, new_st_raw_trans_block)
        else 
          (st_pair, st_raw_trans_block))
  in 
  let rec simplify_trans_blocks (st_pair: state * state) (other_states_ls: (state * state) list) 
    (blocks_acc: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list): 
    ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
    match other_states_ls with [] -> blocks_acc
    | comp_st_pair :: tl -> 
      if (st1_transblock_subset_of_st2_transblock st_pair comp_st_pair blocks_acc debug)
      then (let new_trans_blocks = (replace_st1_transkblock_in_st2_transblock_with_eps st_pair comp_st_pair blocks_acc) in 
            let new_trans_blocks_wo_dups = remove_dup_trans_for_each_block new_trans_blocks false 
            in if debug then (Printf.printf "\n\tAfter Replacing State %s trans with State %s trans\n" (fst comp_st_pair) (fst st_pair);
            Pp.pp_raw_trans_blocks new_trans_blocks_wo_dups);
            simplify_trans_blocks st_pair tl new_trans_blocks_wo_dups)
      else (simplify_trans_blocks st_pair tl blocks_acc)
  in 
  let rec traverse_states (ls: (state * state) list) blocks_acc =
    match ls with [] -> blocks_acc
    | st_pair :: tl -> 
      (* Note: can consider 'tl' as other_states and do comparison assuming 'st_pair_ls' is ordered backward *)
      (* [prev, instead of tl] st_pair_ls |> List.filter (fun ss -> (not (state_pairs_equal ss st_pair))) *)
      let new_trans_blocks = simplify_trans_blocks st_pair tl blocks_acc in
      traverse_states tl new_trans_blocks
  in let simplified_res = traverse_states st_pair_ls input_trans_blocks in 
  if debug then (Printf.printf "\n\t >> Result of simplifying : \n"; simplified_res |> Pp.pp_raw_trans_blocks);
  simplified_res

let remove_meaningless_transitions (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list): 
  ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list = 
  let parenthesis_trans_to_itself (sym_sigsigls: (symbol * (sigma * sigma) list)) (st_pr: (state * state)) =
    let paren_to_sts (siglsls: (sigma * sigma) list) (sts: (state * state)) = match siglsls with 
    | ((T "LPAREN"), _) :: ((Nt sts'), _) :: ((T "RPAREN"), _) :: [] -> String.equal (fst sts) sts'
    | _ -> false in
    match sym_sigsigls with 
    | sym, siglsls -> 
      (syms_equals sym ("LPARENRPAREN", 1)) && (paren_to_sts siglsls st_pr)
  in
  let remove_meaningless_trans (blocks: ((state * state) * (symbol * (sigma * sigma) list)) list) =
    blocks |> List.filter (fun (sts, sym_sig_sig_ls) -> 
      not (parenthesis_trans_to_itself sym_sig_sig_ls sts))
  in
  trans_blocks |> List.map (fun (st_pair, blocks) -> st_pair, (remove_meaningless_trans blocks))

let optimize_sym_list (syms: symbol list) (eps_optimize: bool) (paren_optimize: bool) (debug: bool): symbol list = 
  let syms_opt1 = 
    if eps_optimize then syms |> List.filter (fun s -> not (syms_equals s epsilon_symb)) else syms in 
  let syms_opt2 = 
    if paren_optimize then syms_opt1 |> List.filter (fun s' -> not (syms_equals s' ("LPARENRPAREN", 1))) else syms_opt1 in 
  if debug then (Printf.printf "\n\t Symbols upon filtering out eps or () if needed : \n"; syms_opt2 |> List.iter Pp.pp_symbol); 
    syms_opt2

let optimize_sym_list_new (syms: symbol list) (eps_optimize: bool) (paren_optimize: bool) 
  (curr_o: int) (max_o: int) (debug: bool): symbol list = 
  let syms_opt1 = 
    if (curr_o = max_o) then (Printf.printf "\n\t Curr order = %d vs. Max order = %d\n" curr_o max_o;syms) else 
    if eps_optimize then syms |> List.filter (fun s -> not (syms_equals s epsilon_symb)) else syms in 
  let syms_opt2 = 
    if paren_optimize then syms_opt1 |> List.filter (fun s' -> not (syms_equals s' ("LPARENRPAREN", 1))) else syms_opt1 in 
  if debug then (Printf.printf "\n\t Symbols upon filtering out eps or () if needed : \n"; syms_opt2 |> List.iter Pp.pp_symbol); 
    syms_opt2

let ask_again (filename: string): unit = 
  Printf.printf "\nNew grammar is written on the file %s, but conflicts still exist. So, run 'make' again.\n\n" filename

let inform_user_of_new_grammar (filename: string): unit =
  Printf.printf "\nNew grammar is written on the file %s.\n\n" filename

let no_conflicts_message (filename: string): unit = 
  Printf.printf "\nThere are no ambiguities in %s.\n" filename

let success_message (cnt: int): unit =
  Printf.printf "\nGrammar disambiguation succeeded after %d rounds of user interaction\n\n " cnt

let run_again (filename: string): unit = 
  Printf.printf "\nNew grammar is written on the file %s. Run 'make' again.\n\n" filename

  (* "./test/grammars/G0/G0_results/G0a000.mly" in *)
let test_results_filepath (grammar: string) (postfix: string): string = 
  let except_for_last g = 
<<<<<<< HEAD
    let len = String.length grammar 
    in Str.string_before g (len-1) 
  in
  let grammar_type: string = except_for_last grammar in
  let path = "./test/grammars/" ^ grammar_type ^ "/" ^ grammar_type ^ "_results/" in 
  path ^ grammar ^ postfix ^ ".mly"
  
  (* (* let except_for_last g = 
    let len = String.length grammar 
    in Str.string_before g (len-1) 
  in *)
  (* let grammar_type: string = except_for_last grammar in *)
  (* let path = "/test/grammars/" ^ grammar_type ^ "/" ^ grammar_type ^ "_results/" in  *)
  grammar ^ "_" ^ postfix ^ ".mly" *)
=======
    let len = String.length grammar 
    in Str.string_before g (len-1) 
  in
  let grammar_type: string = except_for_last grammar in
  let path = "./test/grammars/" ^ grammar_type ^ "/" ^ grammar_type ^ "_results/" in 
  path ^ grammar ^ postfix ^ ".mly"
  (* grammar ^ "_" ^ postfix ^ ".mly" *)


let update_flag (current_ta: ta2) (triv_states: state list) (opt: optimization) (debug_print: bool): optimization = 
  (* traverse the TA and if encounter multiple eps transitions then turn on the eps_opt *)
  let only_triv_states (siglsls: sigma list list): bool = 
    siglsls |> List.fold_left (fun acc sigls -> 
      match sigls with 
      | Nt st' :: [] -> (List.mem st' triv_states) || acc
      | _ -> false || acc ) false
  in
  let coll_lst: sigma list list list = 
    Hashtbl.fold (fun (_lhs_nt, sy) siglsls acc -> 
      if (syms_equals sy epsilon_symb) && not (only_triv_states siglsls)
      then siglsls::acc else acc
      ) current_ta.transitions [] in
  if debug_print then (Printf.printf "\n\t\t Updating the flag!\n";
  coll_lst |> List.iter (fun x -> Pp.pp_sigma_listlist x));
    if ((List.length coll_lst) >= 3) && opt.onoff_opt 
  then { eps_opt = true; paren_opt = opt.paren_opt; triv_opt = opt.triv_opt; onoff_opt = opt.onoff_opt }
  else opt
>>>>>>> e3802eee547e5493584a32946f0dfc57ba186b3d
