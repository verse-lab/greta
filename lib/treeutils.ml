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

let wrapped_printf debug fmt =
  if debug then Printf.printf fmt
  else Printf.ifprintf stdout fmt

let cons_uniq xs x = if List.mem x xs then xs else x :: xs

let is_subset_of small_ls big_ls: bool =
  List.for_all (fun x -> List.mem x big_ls) small_ls

let remove_dup_symbols (sym_ls: symbol list): symbol list = 
  List.rev (List.fold_left cons_uniq [] sym_ls)

let rec chars_of_string ch: char list = 
  match ch with
  | "" -> []
  | ch -> String.get ch 0 :: chars_of_string (String.sub ch 1 (String.length ch - 1))

(* let find_order_of_symbol (tbl: (int, symbol list) Hashtbl.t) (sym: symbol): int =
  let res_opt = Hashtbl.fold
    (fun key sym_ls acc ->
       if List.mem sym sym_ls then Some key else acc)
    tbl None in 
  match res_opt with Some i -> i | None -> raise (Failure "find_order_of_symbol : no symbol in tbl") *)



let is_cond_state (s: state): bool =
  String.starts_with ~prefix:"con" s || String.starts_with ~prefix:"C" s

let trees_equal (e1: tree) (e2: tree) (debug_print: bool): bool =
  let booltostr x = if x then "true" else "false" in
  (wrapped_printf debug_print "\n  >> Are the following trees equal?\n\t";
  Pp.pp_tree e1; wrapped_printf debug_print "\n\t"; Pp.pp_tree e2); 
  let rec loop t1 t2 =
    match t1, t2 with
    | Leaf _, Node _ | Node _, Leaf _ -> false
    | Leaf v1, Leaf v2 -> v1 = v2
    | Node (sym1, subts1), Node (sym2, subts2) ->
      syms_equals sym1 sym2 && 
      List.fold_left2 (fun acc subt1 subt2 -> 
        acc && loop subt1 subt2) true subts1 subts2
  in let res = loop e1 e2 in 
  if debug_print then (wrapped_printf debug_print "\n  >> Result of equality:\t%s\n" (booltostr res)); 
  res

let is_leaf (t: tree): bool =
  match t with Leaf _ -> true
  | Node (_, _) -> false

(** return_state : return state (load of Leaf) *)
let return_state (t: tree): state =
  match t with Leaf v -> v
  | Node (_, _) -> "Error: Not a leaf"

let gen_state_list (sym_arity: int) (st: state): state list = 
  List.init sym_arity (fun _ -> st)

(* let gen_rhs_states (sym: symbol) (st: state): state list =
  let sym_arity = (arity_of_sym sym) in 
  if syms_equals sym (_, "IF", 2)
  then ["C"; st]
  else if syms_equals sym (_, "IF", 3)
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
    else stats *)

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
  | Node (s, _) -> (term_of_sym s)

let node_symbol_full (e: tree): symbol =
  match e with Leaf _ -> (-1, "no_symbol", 0)
  | Node (s, _) -> s

(* let change_node_symbol_with (e: tree) (sym: string): tree =
  match e with Leaf v -> Leaf v
  | Node ((_, ar), subts) -> Node ((sym, ar), subts) *)

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

(* let replace_node_wleaf (ts: tree list): tree list =
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
  in loop ts ("dummy") 0 [] *)

(** combine_trees_aux : combine 'up_t' as upper and 'lo_t' as lower trees *)
let combine_trees_aux (up_t: tree) (lo_t: tree): tree = 
  match up_t, lo_t with
  | Leaf _, _ | _, Leaf _ -> Leaf "dummy"
  | Node (up_sym, up_subts), Node (lo_sym, lo_subts) ->
    let last_ind = (List.length up_subts) - 1 in
    let up_subts_new = List.mapi (fun i subt -> 
      if (i = last_ind) then Node (lo_sym, lo_subts) else subt) up_subts in
    Node (up_sym, up_subts_new)

(** rename_states : rename states in ta_res from /\ *)
(* let rename_w_parser_friendly_states_in_ta (debug_print: bool) (inp_ta: ta): ta =
  let wrapped_printf debug_print fmt =
    if debug_print then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  (wrapped_printf debug_print "\nRename the following tree automaton:\n"; Pp.pp_ta inp_ta);
  let start_old, start_new = inp_ta.start_states, "expr1" in
  let states_mapping_init: (state * state) list = (start_old, start_new) :: [] in
  let eind, cind = ref 2, ref 1 in
  let states_mapping: (state * state) list = inp_ta.states |> List.fold_left (fun acc st_curr ->
    if (st_curr = start_old) then acc else if (st_curr = "ϵ") then (st_curr, st_curr)::acc else 
    if (is_cond_state st_curr) then let st_new = "cond_expr" ^ string_of_int !cind in cind := !cind+1; (st_curr, st_new)::acc 
    else let st_new = "expr" ^ string_of_int !eind in eind := !eind + 1; (st_curr, st_new) :: acc) 
    states_mapping_init in
  let replace_with_new (stat_old: state): state = match List.assoc_opt stat_old states_mapping with
    | Some v -> v | None -> raise (Failure "Old state is not found") in
  let finals_new = inp_ta.states |> List.map (fun st -> if (st = "ϵ") then st else replace_with_new st) in
  let trans_new = inp_ta.transitions |> List.map (fun (stlhs, (sym, stsrhs)) ->
    let stlhs_new = replace_with_new stlhs in 
    let stsrhs_new = stsrhs |> List.map replace_with_new in (stlhs_new, (sym, stsrhs_new))) in
  let ta_res: ta = {states=finals_new; alphabet=inp_ta.alphabet; final_states=[start_new]; terminals = []; transitions=trans_new; trivial_sym_nts=[]} in 
  if debug_print then (wrapped_printf "\nResult of renaming:\n"; Pp.pp_ta ta_res; wrapped_printf "\n");
  ta_res *)

(** tree_to_expr : helper to make the dual tree expression easier
 *                 works for the tree combining 2 trees each of which has height 1
 *                 needed to differentiate two expressions when they both use the same operator
 *                 e.g., (expr + expr) + expr   vs.   expr + (expr + expr) *)
let tree_to_expr (t: tree) : string list =
  let open List in
  (* 
    (* *** debug *** *)
    let open Printf in 
    (wrapped_printf "\n\n\t\t >> Tree_to_expr "; Pp.pp_tree t; wrapped_printf "\n\n");
  *)
  let is_empty_leaf (ts: tree list) =
    match (hd ts, length ts) with (Leaf "ϵ"), 1 -> true | _ -> false in
  let rec tree_loop t: string list =
    match t with Leaf s -> [s]
    | Node (sym, subts) ->
      let s', rnk = (term_of_sym sym), (arity_of_sym sym) in 
      match s', rnk with 
      | _, 0 -> 
        if is_empty_leaf subts then [s'] 
        else raise (Failure "tree_to_expr : no trivial case should be presented as a tree")
      | "LPAREN", 3 -> 
        ["("; "LPAREN"] @ tree_loop (nth subts 1) @ ["RPAREN"; ")"]
      | "LBRACER", 3 -> 
        ["("; "LBRACE"] @ tree_loop (nth subts 1) @ ["RBRACE"; ")"]
        (* TODO: Make the rank below to be generalizable *)
      | _, 2 -> 
        ["("] @ tree_loop (nth subts 0) @ tree_loop (nth subts 1) @ [")"] (* prev version @ [s'] @ tree_loop ... *)
      | _s, _ -> 
        let treeexprs_for_subts = subts |> fold_left (fun acc t -> acc @ tree_loop t) [] in 
        ["("] @ treeexprs_for_subts @ [")"] (* prev version ["("; s] @ *)
  in tree_loop t

let present_tree_pair (trees: tree * tree): unit =
  let open Printf in
  printf "\n\nChoose your preference! \n(Type either 0 or 1.)\n\n";
  let print_strs ls = printf "\t"; ls |> List.iter (printf "%s "); printf "\n" in
  let expr1 = fst trees |> tree_to_expr in
  let expr2 = snd trees |> tree_to_expr in (* [fix] instead of 'gen_dual_expr' *)
  printf "Option 0: \n"; print_strs expr1;
  printf "Option 1: \n"; print_strs expr2; printf "\n"

let same_syms ls = 
  let fst_sym = List.hd ls in 
  let filtered = List.filter (fun s -> not (syms_equals s fst_sym)) ls in
  if filtered = [] then true else false

let subtrees_of (e:tree): tree list = 
  match e with Leaf _ -> []
  | Node (_, subts) -> subts

let tree_symbol (e: tree): symbol = 
  match e with Leaf _ -> (-1, "dummy", -1) (*raise Leaf_has_no_symbol*)
  | Node (s, _) -> s

let find_index_subt_with_same_sym (sym: symbol) (subts: tree list): int = 
  let count: int ref = ref 0 in 
  subts |> List.iteri (fun i subt -> 
    let curr_t_sym = tree_symbol subt in
    if (syms_equals curr_t_sym sym) then count := i); !count

let collect_syms (e: tree): symbol list =
  let rec collect_loop t acc = 
    match t with Leaf _ -> acc
    | Node (s, subts) -> 
      let syms = subts |> List.fold_left (fun acc' subt -> 
        (collect_loop subt []) @ acc') []
      in s::acc @ syms
  in collect_loop e []

let is_oa_tree (t: tree): bool = 
  let t_syms: symbol list = collect_syms t 
  in same_syms t_syms
  
(* check_oa_op results in (oa_pos, oa_neg, op) *)
let check_oa_op (e: tree): bool * bool * bool = 
  if (is_oa_tree e) then (true, true, false) else (false, false, true)

let collect_oa_restrictions (example_trees: (string list * tree * (bool * bool) * restriction list) list) 
(debug_print: bool): restriction list = 
  let res = example_trees 
    |> List.fold_left (fun acc (_, _, (oa, _), rls) -> if oa then rls @ acc else acc) [] 
  in if debug_print then (wrapped_printf debug_print "\n  Collected O_a : "; Pp.pp_restriction_lst res); res

let update_if_exist_overlapping_symbols_w_diff_order (so_ls: (symbol * int) list) 
  (acc_ls: (symbol * int) list list) (debug: bool): (symbol * int) list list = 

  let flattened_acc: (symbol * int) list = acc_ls |> List.flatten in
  let syms_ls: symbol list = flattened_acc |> List.map fst in
  let find_ord s': int = List.assoc s' flattened_acc in
  let rec loop (ls: (symbol * int) list) (acc: (symbol * int) list list) = 
    match ls with [] -> acc 
    | (s, o) :: so_tl -> 
      if List.mem s syms_ls then 
        (let ord = find_ord s in 
        if (ord = o) then 
          (if debug then wrapped_printf debug "\n\t Exist in acc so list but same order so leave intact\n"; loop so_tl acc)
        else 
          (* TODO: change logic here! *)
          (if debug then wrapped_printf debug "\n\t"; loop so_tl acc)) 
      else 
        (if debug then wrapped_printf debug "\n\t Not exist in acc so list\n"; loop so_tl acc)
  in loop so_ls acc_ls 

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
  (if debug_print then wrapped_printf debug_print "\n  Combined O_p : "; Pp.pp_restriction_lst reordered_combined_op); 
  reordered_combined_op

let sort_by_fst_element ls =
  List.sort (fun (f1, _) (f2, _) -> Int.compare f1 f2) ls

let sym_of_op_restriction (rest: restriction): symbol = 
  match rest with Assoc _ -> raise (Failure "sym_of_op_restriction : No assoc possible")
  | Prec (s, _o) -> s

let sym_of_oa_restriction (rest: restriction): symbol = 
  match rest with Assoc (s, _) -> s
  | Prec _ -> raise (Failure "sym_of_oa_restriction : No Prec possible")

(* orders_of_sym_in_op_tbl : find all the orders associated with the symbol *)
let orders_of_sym_in_op_tbl (sym: symbol) (_sym: symbol) (op_tbl: (int, symbol list) Hashtbl.t ) (debug: bool): int list = 
  let orders_res = ref [] in 
  op_tbl |> Hashtbl.iter (fun o sym_ls -> 
    if (List.mem sym sym_ls) then (orders_res := o::!orders_res)); 
  if debug then (wrapped_printf debug "\n  Orders of "; Pp.pp_symbol sym; 
    (if (not (is_dummy_sym _sym)) then Pp.pp_symbol _sym); 
    wrapped_printf debug " [ "; !orders_res |> List.iter (fun x -> wrapped_printf debug " %d " x); 
    wrapped_printf debug " ] \n\n"); !orders_res

let sym_top_sym_bot_of_restrictions (r1: restriction) (r2: restriction) (debug: bool): (symbol * symbol) = 
  let (sym_top, sym_bot) = 
    match r1, r2 with 
    | Prec (sym1, o1), Prec (sym2, o2) -> 
      (* order higher means it's sym_bot -> returning (sym_top, sym_bot) pair *)
      if (o1 > o2) then (sym2, sym1) else (sym1, sym2)
    | Assoc _, Prec _ | Prec _, Assoc _ | Assoc _, Assoc _ -> raise (Failure "")
  in if debug then (wrapped_printf debug "\t  Sym_top : "; Pp.pp_symbol sym_top; 
    wrapped_printf debug "\n\t  Sym_bot : "; Pp.pp_symbol sym_bot); 
  (sym_top, sym_bot)

let move_keys_if (tbl: (int, symbol list) Hashtbl.t) (threshold: int): unit =
  let to_move: (int * int * symbol list) list =
    Hashtbl.fold (fun k sls acc ->
      if k >= threshold then (k, k + 1, sls) :: acc else acc
    ) tbl []
  in
  (* Remove all old keys *)
  List.iter (fun (oldk, _, _) -> Hashtbl.remove tbl oldk) to_move;
  (* Insert all new keys *)
  List.iter (fun (_, newk, sls) -> Hashtbl.replace tbl newk sls) to_move

let push_keys_if_gte_order (ord: int) (tbl: (int, symbol list) Hashtbl.t): unit = 
  move_keys_if tbl ord 

let remove_sym_at_lvl (tbl: (int, symbol list) Hashtbl.t) (lvl: int) (sym: symbol): unit =
  match Hashtbl.find_opt tbl lvl with
  | None -> () 
  | Some sym_ls ->
      let new_sym_ls = List.filter (fun s -> not (syms_equals sym s)) sym_ls in
      Hashtbl.replace tbl lvl new_sym_ls

(* Update_op_tbl_per_op_syms : update based on syms wrt. O_p *)
let update_op_tbl_per_op_syms (sym_top: symbol) (sym_bot: symbol) (ord: int) (op_tbl: (int, symbol list) Hashtbl.t) (debug: bool):
  (int, symbol list) Hashtbl.t =
  
  (* 0. Store the current ord -> symbol list in a temporary list *)
  let temp_ord_symbols: symbol list = 
    Hashtbl.find op_tbl ord 
  in 
  (* 1. Push levels >= ord by one and their values (symbol list) also get moved accordingly *)
  push_keys_if_gte_order ord op_tbl;
  (* (if debug then wrapped_printf debug "\n\t  Pushed levels >= ord %d : " ord; 
    Pp.pp_obp_tbl op_tbl); *)

  (* 2. Insert the copied symbols in 'temp_ord_symbols' back at level 'ord' *)
  Hashtbl.add op_tbl ord temp_ord_symbols;
  (* (if debug then wrapped_printf debug "\n\t  Inserted at ord %d : " ord; 
    Pp.pp_obp_tbl op_tbl); *)

  (* 3. Remove sym_bot at Level 'ord' *)
  remove_sym_at_lvl op_tbl ord sym_bot;

  (* 4. Remove sym_top at Level 'ord + 1' *)
  remove_sym_at_lvl op_tbl (ord+1) sym_top;

  if debug then (wrapped_printf debug "\n\t  Updated O_p tbl for symbols : "; 
    Pp.pp_symbol sym_top; Pp.pp_symbol sym_bot; wrapped_printf debug "\n"; Pp.pp_obp_tbl op_tbl); 
  op_tbl


(* Update_op_tbl_per_oa_syms : update based on syms wrt. O_a *)
let update_op_tbl_per_oa_sym (oa_sym: symbol) (ord: int) (op_tbl: (int, symbol list) Hashtbl.t) (debug: bool): 
  (int, symbol list) Hashtbl.t =
  
  (* 0. Store the current ord -> symbol list in a temporary list *)
  let temp_ord_symbols: symbol list = 
    Hashtbl.find op_tbl ord
  in 

  (* 1. Store S \ sym - symbols of 'ord' without 'oa_sym' *)
  let ord_symbols_without_oa_sym: symbol list = 
    temp_ord_symbols |> List.filter (fun x -> not (syms_equals x oa_sym))
  in 
  
  (* 2. Store symbols of 'ord + 1' *)
  let ord_plus_one_symbols: symbol list = 
    match (Hashtbl.find_opt op_tbl (ord+1)) with Some sls -> sls | None -> []
  in

  (* 3. Does symbols of 'ord + 1' have all the 'ord_symbols_without_curr_sym'? *)
  if (is_subset_of ord_symbols_without_oa_sym ord_plus_one_symbols) 
  then 
    (if debug (* 3.1. If it does, then no need to change the current Op_tbl *)
     then (wrapped_printf debug "\n\t 'order+1' symbols include all the symbols S \ sym_oa \n")) 
  else 
    (* 3.2. Otherwise, do the following steps *)
    begin 
      (* 4. Push levels >= ord by one and their valies (symbol list) also get moved accordingly *)
      push_keys_if_gte_order ord op_tbl;
      (* (if debug then wrapped_printf debug "\n\t  Pushed levels >= ord %d : " ord; 
        Pp.pp_obp_tbl op_tbl); *)
      
      (* 5. Insert `temp_ord_symbols` at 'ord' *)
      Hashtbl.add op_tbl ord temp_ord_symbols;

      (* 6. Remove 'oa_sym' at 'ord+1' *)
      remove_sym_at_lvl op_tbl (ord+1) oa_sym;
    end;
  if debug then (wrapped_printf debug "\n\t  Updated O_p tbl for symbol : "; 
    Pp.pp_symbol oa_sym; wrapped_printf debug "\n"; Pp.pp_obp_tbl op_tbl);
  op_tbl



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

(* 
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
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  wrapped_printf "\nFinding transition starting from %s for symbol (%s, %i) ..\n" st (fst sym) (snd sym);
  let rec interm_sts_loop (ls': state list) (rhs_sts_ls: state list): state list * state list = 
    match ls' with 
    | [] -> rhs_sts_ls, []
    | interm_hd :: interm_tl -> 
      wrapped_printf "\n\tNow looking for RHS states starting from %s for symbol (%s, %i)\n" interm_hd (fst sym) (snd sym);
      let i_rhs, next_interm_ls = find_rhs_states trans_ls interm_hd []
      in if (i_rhs = []) then interm_sts_loop interm_tl [] else i_rhs, next_interm_ls
  and 
  find_rhs_states (ls: transition list) (from_st: state) (interm_sts: state list): state list * state list =
    match ls with 
    | [] ->
      if interm_sts = []
      then [], []
      else (wrapped_printf "\n\tInterm states not empty: "; Pp.pp_states interm_sts; 
           interm_sts_loop interm_sts [])
    | (lft_st, (s, rhs_sts)) :: tl -> 
      if (lft_st = from_st) && (syms_equals s sym)
      then rhs_sts, interm_sts
      else if (lft_st = from_st) && (syms_equals s epsilon_symb)
      then (let next_st = (List.hd rhs_sts) (* if eps-trans, save in 'interm_sts' and traverse til the end *)
            in find_rhs_states tl from_st (next_st::interm_sts))
      else find_rhs_states tl from_st interm_sts
  in let res_rhs_sts, _ (* res_interm_sts*) = find_rhs_states trans_ls st [] in
  (if debug then wrapped_printf "\n\t\t >> .. Found RHS states: "; Pp.pp_states res_rhs_sts);
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
  | Term t1 :: stl1, Term t2 :: stl2 -> cross_product_siglsls stl1 stl2 triv_states ((Term t1, Term t2)::acc)
  | Nt nt1 :: stl1, Nt nt2 :: stl2 -> 
    if ((equal nt1 epsilon_state) && (not (List.mem nt2 triv_states))) || ((equal nt2 epsilon_state) && (not (List.mem nt1 triv_states)))
    then cross_product_siglsls stl1 stl2 triv_states ((Nt epsilon_state, Nt epsilon_state)::acc) 
    else 
      if ((List.mem nt1 triv_states) && not (List.mem nt2 triv_states)) || ((List.mem nt2 triv_states) && not (List.mem nt1 triv_states))
      then cross_product_siglsls stl1 stl2 triv_states acc
      else cross_product_siglsls stl1 stl2 triv_states ((Nt nt1, Nt nt2)::acc)
  | (Term _)::_, (Nt _)::_ | (Nt _)::_, (Term _)::_ | _, [] | [], _ -> raise No_cross_product_sigls_possible

let cross_product_raw_sigma_lsls (sig_lsls_ls1: (sigma list list) list) (sig_lsls_ls2: (sigma list list) list) 
  (triv_states: state list) (debug: bool): (sigma * sigma) list list =
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let open List in
  if debug then (wrapped_printf "\n\tFinding cross product of sigma_lsls's first sig_lsls : \n"; 
    sig_lsls_ls1 |> List.iter Pp.pp_sigma_listlist; wrapped_printf "\n\tThen second sig_lsls : \n"; sig_lsls_ls2 |> List.iter Pp.pp_sigma_listlist);
  (* --- helper --- *)
  let rec cross_loop lsls1 lsls2 (acc: (sigma * sigma) list list) = 
    match lsls1 with [] -> acc
    | sig_ls_hd1 :: tl1 -> 
      let sig_ls2: sigma list = find_corresponding_sigls sig_ls_hd1 lsls2 triv_states in
      (if debug then wrapped_printf "\n\t --> corresponding sigma list: \t"; Pp.pp_sigma_list sig_ls2);
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
                wrapped_printf "\n\t\t What's new len(sig_lsls_ls1) %d vs. leng(sig_lsls_ls2) %d\n" len1' len2';
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
                  (wrapped_printf"\n\tCross product bug position!\n";
                   raise No_cross_product_sigls_possible)))
      end
    in 
    let reslsls_refined = reslsls |> filter (fun ls -> not (is_empty ls)) in 
    if debug then (wrapped_printf "\n\n\n  >> Result of cross product:\n\t"; reslsls_refined |> iter Pp.pp_sigma_sigma_list; wrapped_printf "\n\n\n"); 
      reslsls_refined *)

let exist_in_tbl (st: state) (sym: symbol) (tbl: ((state * symbol), sigma list list) Hashtbl.t): bool =
  (* wrapped_printf "\n Is Symbol %s in tbl? \n" (fst sym); *)
  match Hashtbl.find_opt tbl (st, sym) with None -> (* wrapped_printf "\tNO\n";*) false
  | Some _ -> (* wrapped_printf "\tYES\n";*) true

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

(* 
let take_smaller_symbols_list (a1: symbol list) (a2: symbol list) (debug: bool): symbol list =
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in
  
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
  if debug then (wrapped_printf "\n\t >> Smaller symbols : "; res |> List.iter Pp.pp_symbol; wrapped_printf "\n\n");res

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
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let rec rename_raw_trans ls' acc' =
    match ls' with [] -> List.rev acc'
    | (lhs_st_pair, (sym, rhs_sig_pair_ls)) :: tl' -> 
      let lhs_renamed = find_renamed_state lhs_st_pair states_renaming_map in 
      let rhs_renamed: (sigma * sigma) list = 
        rhs_sig_pair_ls |> List.map (fun sig_pr -> 
          match sig_pr with 
          | Nt s1, Nt s2 -> let (new_st1, new_st2) = find_renamed_state (s1, s2) states_renaming_map in 
            (Nt new_st1, Nt new_st2)
          | Term t1, Term _t2 -> (Term t1, Term "")
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
  if debug then (wrapped_printf "\n\t >> Results of renaming in trans in blocks : \n"; 
  res_trans_blocks |> Pp.pp_raw_trans_blocks);
  res_trans_blocks

let remove_dup_trans_for_each_block (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list)
  (debug: bool): ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

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
  if debug then (wrapped_printf "\n\t >> Results of removing duplicates in trans in blocks : \n"; 
  res_trans_blocks |> Pp.pp_raw_trans_blocks);
  res_trans_blocks

(* let remove_dup_trans_after_eps_intro (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list) 
  (debug: bool): ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
  let rec *)


let find_trans_block_for_states_pair (st_pair: (state * state)) 
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list):
  ((state * state) * (symbol * (sigma * sigma) list)) list =
  match List.assoc_opt st_pair trans_blocks with None -> raise Invalid_transitions
  | Some ls -> ls
  
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
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  wrapped_printf "\n\tIs (%s, %s) subset of (%s, %s)?\n" (fst st_pair1) (snd st_pair1) (fst st_pair2) (snd st_pair2);
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
        wrapped_printf "\n\t symbol %s and rhs \n" (fst (fst hsym_rhs_sts)); Pp.pp_sigma_sigma_list (snd hsym_rhs_sts);
        if (trans_mem hsym_rhs_sts st2_trans_rhs_lst) (*(List.mem hsym_rhs_sts st2_trans_rhs_lst)*)
        then (wrapped_printf "\n\tYES MEM\n";traverse_rhs tl)
        else (wrapped_printf "\n\tNO, NOT MEM\n"; false)
    in traverse_rhs st1_trans_rhs_lst 
  in (wrapped_printf "\t %b" res_bool); 
  res_bool

let simplify_trans_blocks_with_epsilon_transitions 
  (input_trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list) 
  (st_pair_ls: (state * state) list) (debug: bool): 
  ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list =
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

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
            in (wrapped_printf "\n\tAfter Replacing State %s trans with State %s trans\n" (fst comp_st_pair) (fst st_pair);
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
  if debug then (wrapped_printf "\n\t >> Result of simplifying : \n"; simplified_res |> Pp.pp_raw_trans_blocks);
  simplified_res

let remove_meaningless_transitions (trans_blocks: ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list): 
  ((state * state) * ((state * state) * (symbol * (sigma * sigma) list)) list) list = 
  let parenthesis_trans_to_itself (sym_sigsigls: (symbol * (sigma * sigma) list)) (st_pr: (state * state)) =
    let paren_to_sts (siglsls: (sigma * sigma) list) (sts: (state * state)) = match siglsls with 
    | ((Term "LPAREN"), _) :: ((Nt sts'), _) :: ((Term "RPAREN"), _) :: [] -> String.equal (fst sts) sts'
    | _ -> false in
    match sym_sigsigls with 
    | sym, siglsls -> 
      (syms_equals sym ("LPAREN", 3)) && (paren_to_sts siglsls st_pr)
  in
  let remove_meaningless_trans (blocks: ((state * state) * (symbol * (sigma * sigma) list)) list) =
    blocks |> List.filter (fun (sts, sym_sig_sig_ls) -> 
      not (parenthesis_trans_to_itself sym_sig_sig_ls sts))
  in
  trans_blocks |> List.map (fun (st_pair, blocks) -> st_pair, (remove_meaningless_trans blocks))

let optimize_sym_list (syms: symbol list) (eps_optimize: bool) (paren_optimize: bool) (debug: bool): symbol list = 
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let syms_opt1 = 
    if eps_optimize then syms |> List.filter (fun s -> not (syms_equals s epsilon_symb)) else syms in 
  let syms_opt2 = 
    if paren_optimize then syms_opt1 |> List.filter (fun s' -> not (syms_equals s' ("LPAREN", 3))) else syms_opt1 in 
  if debug then (wrapped_printf "\n\t Symbols upon filtering out eps or () if needed : \n"; syms_opt2 |> List.iter Pp.pp_symbol); 
    syms_opt2

let optimize_sym_list_new (syms: symbol list) (eps_optimize: bool) (paren_optimize: bool) 
  (curr_o: int) (max_o: int) (debug: bool): symbol list = 
  let wrapped_printf fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let syms_opt1 = 
    if (curr_o = max_o) then (wrapped_printf "\n\t Curr order = %d vs. Max order = %d\n" curr_o max_o;syms) else 
    if eps_optimize then syms |> List.filter (fun s -> not (syms_equals s epsilon_symb)) else syms in 
  let syms_opt2 = 
    if paren_optimize then syms_opt1 |> List.filter (fun s' -> not (syms_equals s' ("LPAREN", 3))) else syms_opt1 in 
  if debug then (wrapped_printf "\n\t Symbols upon filtering out eps or () if needed : \n"; syms_opt2 |> List.iter Pp.pp_symbol); 
    syms_opt2 *)

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
let test_results_filepath (grammar: string) (postfix: string): string = grammar ^ "_" ^ postfix ^ ".mly"

