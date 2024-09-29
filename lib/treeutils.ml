open Ta

exception Leaf_has_no_symbol
exception No_assoc_possible
exception No_prec_possible
exception Op_has_trivial_symbol

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
    (* *** debug *** *)
    let open Printf in 
    (printf "\n\nTree_to_expr "; Pp.pp_tree t; printf "\n\n");
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
      | "IF", 2 -> 
        ["("; s'] @ tree_loop (nth subts 0) @ ["THEN"] @ tree_loop (nth subts 1) @ [")"]
      | _, 2 -> 
        ["("] @ tree_loop (nth subts 0) @ [s'] @ tree_loop (nth subts 1) @ [")"]
      | "IF", 3 -> 
        ["("; s'] @ tree_loop (nth subts 0) @ ["THEN"] @ tree_loop (nth subts 1) @ ["ELSE"] @ tree_loop (nth subts 2) @ [")"]
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
in if debug_print then (Printf.printf "\nCollected O_a : "; Pp.pp_restriction_lst res); res

let collect_op_restrictions (example_trees: (string list * tree * (bool * bool) * restriction list) list) 
  (debug_print: bool): restriction list = 
  let res = example_trees 
    |> List.fold_left (fun acc (_, _, (_, op), rls) -> if op then rls @ acc else acc) [] 
  in if debug_print then (Printf.printf "\nCollected O_p : "; Pp.pp_restriction_lst res); res

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
  (if debug_print then Printf.printf "\nCombined O_p : "; Pp.pp_restriction_lst reordered_combined_op); 
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

let state_pair_append (st_pair: state * state): state = 
  let st1, st2 = (fst st_pair), (snd st_pair) in 
  if (st1 = epsilon_state) || (st2 = epsilon_state)
  then epsilon_state else st1 ^ st2

let state_pairs_equal (st_pair1: state * state) (st_pair2: state * state): bool = 
  (fst st_pair1) = (fst st_pair2) && (snd st_pair1) = (snd st_pair2)

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
      match ls with [] -> List.rev acc
      | hsym :: tl -> 
        if List.mem hsym syms1 then loop tl (hsym::acc) else raise Invalid_symbol_list
    in loop syms2 []
  in 
  let a1_len, a2_len = List.length a1, List.length a2 in 
  let res = if (a1_len > a2_len) then check_subset_of_fst_in_snd a1 a2
            else check_subset_of_fst_in_snd a2 a1 in 
  if debug then (Printf.printf "\n\t >> Smaller symbols : "; res |> List.iter Pp.pp_symbol);res

let collect_raw_trans_for_states_pair (states_pair: state * state) (raw_trans_ls: ((state * state) * (symbol * (state * state) list)) list): 
  ((state * state) * (symbol * (state * state) list)) list =
  let rec loop ls acc = 
    match ls with [] -> List.rev acc
    | (st_pr, _) as raw_tran :: tl ->
      if state_pairs_equal states_pair st_pr then loop tl (raw_tran::acc) else loop tl acc
  in loop raw_trans_ls [] 

(* helper to collect RHS of raw transition list, ie, (sym, state pairs list) *)
let sym_and_rhs_state_pairs (raw_trans: ((state * state) * (symbol * (state * state) list)) list): 
  (symbol * (state * state) list) list =
  raw_trans |> List.map (fun (_, (s, rhs_st_pair_ls)) -> (s, rhs_st_pair_ls))

let same_sym_and_rhs_state_pairs (sym_rhs_stat_pairs1: (symbol * (state * state) list) list) 
  (sym_rhs_stat_pairs2: (symbol * (state * state) list) list): bool =
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
      Printf.printf "\nWhich one?\n\t"; Pp.pp_raw_state st_pair;
      let epsilon_st_pair = (epsilon_state, epsilon_state) in
      if (state_pairs_equal st_pair epsilon_st_pair) then epsilon_st_pair else raise No_state_in_renaming_map 
    | Some matched_sts -> matched_sts

let rename_trans_blocks (states_renaming_map: ((state * state) * (state * state)) list)
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list)
  (debug: bool): ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list =
  let rec rename_raw_trans ls' acc' =
    match ls' with [] -> List.rev acc'
    | (lhs_st_pair, (sym, rhs_st_pair_ls)) :: tl' -> 
      let lhs_renamed = find_renamed_state lhs_st_pair states_renaming_map in 
      let rhs_renamed: (state * state) list = 
        rhs_st_pair_ls |> List.map (fun st_pr -> find_renamed_state st_pr states_renaming_map) in
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
 
let find_trans_block_for_states_pair (st_pair: (state * state)) (trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list):
  ((state * state) * (symbol * (state * state) list)) list =
  match List.assoc_opt st_pair trans_blocks with None -> raise Invalid_transitions
  | Some ls -> ls
  
let st1_transblock_subset_of_st2_transblock (st_pair1: (state * state)) (st_pair2: (state * state))
  (trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list)
  (debug: bool): bool = 
  if debug then Printf.printf "\n\tIs (%s, %s) subset of (%s, %s)?\n" (fst st_pair1) (snd st_pair1) (fst st_pair2) (snd st_pair2);
  let st1_trans_rhs_lst: (symbol * (state * state) list) list =
    find_trans_block_for_states_pair st_pair1 trans_blocks |> List.map (fun (_, rhs_lst) -> rhs_lst) in
  let st2_trans_rhs_lst: (symbol * (state * state) list) list =
    find_trans_block_for_states_pair st_pair2 trans_blocks |> List.map (fun (_, rhs_lst) -> rhs_lst) in 
  let res_bool = 
    if (List.length st1_trans_rhs_lst) > (List.length st2_trans_rhs_lst)
    then false else 
    let rec traverse_rhs (ls: (symbol * (state * state) list) list): bool = 
      match ls with [] -> true
      | hsym_rhs_sts :: tl -> 
        if (List.mem hsym_rhs_sts st2_trans_rhs_lst)
        then traverse_rhs tl
        else false
    in traverse_rhs st1_trans_rhs_lst 
  in if debug then (Printf.printf "\t %b" res_bool); 
  res_bool

let simplify_trans_blocks_with_epsilon_transitions 
  (input_trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list) 
  (st_pair_ls: (state * state) list) (debug: bool): 
  ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list =
  let replace_st1_transkblock_in_st2_transblock_with_eps (st1: state * state) (st2: state * state) 
    (blocks_ls: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list): 
    ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list = 
    let st1_sym_rhs_ls: (symbol * (state * state) list) list = 
      blocks_ls |> List.filter (fun (ss, _) -> (state_pairs_equal ss st1)) 
      |> List.hd |> snd |> List.map snd 
    in 
      blocks_ls |> List.map (fun (st_pair, st_block) -> 
        if (state_pairs_equal st_pair st2)
        then (let new_st_block = st_block |> List.filter 
                (fun (_, (sym_rhs_ls: symbol * (state * state) list)) -> not (List.mem sym_rhs_ls st1_sym_rhs_ls))
                |> List.append [(st2, (epsilon_symb, [st1]))] |> List.rev
              in (st_pair, new_st_block))
        else (st_pair, st_block))
  in 
  let rec simplify_trans_blocks (st_pair: state * state) (other_states_ls: (state * state) list) 
    (blocks_acc: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list): 
    ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list =
    match other_states_ls with [] -> blocks_acc
    | comp_st_pair :: tl -> 
      if (st1_transblock_subset_of_st2_transblock st_pair comp_st_pair input_trans_blocks debug)
      then (let new_trans_blocks = (replace_st1_transkblock_in_st2_transblock_with_eps st_pair comp_st_pair blocks_acc)
            in simplify_trans_blocks st_pair tl new_trans_blocks)
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


let raw_trans_in_blocks_to_trans (trans_blocks: ((state * state) * ((state * state) * (symbol * (state * state) list)) list) list) 
  (debug: bool): transition list =
  let convert_trans_block (block: ((state * state) * (symbol * (state * state) list)) list): transition list = 
    let rec convert_block_loop ls' acc' = 
      match ls' with [] -> acc'
      | (st_pr, (sym, rhs_ls)) :: tl' ->
        let lft_st_appnd = state_pair_append st_pr in
        let rhs_sts_appnd = rhs_ls |> List.map state_pair_append in 
        convert_block_loop tl' ((lft_st_appnd, (sym, rhs_sts_appnd))::acc')
    in convert_block_loop block []
  in 
  let rec convert_loop ls acc = 
    match ls with [] -> List.rev acc 
    | (_, st_block) :: tl -> 
      let converted_trans = convert_trans_block st_block 
      in convert_loop tl (converted_trans@acc)
  in let res_trans = convert_loop trans_blocks [] in 
  if debug then (Printf.printf "\n\t >> Result of converting raw trans to transitions : \n"; res_trans |> Pp.pp_transitions);
  res_trans

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
