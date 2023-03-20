open Ta

(** is_cond_expr : check if state is representing boolean state *)
let is_cond_expr (s: state): bool =
  let open Str in
  string_match (regexp "cond") s 0 || string_match (regexp "Cond") s 0  

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
let rename_states (debug_print: bool) (inp_ta: ta): ta =
  let open Printf in
  if debug_print then (printf "\nRename the following tree automaton:\n"; Pp.pp_ta inp_ta);
  let start_old, start_new = inp_ta.start_state, "expr1" in
  let states_mapping_init: (state * state) list = (start_old, start_new) :: [] in
  let eind, cind = ref 2, ref 1 in
  let states_mapping: (state * state) list = inp_ta.states |> List.fold_left (fun acc st_curr ->
    if (st_curr = start_old) then acc else if (st_curr = "ϵ") then (st_curr, st_curr)::acc else 
    if (is_cond_expr st_curr) then let st_new = "cond_expr" ^ string_of_int !cind in cind := !cind+1; (st_curr, st_new)::acc 
    else let st_new = "expr" ^ string_of_int !eind in eind := !eind + 2; (st_curr, st_new) :: acc) 
    states_mapping_init in
  let replace_with_new (stat_old: state): state = match List.assoc_opt stat_old states_mapping with
    | Some v -> v | None -> raise (Failure "Old state is not found") in
  let states_new = inp_ta.states |> List.map (fun st -> if (st = "ϵ") then st else replace_with_new st) in
  let trans_new = inp_ta.transitions |> List.map (fun (stlhs, (sym, stsrhs)) ->
    let stlhs_new = replace_with_new stlhs in 
    let stsrhs_new = stsrhs |> List.map replace_with_new in (stlhs_new, (sym, stsrhs_new))) in
  let ta_res = {states=states_new; alphabet=inp_ta.alphabet; start_state=start_new; transitions=trans_new} in 
  if debug_print then (printf "\nResult of renaming:\n"; Pp.pp_ta ta_res; printf "\n");
  ta_res

(** tree_to_expr : helper to make the dual tree expression easier
 *                 works for the tree combining 2 trees each of which has height 1
 *                 needed to differentiate two expressions when they both use the same operator
 *                 e.g., (expr + expr) + expr   vs.   expr + (expr + expr) *)
let tree_to_expr (t: tree): string list =
  let open List in
  let is_empty_leaf (ts: tree list) =
    match (hd ts, length ts) with (Leaf "ϵ"), 1 -> true | _ -> false in
  let rec tree_loop t: string list =
    match t with Leaf s -> [s]
    | Node (sym, subts) ->
      let s', rnk = fst sym, (length subts) in
      if (rnk = 0 && is_empty_leaf subts) then [s']
      else if (rnk = 2 && s' = "IF") then
        (["("; s'] @ tree_loop (nth subts 0) @ 
        ["THEN"] @ tree_loop (nth subts 1) @ [")"])
      else if (rnk = 2 && not (s' = "IF")) then
        (["("] @ tree_loop (nth subts 0) @ 
        [s'] @ tree_loop (nth subts 1) @ [")"])
      else if (rnk = 1 && s' = "LPARENRPAREN") then 
        (["("; "LPAREN"] @ tree_loop (nth subts 1) @ ["RPAREN"; ")"])
      else if (rnk = 3 && s' = "IF") then 
        (["("; s'] @ tree_loop (nth subts 0) @ 
        ["THEN"] @ tree_loop (nth subts 1) @
        ["ELSE"] @ tree_loop (nth subts 2) @ [")"])
      else raise (Failure "Node with a rank other than 1, 2 or 3!")
  in tree_loop t

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

let present_tree_pair (trees: tree * tree): unit =
  let open Printf in
  printf "\n\nChoose your preference! \n(Type either 0 or 1.)\n\n";
  let print_strs ls = printf "\t"; ls |> List.iter (printf "%s "); printf "\n" in
  let expr1 = fst trees |> tree_to_expr in
  let expr2 = gen_dual_expr expr1 in
  printf "Option 0: \n"; print_strs expr1;
  printf "Option 1: \n"; print_strs expr2; printf "\n"

let present_tree_pair_single_operator (trees: tree * tree): unit =
  let open Printf in
  printf "\n\nChoose your preference! \n(Type either 0, 1 or 2.)\n\n";
  let print_strs ls = printf "\t"; ls |> List.iter (printf "%s "); printf "\n" in 
  let expr1 = fst trees |> tree_to_expr in 
  let expr2 = gen_dual_expr expr1 in 
  printf "Option 0: \n"; print_strs expr1;
  printf "Option 1: \n"; print_strs expr2;
  printf "Option 2: \n\tNo preference"; printf "\n\n"

let ask_again (filename: string): unit = 
  Printf.printf "\nNew grammar is written on the file %s, but conflicts still exist. So, run 'make' again.\n\n" filename

let tree_with_single_operator (e: tree): bool =
  let fst_sym = node_symbol_full e in
  let bool_ls ls = List.fold_left (fun x acc -> x && acc) true ls in
  let rec traverse t prev_sym res = 
    match t with Leaf _ -> res
    | Node (sym, subts) -> subts 
      |> List.map (fun t -> traverse t sym ((syms_equals sym prev_sym) && res))
      |> bool_ls
  in traverse e fst_sym true


