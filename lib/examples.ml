open Ta
open Treeutils

module D = Draw

let ex03 = Node (("+", 2), [Leaf "expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
let t03: tree = Node (("+", 2), [ Node (("+", 2),  [Leaf "expr2" ;  Leaf "@expr2" ]);  Leaf "expr2" ])
let ex04 = Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
let ex04_neg = Node (("+", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])])
let ex05 = Node (("IF", 2), [Leaf "cond_expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])

(* 
  Steps to generate example trees based on conflicts:
  1. Traverse conflicts file and collect relevant sub-derivation lines
  2. Extract list of tree exprs from the relevant lines 
  3. 
*)

exception Invalid_number_of_trees
exception Leaf_is_not_valid
exception Invalid_subtrees
exception Tree_specifies_oa_or_op
exception Neither_left_nor_right

(** gen_examples : gen examples from parser.conflicts in CFG *)
let gen_examples (filename: string) (a: symbol list) (debug_print: bool): 
  ((string list * tree * (bool * bool) * restriction list) * (string list * tree * (bool * bool) * restriction list)) list = 
  let open List in
  let open String in
  let open Printf in
  let a_new = a |> List.map (fun (sym, ar) -> 
    match sym with "+" -> ("PLUS", ar) | "*" -> ("MUL", ar) | "()" -> ("LPARENRPAREN", ar)
    | rest -> (rest, ar)) in
  let syms_ls: string list = a_new |> List.map fst in
  printf "\nGenerate examples from conflicts in file %s\n" filename; 
  if debug_print then printf "\tGiven alphabet: "; syms_ls |> List.iter (printf "%s ");
  (** helpers *)
  let ic = open_in filename in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let starts str lin = starts_with ~prefix:str lin in
  (* let arity_of_sym sym: int = match List.assoc_opt sym a_new with 
    | None -> printf "Symbol %s not found in alphabet" sym; -1 | Some n -> n in *)
  let is_in_alphabet s: bool = List.mem s syms_ls in
  (** traverse and acc relevant lines for generating trees *)
  let rec traverse (cnt: int) (count_started: bool) (res_acc: string list): string list =
    let relev_point1 lin = starts "** because of the following sub-derivation:" lin in
    let relev_point2 lin = starts "** is permitted because of the following sub-derivation:" lin in
    let collect_here num: bool = (num mod 3) = 0 
    in match try_read () with 
    | None -> (close_in ic; List.rev res_acc)
    | Some s -> 
      (* if 'count_started' && 'collect_here' then collect *)
      if count_started 
      then (if (collect_here cnt) 
            then traverse 1 false (s::res_acc)
            else traverse (cnt+1) count_started res_acc)
      (* if 'count_started' but not 'collect_here', add counter and traverse *)
      else if (count_started && not (collect_here cnt)) (* [List.rev lins_acc] @ *)
      then traverse (cnt+1) count_started res_acc
      (* if the line is 'relev_point', send signal wrt 'count_started' *)
      else if (relev_point1 s) || (relev_point2 s)
      then traverse cnt true res_acc
      else traverse cnt count_started res_acc
      (* if lines need to be read, turn on flag 'can_acclines' *)
  in
  (** convert_to_tree convert an expression (string list) ["expr"; "sym"; "@expr"] 
      to a tree (Node sym, [Leaf "expr"; ...]) *)
  let convert_to_tree_exprs (str_ls: string list): tree = 
    let rec conv_loop ls nodsym_acc subtrees_acc = 
      match ls with [] -> Node (nodsym_acc, List.rev subtrees_acc)
      | (sh: string) :: stl -> 
        if (is_in_alphabet sh) 
        then (let sym_rank = (List.length str_ls) - 1
              in conv_loop stl (sh, sym_rank) subtrees_acc)
        else conv_loop stl nodsym_acc (Leaf sh :: subtrees_acc)
    in conv_loop str_ls ("", -1) []
  in 
  (** extract trees to combine and corresponding symbol list *)
  let extract_tree_exprs (relev_lines: string list): (tree * string list) list =
    let combine_term_w_dot (input_ls: string list): string list = 
      let rec loop ls (acc: string list) =
        match ls with [] -> List.rev acc
        | hd :: tl -> 
          if (tl != [] &&  List.hd tl = "." )
          then loop (List.tl tl) (("@" ^ hd):: acc)
          else loop tl (hd :: acc)
      in loop input_ls []
    in
    let refine_str (str: string): string list = str |> split_on_char ' ' 
        |> List.filter (fun x -> not (x = "THEN") && not (x = "ELSE") && not (x = ""))
        |> combine_term_w_dot
    in
    (* change to each str in list to e.g., [expr; sym; @expr] in list *)
    let rec extract_loop (ls: string list) (res_acc: (tree * string list) list): (tree * string list) list = 
      match ls with 
      | [] -> List.rev res_acc 
      | shd :: stl -> 
        let s_ls = refine_str shd in 
        let s_tree = convert_to_tree_exprs s_ls in 
        extract_loop stl ((s_tree, s_ls) :: res_acc)
    in extract_loop relev_lines []
  in
  (* auxiliary 'insert_tree_in_leaves' for combine_two_trees *)
  let insert_tree_in_leaves (lvs: tree list) (t: tree): tree list = 
    let rec replace_loop ls acc = 
      match ls with [] -> List.rev acc
      | (Leaf v) as hd :: tl -> 
        if starts "@" v 
        then replace_loop tl (t::acc)
        else replace_loop tl (hd::acc)
      | _ -> raise Leaf_is_not_valid
    in replace_loop lvs []
  in
  (* get_restriction_on_tree gets Oa := [(Assoc (sym, "l"))] or Op := [(sym1, 0); (sym2, 1)] *)
  let get_restriction_on_tree (t: tree) (oa: bool) (op: bool): restriction list =
    match t with Leaf _ -> raise Leaf_is_not_valid
    | Node (sym, subts) -> 
      match subts with [] -> raise Invalid_subtrees
      | hd :: tl -> 
        if oa 
        then (let lft_sym = tree_symbol hd
              in if syms_equals lft_sym sym 
                 then [Assoc (sym, "l")]
                 else let rht_sym = tree_symbol (List.hd tl)
                      in if syms_equals rht_sym sym 
                      then [Assoc (sym, "r")]
                      else raise Neither_left_nor_right)
        else if op
        then (let subt_sym = subts |> List.filter (fun t -> not (is_leaf t)) |> List.hd |> tree_symbol
              in [Prec (sym, 0); Prec (subt_sym, 1)])
        else raise Tree_specifies_oa_or_op
  in
  (** combine_two_trees  *)
  let combine_two_trees (te1: tree * string list) (te2: tree * string list): 
    (tree * (bool * bool) * restriction list) list =
    match te1, te2 with 
    | (Leaf _, _), (_, _) | (_, _), (Leaf _, _) -> raise Leaf_is_not_valid
    | (Node (sym1, lvs1), _), (Node (sym2, lvs2), _) -> 
      let lvs1_inserted = insert_tree_in_leaves lvs1 (Node (sym2, lvs2)) in
      let oa1, op1 = check_oa_op (Node (sym1, lvs1_inserted)) in
      let r_ls1: restriction list = get_restriction_on_tree (Node (sym1, lvs1_inserted)) oa1 op1 in 
      let lvs2_inserted = insert_tree_in_leaves lvs2 (Node (sym1, lvs1)) in
      let oa2, op2 = check_oa_op (Node (sym2, lvs2_inserted)) in
      let r_ls2: restriction list = get_restriction_on_tree (Node (sym2, lvs2_inserted)) oa2 op2 in 
      [Node (sym1, lvs1_inserted), (oa1, op1), r_ls1; Node (sym2, lvs2_inserted), (oa2, op2), r_ls2]
  in 
  let combine_tree_exprs (e_trees_n_exprs: (tree * string list) list): 
    (tree * (bool * bool) * restriction list) list = 
    let rec combine_loop ls res_acc =
      match ls with 
      | [] -> List.rev res_acc 
      | texpr1 :: tl ->
        if tl = [] then raise Invalid_number_of_trees
        else (let texpr2 = List.hd tl in 
              let two_trees_combined: (tree * (bool * bool) * restriction list) list = 
                  combine_two_trees texpr1 texpr2 in 
              combine_loop (List.tl tl) (two_trees_combined @ res_acc))
    in combine_loop e_trees_n_exprs []
  in
  let relev_ls: string list = traverse 1 false [] in 
  let extracted_trees_n_exprs: (tree * string list) list = relev_ls |> extract_tree_exprs in
  let combined_trees: (tree * (bool * bool) * restriction list) list = 
                                                combine_tree_exprs extracted_trees_n_exprs in
  (* generate tree expressions by splitting per every two combined ones *)
  (* (TODO) To remove 'tree_expressions' when no longer necessary *)
  let tree_expressions: (string list * string list) list = 
    let rec gen_texprs lst cnt tmp_acc res_acc = 
      match lst with [] -> List.rev res_acc
      | (t, (_, _), _) :: tl ->
        if cnt = 0 
        then (let texpr = tree_to_expr t in 
              gen_texprs tl (cnt+1) (texpr::tmp_acc) res_acc)
        else (let texpr = tree_to_expr t in
              let to_acc = texpr, (List.hd tmp_acc) in
              gen_texprs tl 0 [] (to_acc::res_acc))
    in gen_texprs combined_trees 0 [] []
  in
  (* generate tree expressions by splitting per every two combined ones *)
  let tree_example_pairs: ((string list * tree * (bool * bool) * restriction list) * 
                           (string list * tree * (bool * bool) * restriction list)) list = 
    let rec gen_texamples lst cnt tmp_acc res_acc = 
      match lst with [] -> List.rev res_acc
      | (t, (oa, op), sls) :: tl ->
        if cnt = 0 
        then (let texpr: string list = tree_to_expr t in 
              gen_texamples tl (cnt+1) ((texpr, t, (oa, op), sls)::tmp_acc) res_acc)
        else (let texpr: string list = tree_to_expr t in
              let to_acc = (texpr, t, (oa, op), sls), (List.hd tmp_acc) in
              gen_texamples tl 0 [] (to_acc::res_acc))
    in gen_texamples combined_trees 0 [] []
  in
  if debug_print then (Pp.pp_collected_from_conflicts relev_ls; 
  (* Pp.pp_tree_pairs_syms extracted_trees_syms;  *)
  Pp.pp_combined_trees combined_trees;
  Pp.pp_exprs tree_expressions);
  tree_example_pairs


  
(** negate the pattern (tree) by reversing the hierarchy of an input tree
  * assume (1) at least 2 or more trees are nested in the input tree
  *        (2) in each level, there is at most 1 subtree
  *        (3) whether a subtree is a left or right child does not matter *)
let negate_pat (debug_print: bool) (pat: tree): tree =
  let open Printf in 
  if debug_print then (printf "\n  >> Negating the following pattern:\n\t";
  Pp.pp_tree pat; printf "\n");
  (* traverse from top to bottom and store trees in reverse order *)
  let rec traverse_tree e acc =
    (* if height <= 1 then no hierarchy to reverse *)
    if (height pat <= 1) then [pat] else
      match e with
      | Leaf _ -> acc
      | Node (sym, subts) as t_curr ->
        if (is_there_node subts) then 
          (let ind = return_node_index subts in
          let subt_nxt = List.nth subts ind in
          let subts_new = replace_node_wleaf subts in
          let t_new = Node (sym, subts_new) in
          traverse_tree subt_nxt (t_new::acc)) 
        else traverse_tree (Leaf "dum") (t_curr::acc)
  in
  let rev_ls = traverse_tree pat [] in 
  let rec traverse_lst (prevt: tree list) (ls: tree list) (res: tree) =
    let is_empty lst = match lst with [] -> true | _ -> false in
      match ls with
      | [] -> res
      | h :: tl -> if (is_empty prevt) then traverse_lst (h::prevt) tl res
      else let rev_combined = combine_trees_aux (List.hd prevt) h in 
      traverse_lst [rev_combined] tl rev_combined
  in let res_t = if (List.length rev_ls <= 1) then List.hd rev_ls else traverse_lst [] rev_ls (Leaf "") in 
  if debug_print then (printf "\n  >> Result of reversing hierarchy of tree:\n\t";
  Pp.pp_tree res_t; printf "\n"); res_t



(** generator of random trees without a pattern :
  * given (1) a (+/-) pattern and 
  *       (2) a set of symbols excluding all symbols in this pattern
  *           (above exclusion to ensure no opposite pattern occurs in a generated tree)
  *           generate a random tree with this negative pattern *)

(** generator of random trees with a specific pattern *)
let rand_tree_wpat (a: symbol list) (debug_print: bool) (pat: tree): tree = 
  let open Pp in
  let open Printf in 
  let open Random in
  let len = List.length a in
  let pat_depth = height pat in
  if debug_print then (printf "\nGenerating a random tree with a pattern:\n\t"; pp_tree pat;
  printf "\n\t.. whose height is %d\n" pat_depth;
  printf "\n\tGiven following alphabet:\n\t"; pp_alphabet a);
  (* TODO: To revise below to make generated random tree w/ pattern non-trivial *)
  (* run loop until the randomly chosen sym is non-trivial *)
  (* randomly select a symbol from alphabet and append 'pat' to it *)
  let dep_fin = ref 0 in
  let rec loop a dep =
    let rind = self_init (); int len in
    let sym = List.nth a rind in
    if debug_print then (printf "\n\tRandomly selected symbol is "; pp_symbol sym);
    dep_fin := dep;
    let ar = arity sym in match ar with 
    | 0 -> Node (sym, [pat]) 
    | num -> 
      (match fst sym with "IF" -> 
        if (ar = 2) then Node (sym, [Leaf "cond_expr"; pat])
        else Node (sym, [Leaf "cond_expr"; Leaf "expr"; pat])
      | _ -> 
        let trees_ls = List.init num (fun _ -> loop a (dep+1)) in 
        Node (sym, trees_ls))
  in let tree_res = loop a 0 in if debug_print then 
    (printf "\n\n >> Tree generated: \n"; pp_repeat !dep_fin "  "; pp_tree tree_res; printf "\n"); 
  tree_res




(** [prev] generator of purely random trees *)
let rec rand_tree (a: symbol list) (debug_print: bool) (dep: int): tree =
  let open Pp in
  let open Printf in
  let len = List.length a in
  let rind = Random.self_init (); Random.int len in
  let sym = List.nth a rind in 
    if debug_print then (printf "\nGenerating a purely random tree given:\n\t"; pp_alphabet a;
    printf "\n\tRandomly selected symbol is "; pp_symbol sym);
  let ar = arity sym in
  let tree_res = match ar with
  | 0 -> Node (sym, [Leaf "ϵ"])
    (* if debug_print then (pp_repeat dep "\t" ; pp_tree t'; printf "\n");  *)
  | num -> begin match fst sym with "IF" -> 
      if (ar = 2) then Node (sym, [Leaf "cond_expr"; Leaf "expr"])
      else Node (sym, [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])
    | _ -> let trees_ls: tree list = 
      List.init num (fun _ -> rand_tree a debug_print (dep+1)) in Node (sym, trees_ls) end 
  in printf "\n\n >> Tree Generated: \n";pp_repeat dep "  "; pp_tree tree_res; printf "\n"; tree_res



