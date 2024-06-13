open Ta
open Treeutils

module D = Draw

let ex03 = Node (("+", 2), [Leaf "expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
let ex04 = Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
let ex04_neg = Node (("+", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])])
let ex05 = Node (("IF", 2), [Leaf "cond_expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])

(** gen_examples : gen examples from parser.conflicts in CFG *)
let gen_examples (filename: string) (a: symbol list) (debug_print: bool): (tree * tree) list = 
  let open List in
  let open String in
  let open Printf in
  let a_new = a |> List.map (fun (sym, ar) -> 
    match sym with "+" -> ("PLUS", ar) | "*" -> ("MUL", ar) | "()" -> ("LPARENRPAREN", ar)
    | rest -> (rest, ar)) in
  let syms_ls: string list = a_new |> List.map fst in
  let arity_of_sym sym: int = match List.assoc_opt sym a_new with 
    | None -> printf "Symbol %s not found in alphabet" sym; -1 | Some n -> n in
  printf "\nGenerate examples from conflicts in file %s\n" filename; 
  if debug_print then printf "\tGiven alphabet: "; syms_ls |> List.iter (printf "%s "); 
    (* printf "\n\n  >> Does conflict exist?"; 
    if Sys.file_exists filename then printf "\zn\t\t\tYES\n\n" else printf "\n\t\t\tNO\n\n"; *)
  (** helpers *)
  let ic = open_in filename in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let starts str lin = starts_with ~prefix:str lin in
  let is_in_alphabet s: bool = List.mem s syms_ls 
  in
  (** traverse and acc symbols and relevant lines for generating trees *)
  let rec traverse (lins_acc: string list) (terms_acc: string list) (cnt: int) (can_acclines: bool) 
    (res_acc: (string list * string list) list): (string list * string list) list =
    let is_line_for_syms lin = (starts "** Tokens involved:" lin) || (starts "** Token involved:" lin) in
    let accumulate_syms lin: string list = 
      let lst = split_on_char ':' lin in List.nth lst 1 
      |> split_on_char ' ' |> List.filter (fun x -> not (x = "")) in
    let can_start_acclines lin = starts "** because of the following sub-derivation:" lin in
    let continue_further num: bool = not (num mod 4 = 0) 
    in match try_read () with 
    | None -> close_in ic; List.rev res_acc
    | Some s -> 
      (* if 'can_acclines' count so it reads only 3 lines (where 1st line is blank) *)
      if (can_acclines && (continue_further cnt)) 
      then traverse (s::[] @ lins_acc) terms_acc (cnt + 1) can_acclines res_acc
      else if (can_acclines && not (continue_further cnt))
      then traverse [] [] 1 false ([List.rev lins_acc, List.rev terms_acc] @ res_acc)
      (* if the line lists symbols, collect them *)
      else if (is_line_for_syms s) 
      then (let terms = accumulate_syms s 
            in traverse lins_acc (terms @ terms_acc) cnt can_acclines res_acc)
      (* if lines need to be read, turn on flag 'can_acclines' *)
      else if (can_start_acclines s) then traverse lins_acc terms_acc cnt true res_acc
      else traverse lins_acc terms_acc cnt can_acclines res_acc
  in
  (** convert_to_tree to convert a string list [sym; expr; ...] to (Node sym, [Leaf "expr"; ...]) *)
  let convert_to_tree (str_ls: string list): tree = 
    let rec conv_loop ls nodsym_acc subtrees_acc = 
      match ls with [] -> Node (nodsym_acc, List.rev subtrees_acc)
      | (sh: string) :: stl -> 
        if (is_in_alphabet sh) then (conv_loop stl (sh, (arity_of_sym sh)) subtrees_acc)
        else conv_loop stl nodsym_acc (Leaf sh :: subtrees_acc)
    in conv_loop str_ls ("", -1) [] 
  in
  (** extract trees to combine and corresponding symbol list *)
  let extract_trees (relev_lines: (string list * string list) list): (tree * tree * string list) list =
    let rec extra_loop lsts  res_trees =
      let refine_str (str: string): string list = str |> split_on_char ' ' 
        |> List.filter (fun x -> not (x = "THEN") && not (x = "ELSE") && not (x = "") && not (x = ".")) in
      match lsts with [] -> List.rev res_trees
      | ((lns, syms): string list * string list) :: tl -> 
        (* filter out the first line which is blank *)
        let lns' = lns |> List.filter (fun x -> not (x = "")) in
        let (fst_tree, snd_tree): (tree * tree) =
          if not ((List.length lns') = 2) 
          then (printf "Only 2 lines should be extracted!"; (Leaf "dum1", Leaf "dum2"))
          else 
            let (fst_strls, snd_strls) = (List.hd lns' |> refine_str), (List.nth lns' 1 |> refine_str) in
            convert_to_tree fst_strls, convert_to_tree snd_strls 
        in extra_loop tl ((fst_tree, snd_tree, List.rev syms) :: res_trees)
    in extra_loop relev_lines []
  in
  (** combine_trees : combine t1 and t2 for all possible t2's
   **                 where t2's sym is replaced with symbol from 'syms' *)
  let combine_trees (t1: tree) (t2: tree) (syms: string list): (tree * tree) list =
    let rec comb_loop sls trees_acc: (tree * tree) list = 
      match sls with 
      | [] -> trees_acc
      | sh :: stl -> 
        let t2' = if (sh = (node_symbol t2)) then t2 else change_node_symbol_with t2 sh in
        (* alternate hierarchies to create (tree * tree) *)
        let combined_fst = combine_trees_aux t1 t2' in 
        let combined_snd = combine_trees_aux t2' t1 in
        comb_loop stl trees_acc @ [(combined_fst, combined_snd)]
    in comb_loop syms []
  in
  let relev_ls: (string list * string list) list = traverse [] [] 1 false [] in 
  let extracted_trees_syms: (tree * tree * string list) list = relev_ls |> extract_trees in
  let combined_trees: (tree * tree) list = List.fold_left (fun acc (t1, t2, syms) -> 
    let trees = combine_trees t1 t2 (List.rev syms) in acc @ trees) [] extracted_trees_syms
    |> rewrite_syms in
  let tree_expressions: (string list * string list) list = List.map (fun (t1, t2) ->
    tree_to_expr t1, tree_to_expr t2) combined_trees in
  if debug_print then (Pp.pp_collected_from_conflicts relev_ls; 
  Pp.pp_tree_pairs_syms extracted_trees_syms; Pp.pp_combined_trees combined_trees;
  Pp.pp_exprs tree_expressions);
  combined_trees


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

(* 
let gen_rand_trees n a debug_print: tree list = 
  if debug_print then Printf.printf "\nRandom trees generated : { \n\t"; 
  List.init n (fun _ ->
  let t = rand_tree a debug_print in Pp.pp_tree t; t) 
*)



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



