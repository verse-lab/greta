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

let starts str lin = String.starts_with ~prefix:str lin
let change_to_str_ls s = Str.split (Str.regexp "[ \t\n\r]+") s
let contains lin s = 
  try 
    (ignore (Str.search_forward (Str.regexp_string s) lin 0); true)
  with 
    Not_found -> false
let index_of x ls = 
  let rec aux i = function 
  | [] -> None
  | y :: ys -> if x = y then Some i else aux (i+1) ys 
  in aux 0 ls

let pp_nonaddr (non_addr_ambigs: (string * string list) list ) = 
  let open Printf in 
  printf "\n\t *** Note there are %s ambiguities that are not addressable by Greta. *** \n" (string_of_int (List.length non_addr_ambigs));
  let rec pp_loop (prod_hd: string) (past_qq: bool) (ls: string list) = 
    match ls with [] -> printf "\n"
    | h :: tl ->
      if past_qq then 
        (if (not (String.equal prod_hd "")) 
         then (printf "\t%s -> %s\n" prod_hd h; (pp_loop prod_hd past_qq tl)) 
         else let s_ls = change_to_str_ls h in 
          let arr_ind = match (index_of "->" s_ls) with Some i -> i | None -> raise (Failure "arr index") in
          let lhs = s_ls |> List.filteri (fun j _ -> j = (arr_ind - 1)) |> List.hd in 
          (printf "\t%s\n" h; pp_loop lhs past_qq tl))
      else if contains h "(?)" 
      then (printf "\t%s\n\t At this point (??), the following set of productions makes parsing ambiguous: \n" h; 
            pp_loop prod_hd true tl) 
      else 
        (printf "\t%s -> \n" h; pp_loop prod_hd past_qq tl)
  in 
  non_addr_ambigs |> List.iteri (fun i (tk, lns) -> 
    printf "\n\tAmbig #%s \n\t* Tokens involved: %s\n\t* How to reach this ambiguity: \n" (string_of_int (i+1)) tk; 
    pp_loop "" false lns)


let gen_examples_new (filename: string) (a: symbol list) (debug_print: bool): 
  ((string list * tree * (bool * bool) * restriction list) * (string list * tree * (bool * bool) * restriction list)) list = 
  let open List in
  let open Printf in
  let syms_ls: string list = a |> List.map fst in
  printf "\nGenerate examples from conflicts in file %s\n" filename; 
  (* *** debug *** *)
  if debug_print then (printf "\tGiven alphabet: "; syms_ls |> List.iter (printf "%s "); printf "\n");
  (* helpers *)
  let ic = open_in filename in
  let ic2 = open_in filename in 
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let try_read2 () = try Some (input_line ic2) with End_of_file -> None in 
  let contains_atat lin = 
    try 
      (ignore (Str.search_forward (Str.regexp_string "@@") lin 0); true)
    with 
      Not_found -> false
  in
  let is_in_alphabet s: bool = 
    List.mem s syms_ls 
  in  
  let add_at_at i ls = 
    List.mapi (fun j x -> if j = i then ("@" ^ x) else x) ls in 
  let remove_at i ls =
  List.filteri (fun j _ -> j <> i) ls 
  in
  let attach_at_with_nonterm s: string = 
    let s_ls = change_to_str_ls s in
    let at_ind = match (index_of "@@" s_ls) with Some i -> i | None -> raise (Failure "atat index") in
    let new_sls = add_at_at (at_ind - 1) s_ls in 
    let new_sls_filtered = remove_at at_ind new_sls in 
    String.concat " " new_sls_filtered
  in  
   let extract_tokens s: string = 
    let s_ls = change_to_str_ls s in
    let tkns_ind = match (index_of "Tokens:" s_ls) with Some i -> i | None -> raise (Failure "tokens: index") in
    let new_sls_filtered = List.filteri (fun j _ -> j = tkns_ind + 1) s_ls in
    List.hd new_sls_filtered
  in
  let extract_prod_line s: string = 
    let s_ls = change_to_str_ls s in 
    let prod_ind = match (index_of "Production" s_ls) with Some i -> i | None -> raise (Failure "prod: index") in
    let new_sls_filtered = s_ls |> remove_at prod_ind |> List.tl in
    String.concat " " new_sls_filtered
  in
  (* traverse and acc relevant lines for generating trees from `parser.trees` file *)
  let rec traverse (res_acc: string list): string list = 
    match try_read () with 
    | None -> (close_in ic; List.rev res_acc)
    | Some s -> 
      (* <<== NOTE! below added to not address ambigs outside the scope of greta *)
      if (contains_atat s) && (not (starts "@@" s))
      then (let changed_str = attach_at_with_nonterm s
            in traverse (changed_str::res_acc))
      else traverse res_acc
  in 
  (* traverse_for_nonaddr collects prods that can be useful for non-addressable ambiguities *)
  let rec traverse_nonaddr (outside_acc: (string * string list) list) (can_collect_ctxt: bool) (curr_ctxt: string list)
    (can_collect_last_ctxt: bool) (last_ctxt_prods: string list) (tkns: string): (string * string list) list = 
    match try_read2 () with 
    | None -> (close_in ic2; List.rev outside_acc)
    | Some s -> 
      (* *** 
         ==> Depending on what other grammars dump for non-addressable case, might have to change logic below
       *** *)   
      begin 
        match can_collect_ctxt with 
        | true -> 
          if (starts ">> ContextEnd" s) 
          then traverse_nonaddr outside_acc false curr_ctxt can_collect_last_ctxt last_ctxt_prods tkns 
          else traverse_nonaddr outside_acc can_collect_ctxt (s::curr_ctxt) can_collect_last_ctxt last_ctxt_prods tkns 
        | false -> 
          if can_collect_last_ctxt 
          (* assumption: only 2 production lines w.r.t. last_cotxt_prods - so set to false while collecting curr 's' *)
          then traverse_nonaddr outside_acc can_collect_ctxt curr_ctxt false (s::last_ctxt_prods) tkns 
          else
          (* if collect_last_ctxt is false, then check if it starts from "* Production" *)
          if (starts "* Production" s) 
          then (let curr_prod = extract_prod_line s 
                in traverse_nonaddr outside_acc can_collect_ctxt curr_ctxt true (curr_prod::last_ctxt_prods) tkns)
          else
          (* if contains @@ and starts from @@, then has to do with nonaddressable ambigs (To check this assumption!) *)
          if (contains_atat s) && (starts "@@" s) 
          then 
            (if List.is_empty curr_ctxt 
             then traverse_nonaddr outside_acc can_collect_ctxt curr_ctxt can_collect_last_ctxt last_ctxt_prods tkns  
             else traverse_nonaddr ((tkns, (List.rev curr_ctxt) @ (List.rev last_ctxt_prods))::outside_acc) can_collect_ctxt [] can_collect_last_ctxt [] "") 
          else
          (* if starts from "** Tokens:", then extract tokens and collect *)
          if (starts "** Tokens:" s) then 
            (let tkns_from_string = extract_tokens s 
             in traverse_nonaddr outside_acc can_collect_ctxt curr_ctxt can_collect_last_ctxt last_ctxt_prods tkns_from_string) 
          else 
          (* if starts from ">> ContextStart", then set 'can_collect_ctxt' to be true while passing [] to 'curr_ctxt' *)
          if (starts ">> ContextStart" s) 
          then traverse_nonaddr outside_acc true [] can_collect_last_ctxt last_ctxt_prods tkns 
          else
          (* if 's' contains @@ and does not start form @@, then not relevant in this loop *)
          if (contains_atat s) && (not (starts "@@" s)) 
          then traverse_nonaddr outside_acc false [] false [] ""
          else traverse_nonaddr outside_acc can_collect_ctxt curr_ctxt can_collect_last_ctxt last_ctxt_prods tkns 
      end 
  in 
  let convert_to_tree_exprs (str_ls: string list): tree = 
    let rec conv_loop ls nodsym_acc subtrees_acc = 
      match ls with [] -> Node (nodsym_acc, List.rev subtrees_acc)
      | (sh: string) :: stl -> 
        if (is_in_alphabet sh)
        then (let sym_rank = (List.length str_ls) (* (me) is this length or length - 1? *)
              in conv_loop stl (sh, sym_rank) (Leaf sh :: subtrees_acc))
        else conv_loop stl nodsym_acc (Leaf sh :: subtrees_acc)
    in conv_loop str_ls ("", -1) []
  in 
  let extract_tree_exprs (relev_lines: string list): (tree * string list) list = 
    let rec extract_loop (ls: string list) (res_acc: (tree * string list) list): (tree * string list) list = 
      match ls with 
      | [] -> List.rev res_acc
      | shd :: stl -> 
        let s_ls = shd |> String.split_on_char ' ' in
        let s_tree = convert_to_tree_exprs s_ls in 
        (if debug_print then (Pp.pp_tree s_tree; printf "\n\n");
        extract_loop stl ((s_tree, s_ls) :: res_acc))
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
                 else let rht_sym = tree_symbol (List.hd (List.tl tl))
                      in if syms_equals rht_sym sym 
                      then [Assoc (sym, "r")]
                      else raise Neither_left_nor_right)
        else if op
        then (let subt_sym = subts |> List.filter (fun t -> not (is_leaf t)) |> List.hd |> tree_symbol
              in [Prec (sym, 0); Prec (subt_sym, 1)])
        else raise Tree_specifies_oa_or_op
  in
  (* combine_two_trees  *)
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
                  (* *** debug *** *)
                  if debug_print then (printf "\n\nCombined tree: "; 
                    let (fst_tree, _, _) = List.nth two_trees_combined 0 in 
                    let (snd_tree, _, _) = List.nth two_trees_combined 1
                    in Pp.pp_tree fst_tree; printf "\n"; Pp.pp_tree snd_tree);
              combine_loop (List.tl tl) (two_trees_combined @ res_acc))
    in combine_loop e_trees_n_exprs []
  in 
  let relev_ls: string list = traverse [] in
    (if debug_print then relev_ls |> (List.iter (fun x -> printf "%s\n" x)));
  let extracted_trees_n_exprs: (tree * string list) list = relev_ls |> extract_tree_exprs in 
  if debug_print then (printf "\tExtracted trees: "; 
    let tls: tree list = extracted_trees_n_exprs |> List.map fst 
    in List.iter (fun x -> (Pp.pp_tree x; printf "\n\t")) tls; printf "\n"); 
    
  let combined_trees: (tree * (bool * bool) * restriction list) list = 
                                                combine_tree_exprs extracted_trees_n_exprs in
  (* generate tree expressions by splitting per every two combined ones *)
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
  if debug_print then (Pp.pp_combined_trees combined_trees); Pp.pp_exprs tree_expressions;
  let non_addr_ambigs: (string * string list) list = traverse_nonaddr [] false [] false [] "" in 
  pp_nonaddr non_addr_ambigs;
  tree_example_pairs 



(** gen_examples_prev : gen examples from parser.conflicts in CFG *)
let gen_examples (filename: string) (a: symbol list) (debug_print: bool): 
  ((string list * tree * (bool * bool) * restriction list) * (string list * tree * (bool * bool) * restriction list)) list = 
  let open List in
  let open String in
  let open Printf in
  let syms_ls: string list = a |> List.map fst in
  printf "\nGenerate examples from conflicts in file %s\n" filename; 
    (* *** debug *** *)
    if debug_print then (printf "\tGiven alphabet: "; syms_ls |> List.iter (printf "%s "); printf "\n");
  (* helpers *)
  let ic = open_in filename in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let starts str lin = starts_with ~prefix:str lin in
  (* let arity_of_sym sym: int = match List.assoc_opt sym a_new with 
    | None -> printf "Symbol %s not found in alphabet" sym; -1 | Some n -> n in *)
  let is_in_alphabet s: bool = List.mem s syms_ls in
  (* traverse and acc relevant lines for generating trees *)
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
  (* convert_to_tree convert an expression (string list) ["expr"; "sym"; "@expr"] 
      to a tree (Node sym, [Leaf "expr"; ...]) *)
  let convert_to_tree_exprs (str_ls: string list): tree = 
    let rec conv_loop ls nodsym_acc subtrees_acc = 
      match ls with [] -> Node (nodsym_acc, List.rev subtrees_acc)
      | (sh: string) :: stl -> 
        if (is_in_alphabet sh) 
          (* *** debug *** *)
        then (let sym_rank = (List.length str_ls) - 1
              in conv_loop stl (sh, sym_rank) subtrees_acc)
        else conv_loop stl nodsym_acc (Leaf sh :: subtrees_acc)
    in conv_loop str_ls ("", -1) []
  in 
  (* extract trees to combine and corresponding symbol list *)
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
        |> List.filter (fun x -> not (x = "")) 
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
  (* combine_two_trees  *)
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
                  (* *** debug *** *)
                  if debug_print then (printf "\n\nCombined tree: "; 
                    let (fst_tree, _, _) = List.nth two_trees_combined 0 in 
                    let (snd_tree, _, _) = List.nth two_trees_combined 1
                    in Pp.pp_tree fst_tree; printf "\n"; Pp.pp_tree snd_tree);
              combine_loop (List.tl tl) (two_trees_combined @ res_acc))
    in combine_loop e_trees_n_exprs []
  in
  let relev_ls: string list = traverse 1 false [] in 
    (* *** debug *** *)
    
    if debug_print then (printf "\tRel lines: "; relev_ls |> List.iteri (fun i l -> (printf "#%d %s \n" (i+1) l)); printf "\n"); 
    
  let extracted_trees_n_exprs: (tree * string list) list = relev_ls |> extract_tree_exprs in 
    (* *** debug *** *)
    if debug_print then (printf "\tExtracted trees: "; 
    let tls: tree list = extracted_trees_n_exprs |> List.map fst 
    in List.iter (fun x -> (Pp.pp_tree x; printf "\n\t")) tls; printf "\n"); 
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
  (* 
  Pp.pp_collected_from_conflicts relev_ls 
  Pp.pp_tree_pairs_syms extracted_trees_syms;  
  *)
  if debug_print then (Pp.pp_combined_trees combined_trees; Pp.pp_exprs tree_expressions);
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




