open Ta

let cartesian_product_states (ls1: state list) (ls2: state list): state list =
  let unique_cons elem ls = if (List.mem elem ls) then ls else elem :: ls in
  let remove_dups ls = List.fold_right unique_cons ls [] in
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], _ | _, [] -> acc
    | h1 :: tl1, h2 :: tl2 ->
      (* cross product with ϵ results in ϵ *)
      let prod = if (h1 = "ϵ" || h2 = "ϵ") then "ϵ" else (h1^h2) in
      let acc' = loop [h1] tl2 (prod :: acc) 
      in loop tl1 ls2 (acc' @ acc)
      (* remove duplicate ϵ states *)
  in loop ls1 ls2 [] |> remove_dups |> List.rev

let cartesian_product_trans (states1: state list) (states2: state list) 
(trans1: transition list) (trans2: transition list) (syms: symbol list): transition list =
  let open List in
  let remove_epsilon ls = filter (fun x -> not (x = "ϵ")) ls in
  (** find_rhs_states : based on lhs_state and sym, find corresonding 'Some rhs_states'
     and return 'None' if it doesn't find corresponding list *)
  let find_rhs_states (lhs_state: state) (sym: symbol) (trans: transition list): state list option = 
    let rec traverse_trans lhs_stat ls =
      match ls with
      | [] -> None (* reached end and found none *)
      | (lhs, (s, rhs_states)) :: tl ->
        if ((lhs = lhs_stat) && (s = sym)) then Some rhs_states else 
        (* assuming ε-tran is happening before all others -> TODO: re-arrange before runnign *)
        if ((lhs = lhs_stat) && (sym_equals s "ε") && (length rhs_states = 1)) 
        then traverse_trans (hd rhs_states) tl
        else traverse_trans lhs tl
    in traverse_trans lhs_state trans in
  let stats1, stats2 = remove_epsilon states1, remove_epsilon states2 in
  (* TODO: simplify code below *)
  let states_pairs: (state * state) list = combine stats1 stats2 in
  let states_lhs = cartesian_product_states stats1 stats2 in
  let states_tuples: (state * (state * state)) list = combine states_lhs states_pairs in 
  let res_trans: transition list = map2 (fun (lhs, (stat1, stat2)) sym ->
    match find_rhs_states stat1 sym trans1, find_rhs_states stat2 sym trans2 with 
    | Some rhs_states1, Some rhs_states2 -> 
      let rhs_states = cartesian_product_states rhs_states1 rhs_states2 in 
      (lhs, (sym, rhs_states))
    | Some _(*v1*), None ->  (lhs, (sym, []))
    | None, Some _(*v2*) -> (lhs, (sym, []))
    | None, None -> raise (Invalid_argument "No transition available!")
    ) states_tuples syms in res_trans

(** Intersection of tree automata *)
let intersect (a1: ta) (a2: ta) (debug_print: bool): ta =
  let open Printf in
  if debug_print then (printf "\nIntersect the following 2 TAs:\n  First TA:\n";
  Pp.pp_ta a1; printf "\n  Second TA:\n"; Pp.pp_ta a2; printf "\n");
  (* TODO: Add a sanity check on alphabet based on set equality *)
  let syms = a1.alphabet in
  let stats = cartesian_product_states a1.states a2.states in
  let start = cartesian_product_states [a1.start_state] [a2.start_state] |> List.hd in
  let trans = cartesian_product_trans a1.states a2.states a1.transitions a2.transitions syms in
  let res_ta = { states=stats ; alphabet=syms; start_state=start ; transitions=trans } in
  if debug_print then (printf "\nResult of TA intersection: \n"; Pp.pp_ta res_ta); 
  res_ta


(** TODO: Simplify the resulting tree automata via :
    - Enhancing the look via renaming states
    - Removal of duplicate transitions
    - Epsilon introduction/reduction (double-check on this) *)

