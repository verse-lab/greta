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

(* let cartesian_product_trans (trans1: transition list) (trans2: transition list): transition list =
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], _ | _, [] -> acc
    | (st1, (sym1, stls1)) as h1 :: tl1, (st2, (sym2, stls2)) :: tl2 ->
      let acc' = loop [h1] tl2 ((h1^h2)::acc) 
      in loop tl1 ls2 (acc' @ acc)
  in loop trans1 trans2 [] *)

(** Intersection of tree automata:  *)
let intersect (a1: ta) (a2: ta) (debug_print: bool): ta =
  let open Printf in
  if debug_print then (printf "\nIntersect the following 2 TAs:\n\tFirst TA:\n";
  Pp.pp_ta a1; printf "\n\tSecond TA:\n"; Pp.pp_ta a2; printf "\n");
  (* TODO: Add a sanity check on alphabet based on set equality *)
  let statesls = cartesian_product_states a1.states a2.states in
  let start = cartesian_product_states [a1.start_state] [a2.start_state] |> List.hd in
  let trans = [] in
  let res_ta = { states = statesls ; alphabet = a1.alphabet
  ; start_state = start ; transitions = trans } in
  if debug_print then (printf "\nResult of intersection: \n"; Pp.pp_ta res_ta); 
  res_ta


(** TODO: Simplify the resulting tree automata via :
    - Enhancing the look via renaming states
    - Removal of duplicate transitions
    - Epsilon introduction/reduction (double-check on this) *)

