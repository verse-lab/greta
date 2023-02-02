open Ta

let cartesian_product_states (ls1: state list) (ls2: state list): state list =
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], _ | _, [] -> acc
    | h1::tl1, h2::tl2 ->
      let acc' = loop [h1] tl2 ((h1^h2)::acc) 
      in loop tl1 ls2 (acc' @ acc)
  in loop ls1 ls2 []

(** Intersection of tree automata:  *)
let intersect (a1: ta) (a2: ta): ta =
  (* TODO: Add a sanity check on alphabet based on set equality *)
  let statesls = cartesian_product_states a1.states a2.states in
  let start = cartesian_product_states [a1.start_state] [a2.start_state] |> List.hd in
  let trans = [] in
  { states = statesls ; alphabet = a1.alphabet
  ; start_state = start ; transitions = trans }


(** TODO: Simplify the resulting tree automata via :
    - Enhancing the look via renaming states
    - Removal of duplicate transitions
    - Epsilon introduction/reduction (double-check on this) *)

