open Ta

let cartesian_product_states (ls1: state list) (ls2: state list) (debug_print: bool): state list =
  let open Str in
  if debug_print then (Printf.printf "\n  >> Cross product of states:\n\tFirst set of states:\n";
  Pp.pp_states ls1; Printf.printf "\n\tSecond set of states:\n"; Pp.pp_states ls2);
  (** helpers *)
  (** remove_dups : needed to remove duplicate states *)
  (* [prev ver]
  let rec remove_eps_dups acc eps_not_occurred ls =
    match ls with [] -> acc
    | h :: tl -> if (not (h = "ϵ")) then remove_eps_dups (h::acc) eps_not_occurred tl 
    else if (h = "ϵ" && not eps_not_occurred) then remove_eps_dups (h::acc) true tl 
    else remove_eps_dups acc eps_not_occurred tl
  in *)
  let remove_dups ls =
    let unique_cons elem ls = if (List.mem elem ls) then ls else elem :: ls in
    List.fold_right unique_cons ls [] in
  (** is cond_expr : check if state is representing boolean state *)
  let is_cond_expr s: bool = string_match (regexp "cond") s 0 || string_match (regexp "Cond") s 0 in
  (** remove_dummies : needed to remove _ states *)
  let remove_dummies ls = List.filter (fun elem -> not (elem = "_")) ls in
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], _ | _, [] -> acc
    | h1 :: tl1, h2 :: tl2 ->
      (* cross product with ϵ results in ϵ *)
      let prod = if (h1 = "ϵ" || h2 = "ϵ") then "ϵ" 
        (* cross product of {"cond" ^ s* || "Cond" ^ s*} and non cond_expr results in dummy state "_"  *)
        else if (is_cond_expr h1 && not (is_cond_expr h2)) || (not (is_cond_expr h1) && is_cond_expr h2) 
        then "_" else (h1^h2) in
      let acc' = loop [h1] tl2 (prod :: acc) 
      in loop tl1 ls2 (acc' @ acc)
      (* remove duplicate ϵ states and remove dummy states *)
  in let res = loop ls1 ls2 [] |> remove_dups |> remove_dummies in if debug_print 
    then (Printf.printf "\n  >> Result of states X states:\n"; Pp.pp_states res); List.rev res

let cartesian_product_trans (states1: state list) (states2: state list) (trans1: transition list) (trans2: transition list) 
  (syms: symbol list) (verSyms: string list) (debug_print: bool): transition list =
  let open Str in 
  let open List in
  let open Printf in
  if debug_print then (printf "\n  >> Cross product of transitions:\n\tFirst transitions:\n"; 
  Pp.pp_transitions trans1; printf "\n\tSecond transitions:\n"; Pp.pp_transitions trans2);
  (** helpers *)
  let remove_epsilon ls = filter (fun x -> not (x = "ϵ")) ls in
  let stats1, stats2 = remove_epsilon states1, remove_epsilon states2 in
  if debug_print then (printf "\n\tGiven two lists of states:\n\t"; Pp.pp_states stats1; printf "\t"; Pp.pp_states stats2);
  (** cartesian_tuples : combine two lists of states and make ((state1 * state2) * state1state2) list *)
  let cartesian_tuples l l': ((state * state) * state) list = 
    let is_cond_expr s: bool = string_match (regexp "cond") s 0 || string_match (regexp "Cond") s 0 in
    let is_there_one_cond s s': bool = (is_cond_expr s && not (is_cond_expr s')) || (not (is_cond_expr s) && is_cond_expr s') in
    concat (map (fun e -> (map (fun e' -> ((e, e'), e^e'))) l') l) |> filter (fun ((s, s'), _) -> not (is_there_one_cond s s')) in
  (** find_rhs_states : based on lhs_state and sym, find corresonding 'Some rhs_states'
      and return 'None' if it doesn't find corresponding list
      when it's one of the varsatile symbols, treat it differently so it generates correct rhs_states *)
  let record_counter = ref 1 in
  let find_rhs_states (lhs_state: state) (sym: symbol) (trans: transition list) (debug_print: bool): state list option = 
    if debug_print then (printf "\n\tFor state '%s' and symbol \"%s\", we get the following RHS states:\n" lhs_state) (fst sym);
    (* let traversed_all = ref false in
    let rhs_epsilon = ref "" in *)
    let rec traverse_trans lhs_stat ls: state list option =
      match ls with
      | [] -> None (* reached end and found none *)
      | (lhs, (s, rhs_states)) :: tl ->
        if ((lhs = lhs_stat) && (syms_equals s sym) && not (mem (fst sym) verSyms)) then (Some rhs_states) else 
        if ((lhs = lhs_stat) && (syms_equals s sym) && (mem (fst sym) verSyms) && (!record_counter mod 3 != 0)) 
          then (record_counter := !record_counter + 1; Some rhs_states) else
        if ((lhs = lhs_stat) && (syms_equals s sym) && (mem (fst sym) verSyms) && (!record_counter mod 3 = 0)) 
          then (record_counter := 2; traverse_trans lhs tl) else
        (* assuming ε-tran is happening before all others -> TODO: re-arrange before running *)
        (* if ((lhs = lhs_stat) && (sym_equals s "ε") && (length rhs_states = 1)) then traverse_trans (hd rhs_states) tl else *)
        traverse_trans lhs_stat tl
    in let res_trav = traverse_trans lhs_state trans in if debug_print then 
    (match res_trav with None -> printf "\t\tNo matching RHS states" | Some l -> printf "\t"; Pp.pp_states l); res_trav 
  in
  let states_tuples: ((state * state) * state) list = cartesian_tuples stats1 stats2 in
  (** cartesian_trans : take cartesian products of two sets of transitions *)
  let rec cartesian_rhs_states ls1 ls2 res = 
    match ls1, ls2 with [], [] -> rev res 
    | (h1::tl1), (h2::tl2) -> 
      if ((h1 = "ϵ") || (h2 = "ϵ")) then cartesian_rhs_states tl1 tl2 ("ϵ" :: res) 
    else cartesian_rhs_states tl1 tl2 ((h1^h2) :: res)
    | _, [] | [], _ -> raise (Invalid_argument "RHS states do not match!")
  in
  let rec cartesian_trans l acc: transition list =
    match l with [] -> acc
    | ((s1, s2), s1s2) :: tl -> 
      let trans_ls: transition list = syms |> map (fun sym -> 
        begin match (find_rhs_states s1 sym trans1 debug_print), (find_rhs_states s2 sym trans2 debug_print) with 
        | Some rhs_states1, Some rhs_states2 ->
          let rhs_states_comb: state list = cartesian_rhs_states rhs_states1 rhs_states2 [] in (s1s2, (sym, rhs_states_comb))
        | Some _, None | None, Some _ | None, None -> ("dummy", (sym, []))
        end) |> filter (fun (lhs, (_, _)) -> not (lhs = "dummy")) in cartesian_trans tl (acc @ trans_ls)
  in let res_trans: transition list = cartesian_trans states_tuples [] in
  if debug_print then (printf "\n  >> Result of trans X trans:\n"; 
  Pp.pp_transitions res_trans); res_trans

(** Intersection of tree automata *)
let intersect (a1: ta) (a2: ta) (verSyms: string list) (debug_print: bool): ta =
  let open Printf in
  if debug_print then (printf "\nIntersect the following 2 TAs:\n  (1) First TA:\n";
  Pp.pp_ta a1; printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; printf "\n"; 
  printf "\nVersatile symbol list: \n\t[ "; verSyms |> List.iter (fun x -> printf "%s " x); printf "]\n");
  (* TODO: Add a sanity check on alphabet based on set equality *)
  let syms = a1.alphabet in
  let stats1, stats2 = a1.states, a2.states in
  let stats = cartesian_product_states stats1 stats2 debug_print in
  let start = cartesian_product_states [a1.start_state] [a2.start_state] debug_print |> List.hd in
  let trans = 
    cartesian_product_trans stats1 stats2 a1.transitions a2.transitions syms verSyms debug_print in
  let res_ta = { states=stats ; alphabet=syms; start_state=start ; transitions=trans } in
  if debug_print then (printf "\nResult of TA intersection: \n"; Pp.pp_ta res_ta); 
  res_ta


(** TODO: Simplify the resulting tree automata via :
    - Enhancing the look via renaming states
    - Removal of duplicate transitions
    - Epsilon introduction/reduction (double-check on this) *)

