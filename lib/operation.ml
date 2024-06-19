open Ta
open Treeutils

let cartesian_product_states (ls1: state list) (ls2: state list) (debug_print: bool): state list =
  if debug_print then (Printf.printf "\n  >> Cross product of states:\n\tFirst set of states:\n";
  Pp.pp_states ls1; Printf.printf "\n\tSecond set of states:\n"; Pp.pp_states ls2);
  (** helpers *)
  (** remove_dups : needed to remove duplicate states *)
  let remove_dups ls =
    let unique_cons elem ls = if (List.mem elem ls) then ls else elem :: ls in
    List.fold_right unique_cons ls [] in
  (** remove_dummies : needed to remove _ states *)
  let remove_dummies ls = List.filter (fun elem -> not (elem = "_")) ls in
  let rec cross_loop l1 l2 acc =
    match l1, l2 with [], [] -> acc
    | [], _ | _, [] -> acc
    | h1 :: tl1, h2 :: tl2 ->
      (* cross product with ϵ results in ϵ *)
      let prod = if (h1 = "ϵ" || h2 = "ϵ") then "ϵ" 
        (* cross product of {"cond" ^ s* || "Cond" ^ s*} and non cond_expr results in dummy state "_"  *)
        else if (is_cond_expr h1 && not (is_cond_expr h2)) || (not (is_cond_expr h1) && is_cond_expr h2) 
        then "_" else (h1^h2) in
      let acc' = cross_loop [h1] tl2 (prod :: acc) 
      in cross_loop tl1 ls2 (acc' @ acc)
      (* remove duplicate ϵ states and remove dummy states *)
  in let res = cross_loop ls1 ls2 [] |> remove_dups |> remove_dummies in if debug_print 
    then (Printf.printf "\n  >> Result of states X states:\n"; Pp.pp_states res); List.rev res

let cartesian_product_trans (states1: state list) (states2: state list) (trans1: transition list) (trans2: transition list) 
  (syms: symbol list) (verSyms: (string * int list) list) (debug_print: bool): transition list =
  let open List in
  let open Printf in
  if debug_print then (printf "\n  >> Cross product of transitions:\n\tFirst transitions:\n"; 
  Pp.pp_transitions trans1; printf "\n\tSecond transitions:\n"; Pp.pp_transitions trans2);
  (** helpers *)
  let epsSym, parenSym = ("ε", 1), ("()", 1) in
  let vers_symNames = verSyms |> map fst in
  let remove_epsilon ls = filter (fun x -> not (x = "ϵ")) ls in
  let stats1, stats2 = remove_epsilon states1, remove_epsilon states2 in
  if debug_print then (printf "\n\tGiven two lists of states:\n\t"; Pp.pp_states stats1; printf "\t"; Pp.pp_states stats2);
  (** cartesian_tuples : combine two lists of states and make ((state1 * state2) * state1state2) list *)
  let cartesian_tuples l l': ((state * state) * state) list = 
    let is_there_one_cond s s': bool = (is_cond_expr s && not (is_cond_expr s')) || (not (is_cond_expr s) && is_cond_expr s') in
    concat (map (fun e -> (map (fun e' -> ((e, e'), e^e'))) l') l) |> filter (fun ((s, s'), _) -> not (is_there_one_cond s s')) in
  (** find_rhs_states : based on lhs_state and sym, find corresonding 'Some rhs_states'
      and return 'None' if it doesn't find corresponding list
      when it's one of the varsatile symbols, treat it differently so it generates correct rhs_states *)
  let find_rhs_states (lhs_state: state) (sym: symbol) (trans: transition list) (debug: bool): state list option = 
    if debug then printf "\n\tFor state '%s' and symbol \"%s\", we get the following RHS states:\n" lhs_state (fst sym);
    let rec traverse_trans lhs_stat ls: state list option =
      match ls with
      | [] -> None (* reached end and found none *)
      | (lhs, (s, rhs_states)) :: tl ->
        if ((lhs = lhs_stat) && (syms_equals s sym) && not (mem (fst sym) vers_symNames)) then (Some rhs_states) else 
        if ((lhs = lhs_stat) && (syms_equals s sym) && (mem (fst sym) vers_symNames) && (length rhs_states = (snd sym)))
        then (Some rhs_states) else (* record_counter := !record_counter + 1; *)
        if ((lhs = lhs_stat) && (syms_equals s sym) && (mem (fst sym) vers_symNames) && not (length rhs_states = (snd sym)))
        then (traverse_trans lhs tl) else
        (* assuming ε-tran is happening before all others -> TODO: re-arrange before running *)
        (* if ((lhs = lhs_stat) && (sym_equals s "ε") && (length rhs_states = 1)) then traverse_trans (hd rhs_states) tl else *)
        traverse_trans lhs_stat tl
    in let res_trav = traverse_trans lhs_state trans in if debug then 
    (match res_trav with None -> printf "\t\tNo matching RHS states\n" | Some l -> printf "\t"; Pp.pp_states l); res_trav 
  in
  let find_rhs_states_eps_sym (lhs_state: state) (trans: transition list) (debug: bool): (state list) list option =
    if debug then printf "\n\tFor state'%s' and \"ε\" symbol, we get the following list of RHS states:\n" lhs_state;
    let rec traverse_trans lhs_stat ls acc: (state list) list =
      match ls with [] -> rev acc
      | (lhs, (s, rhs_states)) :: tl -> 
        if ((lhs = lhs_stat) && (syms_equals s epsSym)) 
        then traverse_trans lhs_stat tl (rhs_states :: acc) 
        else traverse_trans lhs_stat tl acc
    in let res_trav = traverse_trans lhs_state trans [] 
    in match res_trav with [] -> (if debug then printf "\t\tNo match RHS states\n"); None
       | ls -> (if debug then ls |> iter (fun l -> printf "\t"; Pp.pp_states l; printf "\n")); Some ls
  in
  let states_tuples: ((state * state) * state) list = cartesian_tuples stats1 stats2 in
  (** cartesian_trans : take cartesian products of two sets of transitions *)
  let rec cartesian_rhs_states ls1 ls2 res: state list = 
    match ls1, ls2 with [], [] -> rev res 
    | (h1::tl1), (h2::tl2) -> 
      if ((h1 = "ϵ") || (h2 = "ϵ")) then cartesian_rhs_states tl1 tl2 ("ϵ" :: res) 
    else cartesian_rhs_states tl1 tl2 ((h1^h2) :: res)
    | _, [] | [], _ -> raise (Invalid_argument "RHS states do not match!")
  in
  let rec cartesian_trans l acc: transition list =
    match l with [] -> acc
    | ((s1, s2), s1s2) :: tl -> 
      let trans_ls: transition list = syms |> fold_left (fun acc_lst sym -> 
        if (not (sym_equals sym "ε")) then
        begin match (find_rhs_states s1 sym trans1 debug_print), (find_rhs_states s2 sym trans2 debug_print) with 
        | Some rhs_states1, Some rhs_states2 ->
          let rhs_states_comb: state list = cartesian_rhs_states rhs_states1 rhs_states2 [] in (s1s2, (sym, rhs_states_comb))::acc_lst
        | Some _, None | None, Some _ | None, None -> ("dummy", (sym, []))::acc_lst
        end else 
        (* there can be multiple epsilon transitions, so treat them differently *)
        begin match (find_rhs_states_eps_sym s1 trans1 debug_print), (find_rhs_states_eps_sym s2 trans2 debug_print) with 
        | Some rhs_statesls1, Some rhs_statesls2 -> 
          (* flatten (state list) list to state list, knowing that ε-trans will have 1 rhs_state *)
          let eps_trans: transition list = 
            let interm: state list = List.fold_left (fun acc rstats1 -> 
                let intermediate = fold_left (fun acc' rstats2 -> 
                  (rstats1^rstats2) :: acc') [] (flatten rhs_statesls2) in 
                  intermediate @ acc) [] (flatten rhs_statesls1)
            in fold_left (fun res_acc rhs_states -> 
              (rhs_states, (("()", 1), [s1s2]))::(s1s2, (("ε", 1), [rhs_states]))::res_acc) [] interm 
            in eps_trans @ acc_lst
          (* TODO: Will have to filter out the one that has same lhs and rhs with epsilon transition! *)
        | Some _, None | None, Some _ | None, None -> ("dummy", (sym, []))::acc_lst 
        end) [] |> filter (fun (lhs, (sym, rhs_states)) -> 
          not (lhs = "dummy") && not (syms_equals sym epsSym && (hd rhs_states) = lhs)
          && not (syms_equals sym parenSym && (hd rhs_states) = lhs)) 
        in cartesian_trans tl (acc @ trans_ls)
  in let res_trans: transition list = cartesian_trans states_tuples [] in
  if debug_print then (printf "\n  >> Result of trans X trans:\n"; 
  Pp.pp_transitions res_trans); res_trans

let cartesian_product_trans_from (st1: state) (st2: state) (trans1: transition list) (trans2: transition list) (a: symbol list) (debug: bool) = 
  let rec traverse_alphabet ls (acc: transition list): transition list = 
    match ls with [] -> acc
    | hd_sym :: tl -> 
      let rht_sts1: state list = find_rhs_states_from_state_with_sym st1 hd_sym trans1 debug in
      let rht_sts2: state list = find_rhs_states_from_state_with_sym st2 hd_sym trans2 debug in
      let new_lft_st: state = st1 ^ st2 in 
      let new_rht_sts: state list = cross_product_state_lists rht_sts1 rht_sts2 in
      traverse_alphabet tl ((new_lft_st, (hd_sym, new_rht_sts))::acc)
  in let res_trans = traverse_alphabet a [] in 
  if debug then (Printf.printf "\nResult of %s-starting transitions X %s-starting transitions : \n" st1 st2; Pp.pp_transitions res_trans);
  res_trans

(** Intersection of tree automata *)
let intersect (a1: ta) (a2: ta) (verSyms: (string * int list) list) (debug_print: bool): ta =
  let open Printf in
  printf "\nIntersect the following 2 TAs:\n\n  (1) First TA:\n";
  Pp.pp_ta a1; printf "\n  (2) Second TA:\n"; Pp.pp_ta a2; printf "\n";
  if debug_print then (printf "\n  >> Versatile symbol list: [ "; 
  verSyms |> List.map fst |> List.iter (fun x -> printf "%s " x); printf "]\n");
  let syms = a1.alphabet in (* TODO: Add a sanity check on alphabet based on set equality *)
  (* Consider symbols excluding epsilon and Boolean for I *)
  let syms_wo_epsilon = syms |> List.filter (fun s -> not (syms_equals s epsilon_symb) && not (syms_equals s ("B", 0))) in
  (* Find I := I_1 x I_2 first *)
  let start = cartesian_product_states [a1.start_state] [a2.start_state] debug_print |> List.hd in
  (* Based on I, get transitions for I *)
  let init_trans_ls = 
    cartesian_product_trans_from a1.start_state a2.start_state a1.transitions a2.transitions syms_wo_epsilon debug_print in
  (* Based on init state-starting transitions, find reachable states *)
  (* Based on E in list of reachable states, find transitions starting from E *)
  (* Then, given \Delta, find a list of duplicate states pairs *)
  (* Based on the duplicate states, remove transitions and rename states *)
  (* Introduce epsilon transitions to simplify \Delta *)
  let stats1, stats2 = a1.states, a2.states in
  let stats = cartesian_product_states stats1 stats2 debug_print in
  let _(* trans*) = 
    cartesian_product_trans stats1 stats2 a1.transitions a2.transitions syms verSyms debug_print in
  let res_ta = { states=stats ; alphabet=syms ; start_state=start ; transitions=init_trans_ls } in
  printf "\nResult of TA intersection: \n"; Pp.pp_ta res_ta; 
  res_ta |> rename_states debug_print


(** TODO: Simplify the resulting tree automata via :
    - Enhancing the look via renaming states
    - Removal of duplicate transitions
    - Epsilon introduction/reduction (double-check on this) *)

