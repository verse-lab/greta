open Greta
open Ta
open Treeutils
open Data

module E = Examples
module L = Learner
module R = Runner

let check_equality_of_trees (ts1: tree list) (ts2: tree list) (debug_print: bool): bool =
  List.fold_left2 (fun acc t1 t2 -> acc && (trees_equal t1 t2 debug_print)) true ts1 ts2

let () = 
  let open Printf in
  let debug_print = true in
  let versatiles = [("IF", [2; 3])] in
  if debug_print then printf "\n\n1. Testing negation of trees ..\n\n";
  let res_tls = es |> List.map (E.negate_pat debug_print) in
  let comp_res = check_equality_of_trees res_tls es_neg debug_print in
  if comp_res then printf "\n\nTests of tree negation PASSED\n\n" 
  else printf "\n\nTests of tree negation FAILED\n\n";
  if debug_print then printf "\n\n2. Testing generation of random trees with a pattern ..\n\n";
  let rand_ts = es |> List.map (E.rand_tree_wpat syms_def0 debug_print) in
  let rand_ts_neg = es_neg |> List.map (E.rand_tree_wpat syms_def0 debug_print) in
  let rt0, rt0_neg = List.nth rand_ts 0, List.nth rand_ts_neg 0 in
  if debug_print then printf "\n\n3. Testing if learnedTAs accept rand tree w/ +pat and reject rand tree w/ -pat ..\n\n";
  let learned_ta0 = L.learner rt0 syms_def0 versatiles debug_print in
  let pos_res = R.accept learned_ta0 rt0 debug_print in
  let neg_res = R.accept learned_ta0 rt0_neg debug_print in
  if (pos_res && not neg_res) then printf "\nPASSED" else printf "\nFAILED"












