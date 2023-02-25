open Greta
open Ta
open Treeutils
open Data

module E = Examples

let check_equality_of_trees (ts1: tree list) (ts2: tree list) (debug_print: bool): bool =
  List.fold_left2 (fun acc t1 t2 -> acc && (trees_equal t1 t2 debug_print)) true ts1 ts2

let () = 
  let open Printf in
  let debug_print = true in
  printf "\n\nTesting negation of trees ...\n\n";
  let res_tls = es |> List.map (E.negate_pat debug_print) in
  let comp_res = check_equality_of_trees res_tls es_neg debug_print in
  if comp_res then printf "\n\n\tTests passed\n\n" else printf "\n\n\tTests Failed\n\n";
  ()












