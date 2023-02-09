open Greta
open Stdlib

let () =
  let debug_print = true in
  let parser_file = "./lib/parser.mly" in
  let conflicts_file = "../_build/default/lib/parser.conflicts" in
  let versatile_syms = ["IF"] in
  let ta_initial = Converter.convertToTa parser_file versatile_syms in
  let ranked_symbols = ta_initial.alphabet in
  (* TODO: 
   * Replace example_tree with multiple examples based on conflicts_file *)
  let example_tree: Ta.tree = Examples.ex03 in
  let _ (* examples *): Ta.tree list = Examples.gen_examples conflicts_file in 
  (* TODO: 
   * run learner -> /\ -> normalize -> ta to cfg -> overwrite parser 
   * until all conflicts disappear (idea: connect with example generation) *)
  let ta_learned = Learner.learner example_tree ranked_symbols in
  let _(* rand_tree_pat *): Ta.tree = Examples.rand_tree_wpat ranked_symbols debug_print 0 example_tree in
  let _: Ta.tree = Examples.rand_tree ranked_symbols debug_print 0 in
  let _: bool = Run.accept ta_learned example_tree debug_print in
  (* let _: bool = Run.accept ta_learned rand_tree_pat debug_print in *)
  let _: Ta.ta = Operation.intersect ta_initial ta_learned versatile_syms debug_print in
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done;

(*** Assumptions made on the language designer (user of this tool):
 *   * Non-terminals representing boolean are specified with "cond" ^ s*
 *     - If this assumption changes, change data type to represent state to a tuple 
 *     - so that when taking X of lists of states, you won't generate unnecesary many states
 *     - but only the relevant ones, e.g., cond_exprCond_expr
 ***)


