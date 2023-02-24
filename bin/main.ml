open Greta
open Stdlib

module C = Converter
module O = Operation
module E = Examples
module L = Learner
module R = Runner
module D = Draw
module T = Ta

let () =
  let debug_print = true in
  let parser_file = "./lib/parser.mly" in
  (* let conflicts_file = "./_build/default/lib/parser.conflicts" in *)
  let conflicts_file = "./test/parser0.conflicts" in
  let versatile_syms = ["IF"] in
  (* TODO: 
   * pass in debug_print to have clean output in converting to TA *)
  let ta_initial = C.convertToTa parser_file versatile_syms in
  let ranked_symbols = ta_initial.alphabet in
  (* TODO: 
   * Replace example_tree with multiple examples based on conflicts_file *)
  let example_tree: T.tree = E.ex03 in
  let _: T.tree = E.negate_pat debug_print example_tree in
  let _: T.tree = E.negate_pat debug_print E.ex04 in
  let _: T.tree = E.negate_pat debug_print E.ex05 in
  let tree_pairs: (T.tree * T.tree) list = 
    E.gen_examples conflicts_file ranked_symbols debug_print in 
  (* Currently testing drawing trees in-progress *)
  let tree_test = tree_pairs |> List.hd |> fst in 
  let _ = D.draw_tree tree_test "testA" in
  (* TODO: 
   * run learner -> /\ -> normalize -> ta to cfg -> overwrite parser 
   * until all conflicts disappear (idea: connect with example generation) *)
  let ta_learned = L.learner example_tree ranked_symbols in
  let _(* rand_tree_pat *): T.tree = E.rand_tree_wpat ranked_symbols debug_print 0 example_tree in
  let _: T.tree = E.rand_tree ranked_symbols debug_print 0 in
  let _: bool = R.accept ta_learned example_tree debug_print in
  (* let _: bool = Run.accept ta_learned rand_tree_pat debug_print in *)
  let _: T.ta = O.intersect ta_initial ta_learned versatile_syms debug_print in
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


