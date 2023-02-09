open Greta
open Stdlib

module P = Parser
module L = Lexer
module T = Ta
module C = Converter
module E = Examples
module Lr = Learner
module R = Run
module O = Operation

let () =
  let debug_print = true in
  let parser_file = "./lib/parser.mly" in
  let conflicts_file = "../_build/default/lib/parser.conflicts" in
  let versatile_syms = ["IF"] in
  let ta_initial:T.ta = C.convertToTa parser_file versatile_syms in
  let ranked_symbols:T.symbol list = ta_initial.alphabet in
  (* TODO: replace example_tree with multiple examples based on conflicts_file *)
  let example_tree:T.tree = E.ex03 in
  let _ (* examples *):T.tree list = E.gen_examples conflicts_file in 
  (* TODO: run learner -> /\ -> normalize -> ta to cfg -> overwrite parser 
     until all conflicts disappear (idea: connect with example generation) *)
  let ta_learned:T.ta = Lr.learner example_tree ranked_symbols in
  let _:T.tree = E.rand_tree ranked_symbols debug_print 0 in
  let _:bool = R.accept ta_learned example_tree debug_print in
  let _:T.ta = O.intersect ta_initial ta_learned versatile_syms debug_print in
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


