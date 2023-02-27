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
  let debug = true in
  (* *** Inputs neede for this framework *** *)
  let parser_file, conflicts_file, versatile_syms = 
    "./lib/parser.mly", "./test/parser0.conflicts", ["IF"] in
  (* TODO: change conflicts_file to "./_build/default/lib/parser.conflicts" *)
  let ta_initial = C.convertToTa parser_file versatile_syms debug in
  let ranked_symbols = ta_initial.alphabet in
  let tree_pairs: (T.tree * T.tree) list = 
    E.gen_examples conflicts_file ranked_symbols debug in
  let example_tree: T.tree = List.nth tree_pairs 2 |> snd in
  (* TODO: Currently testing drawing trees in-progress *)
  let tree_test = tree_pairs |> List.hd |> fst in 
  let _ = D.draw_tree tree_test "testA" in
  (* TODO: 
   * run learner -> /\ -> normalize -> ta to cfg -> overwrite parser 
   * until all conflicts disappear (idea: connect with example generation) *)
  let ta_learned = L.learner example_tree ranked_symbols debug in
  let _: bool = R.accept ta_learned example_tree debug in
  let ta_intersected = O.intersect ta_initial ta_learned versatile_syms debug in
  C.convertToGrammar ta_intersected versatile_syms debug "./test/parser0cp.conflicts";
  (* TODO: change to 'parser_file' *)
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


