open Greta
open Stdlib

module C = Converter
module O = Operation
module E = Examples
module L = Learner
module R = Runner
module U = Treeutils
module D = Draw
module T = Ta

let () =
  let debug = true in
  (* *** Inputs neede for this framework *** *)
  let versatile_syms = ["IF"] in
  let parser_file, conflicts_file = 
    "./lib/parser.mly", "./_build/default/lib/parser.conflicts" in
  (* let test_parser_file = "./test/test_parser.mly" in  *)
  let test_conflicts_file = "./test/parser1.conflicts" in
  let ta_initial = C.convertToTa parser_file versatile_syms debug in
  let ranked_symbols = ta_initial.alphabet in
  (* if (Utils.check_conflicts conflicts_file debug) then  *)
  let tree_pairs: (T.tree * T.tree) list = 
  (* Testing gen_examples with 'test_conflicts_file' below *)
    E.gen_examples test_conflicts_file ranked_symbols debug in
  let fst_pair = match List.nth_opt tree_pairs 2 with 
    | None -> raise (Failure "No examples generated!")
    | Some (t1, t2) -> t1, t2 in
  U.present_tree_pair fst_pair;
  let chosen_index = read_int () in
  let example_tree: T.tree = if (chosen_index = 0) then fst fst_pair else snd fst_pair in
  (* TODO: run learner -> /\ -> normalize -> ta to cfg -> overwrite parser 
   * until all conflicts disappear (idea: connect with example generation) *)
  let ta_learned = L.learner example_tree ranked_symbols debug in
  let _: bool = R.accept ta_learned example_tree debug in
  let ta_intersected = O.intersect ta_initial ta_learned versatile_syms debug in
  C.convertToGrammar ta_intersected versatile_syms debug parser_file;
  if (Utils.check_conflicts conflicts_file debug) then U.ask_again parser_file;
  (* while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done; *)

(*** Assumptions made on the language designer (user of this tool):
 *   * Non-terminals representing boolean are specified with "cond" ^ s*
 *     - If this assumption changes, change data type to represent state to a tuple 
 *     - so that when taking X of lists of states, you won't generate unnecesary many states
 *     - but only the relevant ones, e.g., cond_exprCond_expr
 ***)

(* Notes: 
 *      - To add a loop until user selects the right index
 *      - To draw trees using Graphics (starting from let _ = D.draw_tree tree_test "testA") 
 *      - Tested with "./test/parser0.conflicts" as 'conflicts_file'
 *      - Tested with test_parser_file (test_parser_file = "./test/test_parser.mly")
 *      - ... 
 *)
  

