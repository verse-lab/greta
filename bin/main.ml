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

(* *************** Grammar REpair with Tree Automata *************** *)
(*                                                                   *)
(* Step 1: User feeds in inputs                                      *)
(* - 'versatile_syms' symbols that can have multiple arities         *)
(* - 'parser_file' grammar of the input language                     *)
(* - 'conflicts_file' {path}/{parser-file-name}.conflicts            *)
(*                                                                   *)
(* Step 2: User specifies preference                                 *)
(* - 0 or 1 if an expression contains >1 operators                   *)
(* - 0, 1 or 2 if an expression contains only 1 operator             *)
(*                                                                   *)
(* Step 3: New grammar is written on 'parser_file'                   *)
(* - If ambiguities still exist, ask user to run GRETA again         *)
(* - Repeat these steps until all the ambiguities are resolved       *)
(*                                                                   *)
(* ***************************************************************** *)


let () =
  let debug = true in
  let versatile_syms = ["IF"] in
  let parser_file, conflicts_file = "./lib/parser.mly", "./_build/default/lib/parser.conflicts" in
  let _test_conflicts_file = "./test/parser1.conflicts" in
  let ta_initial = C.convertToTa parser_file versatile_syms debug in
  let ranked_symbols = ta_initial.alphabet in
  (* if (Utils.check_conflicts conflicts_file debug) then  *)
  let tree_pairs: (T.tree * T.tree) list =
    E.gen_examples conflicts_file ranked_symbols debug in
  let fst_pair = match List.nth_opt tree_pairs 0 with 
    | None -> raise (Failure "No examples generated!")
    | Some (t1, t2) -> t1, t2 in
  if (U.tree_with_single_operator (fst fst_pair)) 
  then 
    (Printf.printf "\nTree involving only one symbol..\n";
    U.present_tree_pair_single_operator fst_pair;
    let chosen_index = read_int () in 
    C.specify_associativity parser_file chosen_index fst_pair debug)
  else
    (Printf.printf "\nTree involves more than one symbol..\n";
    U.present_tree_pair fst_pair;
    let chosen_index = read_int () in
    let example_tree: T.tree = if (chosen_index = 0) then fst fst_pair else snd fst_pair in
    let ta_learned = L.learner example_tree ranked_symbols debug in
    let _: bool = R.accept ta_learned example_tree debug in
    let ta_intersected = O.intersect ta_initial ta_learned versatile_syms debug in
    C.convertToGrammar ta_intersected versatile_syms debug parser_file);
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
  

