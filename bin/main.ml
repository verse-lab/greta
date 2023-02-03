open Discota
open Stdlib

module P = Parser
module L = Lexer
module C = Converter
module E = Examples
module Lr = Learner
module T = Ta
module R = Run
module O = Operation

let () =
  let parser_file = "./lib/parser.mly" in
  let conflicts_file = "../_build/default/lib/parser.conflicts" in
  let debug_print = true in
  let ta_origin:T.ta = C.convertToTa parser_file in
  let ranked_symbols:T.symbol list = ta_origin.alphabet in
  let example_tree:T.tree = E.ex03 in
  let _ (* examples *):T.tree list = E.gen_examples conflicts_file in 
  let ta_learned:T.ta = Lr.learner example_tree ranked_symbols in
  let _:T.tree = E.rand_tree ranked_symbols debug_print 0 in
  let _:bool = R.accept ta_learned example_tree debug_print in
  let _:T.ta = O.intersect ta_origin ta_learned debug_print in
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done;




