open Discota
open Stdlib

module P = Parser
module L = Lexer
module C = Converter
module E = Examples
module Lr = Learner
module T = Ta
module R = Run

let () =
  let parser_file = "./lib/parser.mly" in
  let conflicts_file = "../_build/default/lib/parser.conflicts" in
  let ta_origin:T.ta = C.convertToTa parser_file in
  let ranked_symbols:T.symbol list = ta_origin.alphabet in
  let example_tree:T.tree = E.ex03 in
  let _ (* examples *):T.tree list = E.gen_examples conflicts_file in 
  let learned_ta:T.ta = Lr.learner example_tree ranked_symbols in
  let _:T.tree = E.rand_tree ranked_symbols true 0 in
  let _:bool = R.accept learned_ta example_tree true in
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done;




