open Discota
open Stdlib

module P = Parser
module L = Lexer
module C = Converter
module E = Examples
module Lr = Learner
module T = Ta

let () =
  let parser_file = "./lib/parser.mly" in
  let conflicts_file = "../_build/default/lib/parser.conflicts" in
  let ta_origin = C.convertToTa parser_file in
  let ranked_symbols = ta_origin.alphabet in
  let _ (* examples *): T.tree list = E.gen_examples conflicts_file in 
  let _ (* ta_example *): T.ta = Lr.learner E.ex02 ranked_symbols in
  let _: T.tree = E.rand_tree ranked_symbols true 0 in
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done;




