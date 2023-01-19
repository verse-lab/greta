open Discota
open Stdlib

module P = Parser
module L = Lexer
module C = Converter
module E = Examples
module Lr = Learner

let () =
  let open Printf in
  let conflicts_file = "../_build/default/bin/parser.conflicts" in
  let cfg_origin = C.mly_to_cfg ("./lib/parser.mly") in
  let ta_origin (* ta_origin *) =
    if Sys.file_exists conflicts_file
    then printf "Conflicts exist\n\n"
    else printf "Conflicts do NOT exist\n\n";
    C.cfg_to_ta (cfg_origin)
  in
  (* let _ (* examples *) = E.gen_examples (conflicts_file) in *)
  let ranked_symbols = ta_origin.alphabet in
  let _ (* ta_example *) = 
    printf "Now learn tree automaton from an example:\n";
    Lr.learner E.ex00 ranked_symbols
  in
  (* Cfg.cfg_to_ta (); *)
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done;




