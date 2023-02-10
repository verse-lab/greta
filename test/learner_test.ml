open Greta

module E = Examples
(* TODO:
 * Migrate examples and testing to this directory
 * Create different parser files with simple to complex grammars and test
 *  *)

let () = 
  let ta0 = Converter.convertToTa "./../lib/parser.mly" ["IF"] in
  let ranked_symbols = ta0.alphabet in
  let _ = E.rand_tree ranked_symbols true 0 in 
  let _ = E.rand_tree_wpat ranked_symbols true 0 E.ex02 in
  Printf.printf "\n\nTesting 123 ...\n\n"






