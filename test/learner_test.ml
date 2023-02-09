open Greta

(* TODO:
 * Migrate examples and testing to this directory *)

let () = 
  let ta0 = Converter.convertToTa "./../lib/parser.mly" ["IF"] in
  let ranked_symbols = ta0.alphabet in
  let _ = Examples.rand_tree ranked_symbols true 0 in ()






