module P = Parser
module L = Lexer
module C = Converter
module E = Examples

open Stdlib

let () =
  let open Printf in
  let conflicts_file = "_build/default/parser.conflicts" in
  let cfg_origin = C.mly_to_cfg ("parser.mly") in
  let _ (* ta_origin *) =
    if Sys.file_exists conflicts_file
    then printf "Conflicts exist\n\n"
    else printf "Conflicts do NOT exist\n\n";
    C.cfg_to_ta (cfg_origin)
  in
  let _ (* examples *) = E.gen_examples (conflicts_file) in
  (* Cfg.cfg_to_ta (); *)
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done;




