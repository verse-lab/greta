module P = Parser
module L = Lexer

open Stdlib

let () =
  (* Converter.mly_to_cfg (); *)
  (* Converter.cfg_to_ta (); *)
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done




