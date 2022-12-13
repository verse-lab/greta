module P = Parser
module L = Lexer
module C = Cfg
module T = Ta

open Stdlib

let () =
  let _ = C.mly_to_cfg ("parser.mly") |> T.cfg_to_ta in
  (* Cfg.cfg_to_ta (); *)
  while true do
    let inp = read_line () in
    match Utils.parse_string inp with
    | ast -> print_endline @@ Ast.show ast
    | exception e -> print_endline @@ Printexc.to_string e
  done




