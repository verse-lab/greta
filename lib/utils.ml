(** utilities for gluing togther the Menhir parser with the Sedlex lexer  *)
let revised_parse lexbuf =
  let tok () =
    let tok = Lexer.token lexbuf in
    let (st,ed) = Sedlexing.lexing_positions lexbuf in
    (tok,st,ed) in
  MenhirLib.Convert.Simplified.traditional2revised
    Parser.program tok

let parse lexbuf =
  try revised_parse lexbuf with Parser.Error -> failwith "failed to parse" 

let parse_string str =
  parse (Sedlexing.Utf8.from_string str)

(** utilities for reading lines from parser *)
let read_line i = try Some (input_line i) with End_of_file -> None

let starts tk s = String.starts_with ~prefix:tk s
