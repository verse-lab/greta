(* This file glues togther the Menhir parser with the Sedlex lexer. Shouldn't need to be modified  *)

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

let read_line i = try Some (input_line i) with End_of_file -> None

let lines_from_file (filename : string): string list =
  let rec loop inp acc =
    match (read_line inp) with
    | None -> List.rev acc
    | Some s -> loop inp (s :: acc) in 
    loop (open_in filename) []




