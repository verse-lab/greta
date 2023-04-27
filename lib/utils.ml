(** utilities for gluing togther the Menhir parser with the Sedlex lexer  *)
let revised_parse (lexbuf: Sedlexing.lexbuf): Ast.t =
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

let check_conflicts (conflicts_file: string) (debug_print: bool): bool =
  let open Printf in 
  let conflicts_file_exist = Sys.file_exists conflicts_file in
  let conflicts_file_nonempty = 
    let rec loop inp acc =
      match (read_line inp) with 
      | None -> (List.length acc) != 0
      | Some s -> loop inp (s::acc)
    in loop (open_in conflicts_file) []
  in
  let res = conflicts_file_exist && conflicts_file_nonempty in
  if debug_print then (printf "\n\n  >> Is there any conflicts in grammar?\n";
  if res then printf "\t\t\tYES\n\n" else printf "\t\t\tNO\n\n");
  res
