(* mly_parse.ml - High-level parsing interface for .mly files *)

open Mly_types

exception Parse_error of string * Lexing.position

let parse_mly_channel ?(filename="<input>") ic =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname = filename };
  try
    let result = Mly_parser.mly_file Mly_lexer.token lexbuf in

    (* Post-processing: handle epsilon productions *)
    (* If a production has empty RHS, add EMPTY terminal for compatibility *)
    let productions = List.map (fun prod ->
      if prod.rhs = [] then
        { prod with rhs = [T { name = Converter.empty_term; binding = None }] }
      else
        prod
    ) result.productions in

    { result with productions }

  with
  | Mly_lexer.Lexer_error (msg, pos) ->
      raise (Parse_error (msg, pos))
  | Mly_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      raise (Parse_error ("Syntax error", pos))

let parse_mly_file filename =
  let ic = open_in filename in
  try
    let result = parse_mly_channel ~filename ic in
    close_in ic;
    result
  with e ->
    close_in ic;
    raise e

let parse_mly_string ?(filename="<string>") s =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname = filename };
  try
    let result = Mly_parser.mly_file Mly_lexer.token lexbuf in

    (* Post-processing: handle epsilon productions *)
    let productions = List.map (fun prod ->
      if prod.rhs = [] then
        { prod with rhs = [T { name = Converter.empty_term; binding = None }] }
      else
        prod
    ) result.productions in

    { result with productions }

  with
  | Mly_lexer.Lexer_error (msg, pos) ->
      raise (Parse_error (msg, pos))
  | Mly_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      raise (Parse_error ("Syntax error", pos))
