module P = Parser

let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? '0' | ('1' ..'9'), Star digit]

let ustr_to_str str =
  Array.to_seq str |> Seq.map Uchar.to_char |> String.of_seq

let rec token lexbuf =
  match%sedlex lexbuf with
  | (' ' | '\013' | '\009' | '\012') -> token lexbuf (* skip whitespace *)
  | '\010' -> Sedlexing.new_line lexbuf; token lexbuf
  | "(*" -> comment 0 lexbuf; token lexbuf (* comments *)
  | number -> P.INT (int_of_string @@ ustr_to_str (Sedlexing.lexeme lexbuf))
  | "*" -> P.MUL
  | "+" -> P.PLUS
  | "(" -> P.LPAREN
  | ")" -> P.RPAREN
  | eof -> P.EOF
  | _ -> P.EOF
and comment depth lexbuf =
  match%sedlex lexbuf with
  | "(*" -> comment (depth + 1) lexbuf
  | "*)" -> if depth = 0 then () else comment (depth - 1) lexbuf
  | '\010' ->
    Sedlexing.new_line lexbuf; comment depth lexbuf
  | eof -> ()
  | Compl '*' -> comment depth lexbuf
  | _ -> comment depth lexbuf
