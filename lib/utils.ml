open Graphics

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

(** utilities for drawing *)
let pastel_yellow = rgb 255 255 153
let pastel_purple = rgb 150 120 250
let pastel_green = rgb 204 255 204
let dark_orange = rgb 255 153 0
let light_green = rgb 204 255 204
let dark_navy = rgb 0 21 51

(** clear window and set background color *)
let set_background color = 
  let fg = foreground 
  in
      set_color color;
      fill_rect 0 0 (size_x ()) (size_y ());
      set_color fg


