{
  open Mly_parser

  exception Lexer_error of string * Lexing.position

  let buffer = Buffer.create 256
  let brace_depth = ref 0
  let angle_depth = ref 0
  let comment_depth = ref 0
}

let whitespace = [' ' '\t' '\r']
let newline = '\n'
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']

rule token = parse
  | whitespace+              { token lexbuf }
  | newline                  { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^ '\n']*           { token lexbuf }
  | "/*"                     { c_comment lexbuf; token lexbuf }
  | "(*"                     { comment_depth := 1; ocaml_comment lexbuf; token lexbuf }
  | "%{"                     { Buffer.clear buffer; preamble_code lexbuf }
  | "%%"                     { PERCENT_PERCENT }
  | "%token"                 { PERCENT_TOKEN }
  | "%start"                 { PERCENT_START }
  | "%type"                  { PERCENT_TYPE }
  | "%left"                  { PERCENT_LEFT }
  | "%right"                 { PERCENT_RIGHT }
  | "%nonassoc"              { PERCENT_NONASSOC }
  | "%prec"                  { PERCENT_PREC }
  | '{'                      { Buffer.clear buffer; brace_depth := 1; action lexbuf }
  | '<'                      { Buffer.clear buffer; angle_depth := 1; type_spec lexbuf }
  | ':'                      { COLON }
  | '|'                      { PIPE }
  | ';'                      { SEMICOLON }
  | '='                      { EQUALS }
  | (lowercase | uppercase) identchar* as id { IDENT id }
  | eof                      { EOF }
  | _ as c                   { raise (Lexer_error (Printf.sprintf "Unexpected character: %c" c, lexbuf.lex_curr_p)) }

and preamble_code = parse
  | "%}"                     { OCAML_CODE (Buffer.contents buffer) }
  | "(*"                     { Buffer.add_string buffer "(*";
                               comment_depth := 1;
                               preamble_ocaml_comment lexbuf;
                               preamble_code lexbuf }
  | newline                  { Lexing.new_line lexbuf; Buffer.add_char buffer '\n'; preamble_code lexbuf }
  | _ as c                   { Buffer.add_char buffer c; preamble_code lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated preamble %{ ... %}", lexbuf.lex_curr_p)) }

and action = parse
  | '{'                      { Buffer.add_char buffer '{'; incr brace_depth; action lexbuf }
  | '}'                      { decr brace_depth;
                               if !brace_depth = 0 then ACTION (Buffer.contents buffer)
                               else (Buffer.add_char buffer '}'; action lexbuf) }
  | '"'                      { Buffer.add_char buffer '"'; string_in_action lexbuf; action lexbuf }
  | '\''                     { Buffer.add_char buffer '\''; char_in_action lexbuf; action lexbuf }
  | "(*"                     { Buffer.add_string buffer "(*";
                               comment_depth := 1;
                               action_ocaml_comment lexbuf;
                               action lexbuf }
  | newline                  { Lexing.new_line lexbuf; Buffer.add_char buffer '\n'; action lexbuf }
  | _ as c                   { Buffer.add_char buffer c; action lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated action { ... }", lexbuf.lex_curr_p)) }

and type_spec = parse
  | '<'                      { Buffer.add_char buffer '<'; incr angle_depth; type_spec lexbuf }
  | '>'                      { decr angle_depth;
                               if !angle_depth = 0 then TYPE_SPEC (Buffer.contents buffer)
                               else (Buffer.add_char buffer '>'; type_spec lexbuf) }
  | newline                  { Lexing.new_line lexbuf; Buffer.add_char buffer '\n'; type_spec lexbuf }
  | _ as c                   { Buffer.add_char buffer c; type_spec lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated type specification < ... >", lexbuf.lex_curr_p)) }

and c_comment = parse
  | "*/"                     { () }
  | newline                  { Lexing.new_line lexbuf; c_comment lexbuf }
  | _                        { c_comment lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated C comment /* ... */", lexbuf.lex_curr_p)) }

and ocaml_comment = parse
  | "*)"                     { decr comment_depth; if !comment_depth > 0 then ocaml_comment lexbuf }
  | "(*"                     { incr comment_depth; ocaml_comment lexbuf }
  | newline                  { Lexing.new_line lexbuf; ocaml_comment lexbuf }
  | _                        { ocaml_comment lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated OCaml comment (* ... *)", lexbuf.lex_curr_p)) }

and string_in_action = parse
  | '"'                      { Buffer.add_char buffer '"' }
  | '\\' (_ as c)            { Buffer.add_char buffer '\\'; Buffer.add_char buffer c; string_in_action lexbuf }
  | newline                  { Lexing.new_line lexbuf; Buffer.add_char buffer '\n'; string_in_action lexbuf }
  | _ as c                   { Buffer.add_char buffer c; string_in_action lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated string in action", lexbuf.lex_curr_p)) }

and char_in_action = parse
  | '\''                     { Buffer.add_char buffer '\'' }
  | '\\' (_ as c)            { Buffer.add_char buffer '\\'; Buffer.add_char buffer c; char_in_action lexbuf }
  | _ as c                   { Buffer.add_char buffer c; char_in_action lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated char literal in action", lexbuf.lex_curr_p)) }

and preamble_ocaml_comment = parse
  | "*)"                     { Buffer.add_string buffer "*)";
                               decr comment_depth;
                               if !comment_depth > 0 then preamble_ocaml_comment lexbuf }
  | "(*"                     { Buffer.add_string buffer "(*";
                               incr comment_depth;
                               preamble_ocaml_comment lexbuf }
  | newline                  { Lexing.new_line lexbuf; Buffer.add_char buffer '\n';
                               preamble_ocaml_comment lexbuf }
  | _ as c                   { Buffer.add_char buffer c; preamble_ocaml_comment lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated OCaml comment in preamble", lexbuf.lex_curr_p)) }

and action_ocaml_comment = parse
  | "*)"                     { Buffer.add_string buffer "*)";
                               decr comment_depth;
                               if !comment_depth > 0 then action_ocaml_comment lexbuf }
  | "(*"                     { Buffer.add_string buffer "(*";
                               incr comment_depth;
                               action_ocaml_comment lexbuf }
  | newline                  { Lexing.new_line lexbuf; Buffer.add_char buffer '\n';
                               action_ocaml_comment lexbuf }
  | _ as c                   { Buffer.add_char buffer c; action_ocaml_comment lexbuf }
  | eof                      { raise (Lexer_error ("Unterminated OCaml comment in action", lexbuf.lex_curr_p)) }
