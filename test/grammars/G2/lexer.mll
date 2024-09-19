{
  open Lexing
  open Parser
  open Range
  
  exception Lexer_error of Range.t * string

  let reset_lexbuf (filename:string) (lnum:int) lexbuf : unit =
    lexbuf.lex_curr_p <- {
      pos_fname = filename;
      pos_cnum = 0;
      pos_bol = 0;
      pos_lnum = lnum;
    }

  let newline lexbuf =
    lexbuf.lex_curr_p <- { (lexeme_end_p lexbuf) with
      pos_lnum = (lexeme_end_p lexbuf).pos_lnum + 1;
      pos_bol = (lexeme_end lexbuf) }
    
  (* Boilerplate to define exceptional cases in the lexer. *)
  let unexpected_char lexbuf (c:char) : 'a =
    raise (Lexer_error (Range.lex_range lexbuf,
        Printf.sprintf "Unexpected character: '%c'" c))

  (* Lexing reserved words *)
  let reserved_words = [
  (* Keywords *)
  ("else", ELSE);
  ("if", IF);
  ("int", TINT);
  ("return", RETURN);
  ("while", WHILE);
  
  (* Symbols *)
  ( ";", SEMI);
  ( "{", LBRACE);
  ( "}", RBRACE);
  ( "+", PLUS);
  ( "-", DASH);
  ( "*", STAR);
  ( "=", EQ);
  ( "(", LPAREN);
  ( ")", RPAREN);
  ]

let (symbol_table : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024
  let _ =
    List.iter (fun (str,t) -> Hashtbl.add symbol_table str t) reserved_words

  let create_token lexbuf =
    let str = lexeme lexbuf in 
    try (Hashtbl.find symbol_table str) 
    with _ -> IDENT str

  (* Lexing comments and strings *)
  let string_buffer = ref (Bytes.create 2048)
  let string_end = ref 0
  let start_lex = ref (Range.start_of_range Range.norange)

  let start_pos_of_lexbuf lexbuf : pos =
    (Range.pos_of_lexpos (lexeme_start_p lexbuf))

  let lex_long_range lexbuf : Range.t =
    let end_p = lexeme_end_p lexbuf in
    mk_range end_p.pos_fname (!start_lex) (pos_of_lexpos end_p)  

  let reset_str () = string_end := 0

  let add_str ch =
    let x = !string_end in
    let buffer = !string_buffer
    in
      if x = Bytes.length buffer then
        begin
          let new_buffer = Bytes.create (x*2) in
          Bytes.blit buffer 0 new_buffer 0 x;
          Bytes.set new_buffer x ch;
          string_buffer := new_buffer;
          string_end := x+1
        end
      else
        begin
          Bytes.set buffer x ch;
          string_end := x+1
        end

  let get_str () = Bytes.sub (!string_buffer) 0 (!string_end)
}

let newline = '\n' | ('\r' '\n') | '\r'
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = uppercase | lowercase
let whitespace = ['\t' ' ']
let digit = ['0'-'9']
let hexdigit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']

rule token = parse
  | eof { EOF }
  | "/*" { start_lex := start_pos_of_lexbuf lexbuf; comments 0 lexbuf }

  | '"' { reset_str(); start_lex := start_pos_of_lexbuf lexbuf; string lexbuf }

  | lowercase (digit | character | '_')* { create_token lexbuf }
  | digit+ | "0x" hexdigit+ { INT (Int64.of_string (lexeme lexbuf)) }
  | whitespace+ { token lexbuf }
  | newline { newline lexbuf; token lexbuf }

  | ';' | '{' | '}' | '+' | '-' | '*' | '=' 
  | '(' | ')' 
    { create_token lexbuf }

  | _ as c { unexpected_char lexbuf c }

and comments level = parse
  | "*/" { if level = 0 then token lexbuf
	   else comments (level-1) lexbuf }
  | "/*" { comments (level+1) lexbuf}
  | [^ '\n'] { comments level lexbuf }
  | "\n" { newline lexbuf; comments level lexbuf }
  | eof	 { raise (Lexer_error (lex_long_range lexbuf,
             Printf.sprintf "comments are not closed")) }


and string  = parse
  | '"'  { STRING (Bytes.to_string (get_str())) }
  | '\\' { add_str(escaped lexbuf); string lexbuf }
  | '\n' { add_str '\n'; newline lexbuf; string lexbuf }
  | eof  { raise (Lexer_error (lex_long_range lexbuf,
             Printf.sprintf "String is not terminated")) }
  | _    { add_str (Lexing.lexeme_char lexbuf 0); string lexbuf }

and escaped = parse
  | 'n'    { '\n' }
  | 't'    { '\t' }
  | '\\'   { '\\' }
  | '"'    { '\034'  }
  | '\''   { '\'' }
  | ['0'-'9']['0'-'9']['0'-'9']
    {
      let x = int_of_string(lexeme lexbuf) in
      if x > 255 then
        raise (Lexer_error (lex_long_range lexbuf,
          (Printf.sprintf "%s is an illegal escaped character constant" (lexeme lexbuf))))
      else
        Char.chr x
    }
  | [^ '"' '\\' 't' 'n' '\'']
    { raise (Lexer_error (lex_long_range lexbuf,
        (Printf.sprintf "%s is an illegal escaped character constant" (lexeme lexbuf) ))) }