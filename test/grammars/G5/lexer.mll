{
  open Lexing
  open Parser
  open Range
  
  exception Lexer_error of Range.t * string

  let pos_of_lexpos (p:Lexing.position) : pos =
    mk_pos (p.pos_lnum) (p.pos_cnum - p.pos_bol)

  let mk_lex_range (p1:Lexing.position) (p2:Lexing.position) : Range.t =
    mk_range p1.pos_fname (pos_of_lexpos p1) (pos_of_lexpos p2)

  let lex_range lexbuf : Range.t = 
    mk_lex_range (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)

  let reset_lexbuf (filename:string) lexbuf : unit =
    lexbuf.lex_curr_p <- {
      pos_fname = filename;
      pos_cnum = 0;
      pos_bol = 0;
      pos_lnum = 1;
    }
    
  let unexpected_char lexbuf (c:char) : 'a =
    raise (Lexer_error (lex_range lexbuf,
        Printf.sprintf "Unexpected character: '%c'" c))
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = uppercase | lowercase
let whitespace = ['\t' ' ' '\r' '\n']
let digit = ['0'-'9']

rule token = parse
  | eof         { EOF }
  | whitespace+ { token lexbuf }  (* skip whitespace *)
  | character+  { VAR (lex_range lexbuf, lexeme lexbuf) }
  | '|'         { OR (lex_range lexbuf) }
  | '&'         { AND (lex_range lexbuf) }
  | '~'         { NOT (lex_range lexbuf) }
  | '('         { LPAREN (lex_range lexbuf) }
  | ')'         { RPAREN (lex_range lexbuf) }
  | _ as c      { unexpected_char lexbuf c }