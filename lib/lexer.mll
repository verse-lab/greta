(* A lexer for the simple boolean logic grammar specified in grammar.txt *)

{
  open Lexing
  open Parser
  open Range
  
  exception Lexer_error of Range.t * string

  (* Creates a Range.pos from the Lexing.position data *)
  let pos_of_lexpos (p:Lexing.position) : pos =
    mk_pos (p.pos_lnum) (p.pos_cnum - p.pos_bol)

  (* Here, pos_lnum is the line number; pos_bol is the offset of the
     beginning of the line (number of characters between the beginning of
     the lexbuf and the beginning of the line); pos_cnum is the offset of
     the position (number of characters between the beginning of the
     lexbuf and the position). *)
    
  (* Creates a Range.t from two Lexing.positions *)
  let mk_lex_range (p1:Lexing.position) (p2:Lexing.position) : Range.t =
    mk_range p1.pos_fname (pos_of_lexpos p1) (pos_of_lexpos p2)

  (* Expose the lexer state as a Range.t value *)
  let lex_range lexbuf : Range.t = 
    mk_lex_range (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)

  (* Reset the lexer state *)
  let reset_lexbuf (filename:string) lexbuf : unit =
    lexbuf.lex_curr_p <- {
      pos_fname = filename;
      pos_cnum = 0;
      pos_bol = 0;
      pos_lnum = 1;
    }
    
  (* Boilerplate to define exceptional cases in the lexer. *)
  let unexpected_char lexbuf (c:char) : 'a =
    raise (Lexer_error (lex_range lexbuf,
        Printf.sprintf "Unexpected character: '%c'" c))
}


(* Basic regular expressions for the tokens of this grammar *)
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = uppercase | lowercase
let whitespace = ['\t' ' ' '\r' '\n']
let digit = ['0'-'9']

(* Returns a token of type as specified in parser.mly 
   
   Each token carries a Range.t value indicating its
   position in the file.
*)
rule token = parse
  | eof         { EOF }
  | whitespace+ { token lexbuf }  (* skip whitespace *)
  | "true"      { TRUE (lex_range lexbuf) }
  | "false"     { FALSE (lex_range lexbuf) }
  | character+  { VAR (lex_range lexbuf, lexeme lexbuf) }
  | '|'         { BAR (lex_range lexbuf) }
  | '&'         { AMPER (lex_range lexbuf) }
  | '~'         { TILDE (lex_range lexbuf) }
  | "->"        { ARR (lex_range lexbuf) }
  | '('         { LPAREN (lex_range lexbuf) }
  | ')'         { RPAREN (lex_range lexbuf) }
  | _ as c      { unexpected_char lexbuf c }