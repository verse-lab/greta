{
let reservedWords = [
  (* Keywords *)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
| ['%' ':'] ['A'-'Z' 'a'-'z' '_']+  { main lexbuf }
| '#' (_ # '\n')* '\n' { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (Lexing.lexeme lexbuf) }
| "-"? "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
    { Parser.INTV (Lexing.lexeme lexbuf) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "{" { Parser.LBRACE }
| "}" { Parser.RBRACE }
| ";" { Parser.SEMI }

| "code" { Parser.CODE }
| "parameter" { Parser.PARAM }
| "storage" { Parser.STORAGE }
| "True" { Parser.BOOL true }
| "False" { Parser.BOOL false }
| "Unit" { Parser.UNIT }
| "Pair" { Parser.PAIR }
| "Left" { Parser.LEFT }
| "Right" { Parser.RIGHT }
| "Some" { Parser.SOME }
| "None" { Parser.NONE }
| "Elt" { Parser.ELT }

| ['A'-'Z' '_' ]+
    { Parser.MNEMONIC (Lexing.lexeme lexbuf) }
| ['a'-'z' '_' ]+
    { Parser.LCID (Lexing.lexeme lexbuf) }
| '"' (_ # '"')* '"'
    { let lexeme = Lexing.lexeme lexbuf in
       Parser.STR (String.sub lexeme 1 (String.length lexeme - 2))
    }
| eof { EOF }
| _ { prerr_string "Lexer: Unknown character: "; prerr_string (Lexing.lexeme lexbuf); exit 0 }