{
let reservedWords = [
  (* Keywords *)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
| ['%' ':'] ['A'-'Z' 'a'-'z' '_']+  { main lexbuf }
| '#' (_ # '\n')* '\n' { main lexbuf } (* line comments starting with # to end of line *)

 (* ---- integers ---- *)
| "-"? ['0'-'9']+   { Parser.INTV (Lexing.lexeme lexbuf) }
| "-"? "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ { Parser.INTV (Lexing.lexeme lexbuf) }

  (* ---- combined tokens like "(Left", "(Right", "(Pair" ---- these BEFORE the plain "(" rule *)
| "(" [' ' '\t' '\012' '\n']* "Left"     { Parser.LPAREN_LEFT }
| "(" [' ' '\t' '\012' '\n']* "Right"    { Parser.LPAREN_RIGHT }
| "(" [' ' '\t' '\012' '\n']* "Pair"     { Parser.LPAREN_PAIR }

  (* ---- single-character delimiters ---- *)
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "{" { Parser.LBRACE }
| "}" { Parser.RBRACE }
| ";" { Parser.SEMI }


  (* ---- annotations: emit the marker, let parser read following LCID ---- *)
| "@"  { Parser.AT }
| "%"  { Parser.PCT }
| ":"  { Parser.COLON }

 (* ---- section keywords ---- *)
| "code" { Parser.CODE }
| "parameter" { Parser.PARAM }
| "storage" { Parser.STORAGE }

  (* ---- booleans & simple data constructors ---- *)
| "True" { Parser.BOOL true }
| "False" { Parser.BOOL false }
| "Unit" { Parser.UNIT }
| "Pair"                                          { Parser.PAIR }
| "Some"                                          { Parser.SOME }
| "None"                                          { Parser.NONE }
| "Elt"                                           { Parser.ELT  }

  (* ---- control macros ---- *)
| "IF_LEFT"                               { Parser.IF_LEFT }
| "IF_RIGHT"                              { Parser.IF_RIGHT }
| "IF_NONE"                               { Parser.IF_NONE }
| "IF"                                    { Parser.IF }
| "LOOP"                                          { Parser.LOOP }
| "LOOP_LEFT"                                     { Parser.LOOP_LEFT }
| "ITER"                                          { Parser.ITER }
| "MAP"                                           { Parser.MAP }
| "LAMBDA"                                        { Parser.LAMBDA }
| "EXEC"                                          { Parser.EXEC }
| "DIP"                                           { Parser.DIP }

  (* ---- arithmetic / bitwise / comparison (specific tokens) ---- *)
| "ADD"                                           { Parser.ADD }
| "SUB"                                           { Parser.SUB }
| "MUL"                                           { Parser.MUL }
| "EDIV"                                          { Parser.EDIV }
| "ABS"                                           { Parser.ABS }
| "NEG"                                           { Parser.NEG }
| "LSL"                                           { Parser.LSL }
| "LSR"                                           { Parser.LSR }
| "AND"                                           { Parser.AND_ }   (* avoid Menhir keyword clash *)
| "OR"                                            { Parser.OR_  }
| "XOR"                                           { Parser.XOR }
| "NOT"                                           { Parser.NOT }
| "COMPARE"                                       { Parser.COMPARE }
| "EQ"                                            { Parser.EQ }
| "NEQ"                                           { Parser.NEQ }
| "LT"                                            { Parser.LT }
| "LE"                                            { Parser.LE }
| "GT"                                            { Parser.GT }
| "GE"                                            { Parser.GE }

  (* ---- identifiers ---- *)
| ['A'-'Z' '_' ]+    { Parser.MNEMONIC (Lexing.lexeme lexbuf) }
| ['a'-'z' '_' ]+    { Parser.LCID (Lexing.lexeme lexbuf) }

  (* ---- string literal (double quotes, no escapes here) ---- *)
| '"' (_ # '"')* '"'
    { 
      let lexeme = Lexing.lexeme lexbuf in
       Parser.STR (String.sub lexeme 1 (String.length lexeme - 2))    
    }

  (* ---- LIT introduced for list/set form "{ LIT ... }" ---- *)
| "Lit"                                   { Parser.LIT }

  (* ---- eof / fallback ---- *)
| eof { EOF }
| _ { 
      prerr_string "Lexer: Unknown character: "; 
      prerr_string (Lexing.lexeme lexbuf); 
      exit 1 
    }