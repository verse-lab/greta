/* michelson.mly — Micheline-text + legacy headers */

%{
open Ast
%}

%start <script> script

/* ===== Tokens ===== */
%token LBRACE RBRACE LPAREN RPAREN SEMI EOF
%token KW_PARAMETER KW_STORAGE KW_CODE

/* Numbers/strings/bytes */
%token <Ast.expr> INT
%token <string> STRING
%token <string> BYTES

/* Prim heads */
%token <string> INSTR /* e.g., PUSH, IF, DIP, LOOP, SWAP */

/* Five distinct annotation terminals (sigils) */
%token <string> AT_ANNOT     /* e.g., @x      */
%token <string> PCT_ANNOT    /* e.g., %entry  */
%token <string> COLON_ANNOT  /* e.g., :t      */
%token <string> AMP_ANNOT    /* e.g., &lbl    */
%token <string> HASH_ANNOT   /* e.g., #meta   */

%%

/* ===== Top level ===== */

script:
  | legacy_contract EOF                  { $1 }
  ;

/* Legacy .tz header form or expr */
legacy_contract:
  | KW_PARAMETER expr SEMI KW_STORAGE expr SEMI KW_CODE expr { SContract { parameter = $2; storage = $5; code = $8 } }
  | expr { SExpr $1 }
  ;

/* ===== Expressions ===== */

expr:
  | atoms { $1 }
  /* allow each sigil token to start an expression as an "annotation atom"
     — this is crucial for longest-match conflicts against annots5 below */
  | atom_annot                           { $1 }
  | instr                                { $1 }
  | LBRACE seq_items_opt RBRACE          { ESeq $2 } /* sequences */
  | LPAREN expr RPAREN                   { $2 } /* grouping */
  ;

instr:
  | INSTR annots5 expr { EPrim { name = $1; annots = $2; args = [$3] } } /* prim application */

atoms: 
  | INT                                  { EInt $1 }
  | STRING                               { EString $1 }
  | BYTES                                { EBytes $1 }
  ;

/* Turn any sigil token into an expr so it can compete with 'annots5'. */
atom_annot:
  | AT_ANNOT                             { EPrim { name = "ANNOT_AT";   annots = []; args = [EString $1] } }
  | PCT_ANNOT                            { EPrim { name = "ANNOT_PCT";  annots = []; args = [EString $1] } }
  | COLON_ANNOT                          { EPrim { name = "ANNOT_COLON";annots = []; args = [EString $1] } }
  | AMP_ANNOT                            { EPrim { name = "ANNOT_AMP";  annots = []; args = [EString $1] } }
  | HASH_ANNOT                           { EPrim { name = "ANNOT_HASH"; annots = []; args = [EString $1] } }
  ;

/* ===== Sequences ===== */

seq_items_opt:
  | /* empty */                          { [] }
  | expr                                 { [ $1 ] }
  | seq_items_opt SEMI expr              { $1 @ [ $3 ] }
  | seq_items_opt SEMI                   { $1 }        /* trailing ';' */
  ;

/* ===== Prim applications =====
   PRIM followed by a GREEDY mix of any sigil annot tokens (annots5),
   followed by zero or more arguments (args_opt).

   Longest-match conflict sites (≥5):
   For each of AT_ANNOT, PCT_ANNOT, COLON_ANNOT, AMP_ANNOT, HASH_ANNOT:
   at lookahead = that token, Menhir can:
     - shift to keep eating annots5 (greedy), OR
     - reduce annots5 and let that token start an argument (via atom_annot -> expr).
*/

/* Greedy kleene-star over a 5-way union of sigil tokens. */
annots5:
  | /* empty */                          { [] }
  | annots5 AT_ANNOT                     { $1 @ [ $2 ] }
  | annots5 PCT_ANNOT                    { $1 @ [ $2 ] }
  | annots5 COLON_ANNOT                  { $1 @ [ $2 ] }
  | annots5 AMP_ANNOT                    { $1 @ [ $2 ] }
  | annots5 HASH_ANNOT                   { $1 @ [ $2 ] }
;

