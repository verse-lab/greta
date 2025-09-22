// Michelson (per Octez “Active Michelson”) 

%{
open Ast
%}

%start <script> script

/* ===== Structural tokens ===== */
%token LBRACE RBRACE LPAREN RPAREN SEMI EOF

/* ===== Header keywords (legacy .tz entry) ===== */
%token KW_PARAMETER KW_STORAGE KW_CODE

/* ===== Atoms ===== */
%token <Ast.expr> INT
%token <string>   STRING
%token <string>   BYTES
%token <string>   NAT

/* ===== Instruction heads (subset from spec; extend as needed) ===== */
%token KW_DROP_SI KW_DROP KW_DUP_SI KW_DUP KW_SWAP KW_DIG KW_DUG
%token KW_PUSH
%token KW_SOME KW_NONE KW_UNIT KW_NEVER
%token KW_IF KW_IF_NONE KW_IF_LEFT KW_IF_RIGHT KW_IF_CONS
%token KW_PAIR_SI KW_PAIR KW_UNPAIR KW_CAR KW_CDR
%token KW_LEFT KW_RIGHT
%token KW_NIL KW_CONS
%token KW_SIZE
%token KW_EMPTY_SET KW_EMPTY_MAP KW_EMPTY_BIG_MAP
%token KW_MAP KW_ITER
%token KW_MEM KW_GET_SI KW_GET KW_UPDATE
%token KW_LOOP KW_LOOP_LEFT
%token KW_LAMBDA KW_LAMBDA_REC
%token KW_EXEC KW_APPLY
%token KW_DIP
%token KW_FAILWITH KW_CAST KW_RENAME
%token KW_CONCAT KW_SLICE KW_PACK KW_UNPACK
%token KW_ADD KW_SUB KW_MUL KW_EDIV KW_ABS KW_ISNAT KW_INT KW_NEG KW_LSL KW_LSR
%token KW_OR KW_AND KW_XOR KW_NOT KW_COMPARE KW_EQ KW_NEQ KW_LT KW_GT KW_LE KW_GE
%token KW_SELF KW_SELF_ADDRESS KW_CONTRACT KW_TRANSFER_TOKENS KW_SET_DELEGATE

/* ===== Data constructors (keywords in data context) ===== */
%token KD_Unit KD_True KD_False KD_Pair KD_Left KD_Right KD_Some KD_None KD_Lambda_rec

/* ===== Type keywords (lowercase) ===== */
%token TYPE_option TYPE_list TYPE_set TYPE_operation TYPE_contract TYPE_ticket
%token TYPE_pair TYPE_or TYPE_lambda TYPE_map TYPE_big_map
%token TYPE_bls12_381_g1 TYPE_bls12_381_g2 TYPE_bls12_381_fr
%token TYPE_sapling_transaction TYPE_sapling_state
%token TYPE_chest TYPE_chest_key

/* Comparable type atoms (lowercase) */
%token CTYPE_unit CTYPE_never CTYPE_bool CTYPE_int CTYPE_nat CTYPE_string CTYPE_chain_id
%token CTYPE_bytes CTYPE_mutez CTYPE_key_hash CTYPE_key CTYPE_signature CTYPE_timestamp
%token CTYPE_address

/* ===== Optional infix macro tokens (for OP-STYLE conflicts) ===== */
%token PLUS MINUS STAR SLASH CARET
%token DAMP DPIPE
%token RLT RLE RGT RGE REQ RNEQ
%token CONS

/* ===== Annotation sigils (only used as annotations, not as standalone exprs here) ===== */
%token <string> AT_ANNOT
%token <string> PCT_ANNOT
%token <string> COLON_ANNOT
%token <string> AMP_ANNOT
%token <string> HASH_ANNOT

%%

/* ===== Top level ===== */

script:
  | KW_PARAMETER ty SEMI KW_STORAGE ty SEMI KW_CODE iblock EOF
      { SContract { parameter = $2; storage = $5; code = $8 } }
  | instruction EOF
      { SExpr $1 }
;

/* ===== Instruction blocks and sequences ===== */

iblock:
  | LBRACE instr_list_opt RBRACE        { ESeq $2 }
;

instr_list_opt:
  | /* empty */                          { [] }                     /* allow empty block */
  | instruction                          { [ $1 ] }
  | instr_list_opt instruction           { $1 @ [ $2 ] }
  | instr_list_opt SEMI instruction      { $1 @ [ $3 ] }            /* semicolons optional */
  | instr_list_opt SEMI                  { $1 }                     /* trailing ';' */
;

/* ===== Instructions ===== */

instruction:
  /* block as an instruction */
  | iblock                               { $1 }

  /* DROP | DROP n */
  | KW_DROP_SI                           { EPrim { name="DROP"; annots=[]; args=[] } }
  | KW_DROP NAT                          { EPrim { name="DROP"; annots=[]; args=[EString $2] } }

  /* DUP | DUP n */
  | KW_DUP_SI                            { EPrim { name="DUP"; annots=[]; args=[] } }
  | KW_DUP NAT                           { EPrim { name="DUP"; annots=[]; args=[EString $2] } }

  | KW_SWAP                              { EPrim { name="SWAP"; annots=[]; args=[] } }
  | KW_DIG NAT                           { EPrim { name="DIG"; annots=[]; args=[EString $2] } }
  | KW_DUG NAT                           { EPrim { name="DUG"; annots=[]; args=[EString $2] } }

  /* PUSH <type> <data> */
  | KW_PUSH annots5 ty data              { EPrim { name="PUSH"; annots=$2; args=[ $3; $4 ] } }

  /* SOME | NONE <type> | UNIT | NEVER */
  | KW_SOME                              { EPrim { name="SOME"; annots=[]; args=[] } }
  | KW_NONE ty                           { EPrim { name="NONE"; annots=[]; args=[ $2 ] } }
  | KW_UNIT                              { EPrim { name="UNIT"; annots=[]; args=[] } }
  | KW_NEVER                             { EPrim { name="NEVER"; annots=[]; args=[] } }

  /* IF_NONE {..} {..} */
  | KW_IF_NONE annots5 iblock iblock     { EPrim { name="IF_NONE"; annots=$2; args=[ $3; $4 ] } }

  /* PAIR | PAIR n | CAR | CDR | UNPAIR | UNPAIR n */
  | KW_PAIR_SI                           { EPrim { name="PAIR"; annots=[]; args=[] } }
  | KW_PAIR NAT                          { EPrim { name="PAIR"; annots=[]; args=[ EString $2 ] } }
  | KW_CAR                               { EPrim { name="CAR"; annots=[]; args=[] } }
  | KW_CDR                               { EPrim { name="CDR"; annots=[]; args=[] } }
  | KW_UNPAIR                            { EPrim { name="UNPAIR"; annots=[]; args=[] } }
  | KW_UNPAIR NAT                        { EPrim { name="UNPAIR"; annots=[]; args=[ EString $2 ] } }

  /* LEFT/RIGHT <type> */
  | KW_LEFT  ty                          { EPrim { name="LEFT";  annots=[]; args=[ $2 ] } }
  | KW_RIGHT ty                          { EPrim { name="RIGHT"; annots=[]; args=[ $2 ] } }

  /* IF_LEFT/IF_RIGHT {..}{..} */
  | KW_IF_LEFT  annots5 iblock iblock    { EPrim { name="IF_LEFT";  annots=$2; args=[ $3; $4 ] } }
  | KW_IF_RIGHT annots5 iblock iblock    { EPrim { name="IF_RIGHT"; annots=$2; args=[ $3; $4 ] } }

  /* NIL <type> | CONS | IF_CONS {..}{..} */
  | KW_NIL ty                            { EPrim { name="NIL"; annots=[]; args=[ $2 ] } }
  | KW_CONS                              { EPrim { name="CONS"; annots=[]; args=[] } }
  | KW_IF_CONS annots5 iblock iblock     { EPrim { name="IF_CONS"; annots=$2; args=[ $3; $4 ] } }

  | KW_SIZE                              { EPrim { name="SIZE"; annots=[]; args=[] } }

  /* EMPTY_* */
  | KW_EMPTY_SET     cty                 { EPrim { name="EMPTY_SET"; annots=[]; args=[ $2 ] } }
  | KW_EMPTY_MAP     cty ty              { EPrim { name="EMPTY_MAP"; annots=[]; args=[ $2; $3 ] } }
  | KW_EMPTY_BIG_MAP cty ty              { EPrim { name="EMPTY_BIG_MAP"; annots=[]; args=[ $2; $3 ] } }

  /* MAP {..} | ITER {..} */
  | KW_MAP  iblock                       { EPrim { name="MAP";  annots=[]; args=[ $2 ] } }
  | KW_ITER iblock                       { EPrim { name="ITER"; annots=[]; args=[ $2 ] } }

  | KW_MEM                               { EPrim { name="MEM"; annots=[]; args=[] } }
  | KW_GET_SI                            { EPrim { name="GET"; annots=[]; args=[] } }
  | KW_GET NAT                           { EPrim { name="GET"; annots=[]; args=[ EString $2 ] } }
  | KW_UPDATE                            { EPrim { name="UPDATE"; annots=[]; args=[] } }
  | KW_UPDATE NAT                        { EPrim { name="UPDATE"; annots=[]; args=[ EString $2 ] } }

  /* IF {..}{..} canonical; plus one-armed macro to induce (DANGLING) */
  | KW_IF annots5 iblock iblock          { EPrim { name="IF"; annots=$2; args=[ $3; $4 ] } }
  | KW_IF annots5 iblock                 { EPrim { name="IF_THEN"; annots=$2; args=[ $3 ] } }  /* macro */

  | KW_LOOP      iblock                  { EPrim { name="LOOP"; annots=[]; args=[ $2 ] } }
  | KW_LOOP_LEFT iblock                  { EPrim { name="LOOP_LEFT"; annots=[]; args=[ $2 ] } }

  /* LAMBDA / LAMBDA_REC <type> <type> {..} */
  | KW_LAMBDA     ty ty iblock           { EPrim { name="LAMBDA";     annots=[]; args=[ $2; $3; $4 ] } }
  | KW_LAMBDA_REC ty ty iblock           { EPrim { name="LAMBDA_REC"; annots=[]; args=[ $2; $3; $4 ] } }

  | KW_EXEC                              { EPrim { name="EXEC"; annots=[]; args=[] } }
  | KW_APPLY                             { EPrim { name="APPLY"; annots=[]; args=[] } }

  /* DIP {..} | DIP n {..} */
  | KW_DIP annots5 iblock                { EPrim { name="DIP"; annots=$2; args=[ $3 ] } }
  | KW_DIP annots5 NAT iblock            { EPrim { name="DIP"; annots=$2; args=[ EString $3; $4 ] } }

  | KW_FAILWITH                          { EPrim { name="FAILWITH"; annots=[]; args=[] } }
  | KW_CAST                              { EPrim { name="CAST"; annots=[]; args=[] } }
  | KW_RENAME                            { EPrim { name="RENAME"; annots=[]; args=[] } }

  | KW_CONCAT                            { EPrim { name="CONCAT"; annots=[]; args=[] } }
  | KW_SLICE                             { EPrim { name="SLICE"; annots=[]; args=[] } }
  | KW_PACK                              { EPrim { name="PACK"; annots=[]; args=[] } }
  | KW_UNPACK ty                         { EPrim { name="UNPACK"; annots=[]; args=[ $2 ] } }

  /* Arithmetic / logic / compare family (prefix canonical) */
  | KW_ADD { EPrim { name="ADD"; annots=[]; args=[] } }
  | KW_SUB { EPrim { name="SUB"; annots=[]; args=[] } }
  | KW_MUL { EPrim { name="MUL"; annots=[]; args=[] } }
  | KW_EDIV { EPrim { name="EDIV"; annots=[]; args=[] } }
  | KW_ABS { EPrim { name="ABS"; annots=[]; args=[] } }
  | KW_ISNAT { EPrim { name="ISNAT"; annots=[]; args=[] } }
  | KW_INT { EPrim { name="INT"; annots=[]; args=[] } }
  | KW_NEG { EPrim { name="NEG"; annots=[]; args=[] } }
  | KW_LSL { EPrim { name="LSL"; annots=[]; args=[] } }
  | KW_LSR { EPrim { name="LSR"; annots=[]; args=[] } }
  | KW_OR  { EPrim { name="OR"; annots=[]; args=[] } }
  | KW_AND { EPrim { name="AND"; annots=[]; args=[] } }
  | KW_XOR { EPrim { name="XOR"; annots=[]; args=[] } }
  | KW_NOT { EPrim { name="NOT"; annots=[]; args=[] } }
  | KW_COMPARE { EPrim { name="COMPARE"; annots=[]; args=[] } }
  | KW_EQ { EPrim { name="EQ"; annots=[]; args=[] } }
  | KW_NEQ { EPrim { name="NEQ"; annots=[]; args=[] } }
  | KW_LT { EPrim { name="LT"; annots=[]; args=[] } }
  | KW_GT { EPrim { name="GT"; annots=[]; args=[] } }
  | KW_LE { EPrim { name="LE"; annots=[]; args=[] } }
  | KW_GE { EPrim { name="GE"; annots=[]; args=[] } }

  /* Contract & crypto (subset) */
  | KW_SELF            { EPrim { name="SELF"; annots=[]; args=[] } }
  | KW_SELF_ADDRESS    { EPrim { name="SELF_ADDRESS"; annots=[]; args=[] } }
  | KW_CONTRACT ty     { EPrim { name="CONTRACT"; annots=[]; args=[ $2 ] } }
  | KW_TRANSFER_TOKENS { EPrim { name="TRANSFER_TOKENS"; annots=[]; args=[] } }
  | KW_SET_DELEGATE    { EPrim { name="SET_DELEGATE"; annots=[]; args=[] } }

  /* -------- Infix macro forms (OP-STYLE; no precedence on purpose) -------- */
  | instruction PLUS  instruction        { EPrim { name="ADD"; annots=[]; args=[ $1; $3 ] } }
  | instruction MINUS instruction        { EPrim { name="SUB"; annots=[]; args=[ $1; $3 ] } }
  | instruction STAR  instruction        { EPrim { name="MUL"; annots=[]; args=[ $1; $3 ] } }
  | instruction SLASH instruction        { EPrim { name="EDIV"; annots=[]; args=[ $1; $3 ] } }
  | instruction CARET instruction        { EPrim { name="POW";  annots=[]; args=[ $1; $3 ] } }
  | instruction DAMP  instruction        { EPrim { name="AND"; annots=[]; args=[ $1; $3 ] } }
  | instruction DPIPE instruction        { EPrim { name="OR";  annots=[]; args=[ $1; $3 ] } }
  | instruction RLT  instruction         { EPrim { name="LT";  annots=[]; args=[ $1; $3 ] } }
  | instruction RLE  instruction         { EPrim { name="LE";  annots=[]; args=[ $1; $3 ] } }
  | instruction RGT  instruction         { EPrim { name="GT";  annots=[]; args=[ $1; $3 ] } }
  | instruction RGE  instruction         { EPrim { name="GE";  annots=[]; args=[ $1; $3 ] } }
  | instruction REQ  instruction         { EPrim { name="EQ";  annots=[]; args=[ $1; $3 ] } }
  | instruction RNEQ instruction         { EPrim { name="NEQ"; annots=[]; args=[ $1; $3 ] } }
  | instruction CONS instruction         { EPrim { name="CONS"; annots=[]; args=[ $1; $3 ] } }
  | LPAREN instruction RPAREN            { $2 }
;

/* ===== Data (per spec, *without* “| instruction”) ===== */

data:
  | INT                                  { EInt $1 }
  | STRING                               { EString $1 }
  | BYTES                                { EBytes $1 }
  | KD_Unit                              { EPrim { name="Unit"; annots=[]; args=[] } }
  | KD_True                              { EPrim { name="True"; annots=[]; args=[] } }
  | KD_False                             { EPrim { name="False"; annots=[]; args=[] } }
  | KD_Pair data data more_data          { EPrim { name="Pair"; annots=[]; args=$2 :: $3 :: $4 } }
  | KD_Left data                         { EPrim { name="Left"; annots=[]; args=[ $2 ] } }
  | KD_Right data                        { EPrim { name="Right"; annots=[]; args=[ $2 ] } }
  | KD_Some data                         { EPrim { name="Some"; annots=[]; args=[ $2 ] } }
  | KD_None                              { EPrim { name="None"; annots=[]; args=[] } }
  | KD_Lambda_rec instruction            { EPrim { name="Lambda_rec"; annots=[]; args=[ $2 ] } }
  | LBRACE data_elems_opt RBRACE         { ESeq $2 }
;

more_data:
  | /* empty */                           { [] }
  | data more_data                        { $1 :: $2 }
;

data_elems_opt:
  | /* empty */                           { [] }
  | data                                  { [ $1 ] }
  | data_elems_opt SEMI data              { $1 @ [ $3 ] }
  | data_elems_opt SEMI                   { $1 }
;

/* ===== Types (per spec; RR-free) ===== */

ty:
  /* inline the atomic comparable types so ty no longer depends on cty */
  | CTYPE_unit                            { EPrim { name="unit"; annots=[]; args=[] } }
  | CTYPE_never                           { EPrim { name="never"; annots=[]; args=[] } }
  | CTYPE_bool                            { EPrim { name="bool"; annots=[]; args=[] } }
  | CTYPE_int                             { EPrim { name="int"; annots=[]; args=[] } }
  | CTYPE_nat                             { EPrim { name="nat"; annots=[]; args=[] } }
  | CTYPE_string                          { EPrim { name="string"; annots=[]; args=[] } }
  | CTYPE_chain_id                        { EPrim { name="chain_id"; annots=[]; args=[] } }
  | CTYPE_bytes                           { EPrim { name="bytes"; annots=[]; args=[] } }
  | CTYPE_mutez                           { EPrim { name="mutez"; annots=[]; args=[] } }
  | CTYPE_key_hash                        { EPrim { name="key_hash"; annots=[]; args=[] } }
  | CTYPE_key                             { EPrim { name="key"; annots=[]; args=[] } }
  | CTYPE_signature                       { EPrim { name="signature"; annots=[]; args=[] } }
  | CTYPE_timestamp                       { EPrim { name="timestamp"; annots=[]; args=[] } }
  | CTYPE_address                         { EPrim { name="address"; annots=[]; args=[] } }

  /* composite types */
  | TYPE_option ty                        { EPrim { name="option"; annots=[]; args=[ $2 ] } }
  | TYPE_list ty                          { EPrim { name="list";   annots=[]; args=[ $2 ] } }
  | TYPE_set  cty                         { EPrim { name="set";    annots=[]; args=[ $2 ] } }
  | TYPE_operation                        { EPrim { name="operation"; annots=[]; args=[] } }
  | TYPE_contract ty                      { EPrim { name="contract";  annots=[]; args=[ $2 ] } }
  | TYPE_ticket   cty                     { EPrim { name="ticket";    annots=[]; args=[ $2 ] } }
  | TYPE_pair ty ty more_types            { EPrim { name="pair";      annots=[]; args=$2 :: $3 :: $4 } }
  | TYPE_or ty ty                         { EPrim { name="or";        annots=[]; args=[ $2; $3 ] } }
  | TYPE_lambda ty ty                     { EPrim { name="lambda";    annots=[]; args=[ $2; $3 ] } }
  | TYPE_map     cty ty                   { EPrim { name="map";       annots=[]; args=[ $2; $3 ] } }
  | TYPE_big_map cty ty                   { EPrim { name="big_map";   annots=[]; args=[ $2; $3 ] } }
  | TYPE_bls12_381_g1                     { EPrim { name="bls12_381_g1"; annots=[]; args=[] } }
  | TYPE_bls12_381_g2                     { EPrim { name="bls12_381_g2"; annots=[]; args=[] } }
  | TYPE_bls12_381_fr                     { EPrim { name="bls12_381_fr"; annots=[]; args=[] } }
  | TYPE_sapling_transaction NAT          { EPrim { name="sapling_transaction"; annots=[]; args=[ EString $2 ] } }
  | TYPE_sapling_state        NAT         { EPrim { name="sapling_state";        annots=[]; args=[ EString $2 ] } }
  | TYPE_chest                             { EPrim { name="chest";     annots=[]; args=[] } }
  | TYPE_chest_key                         { EPrim { name="chest_key"; annots=[]; args=[] } }
;

more_types:
  | /* empty */                           { [] }
  | ty more_types                         { $1 :: $2 }
;

/* Comparable types (kept only for contexts that require them) */
cty:
  | CTYPE_unit                            { EPrim { name="unit"; annots=[]; args=[] } }
  | CTYPE_never                           { EPrim { name="never"; annots=[]; args=[] } }
  | CTYPE_bool                            { EPrim { name="bool"; annots=[]; args=[] } }
  | CTYPE_int                             { EPrim { name="int";  annots=[]; args=[] } }
  | CTYPE_nat                             { EPrim { name="nat";  annots=[]; args=[] } }
  | CTYPE_string                          { EPrim { name="string"; annots=[]; args=[] } }
  | CTYPE_chain_id                        { EPrim { name="chain_id"; annots=[]; args=[] } }
  | CTYPE_bytes                           { EPrim { name="bytes"; annots=[]; args=[] } }
  | CTYPE_mutez                           { EPrim { name="mutez"; annots=[]; args=[] } }
  | CTYPE_key_hash                        { EPrim { name="key_hash"; annots=[]; args=[] } }
  | CTYPE_key                             { EPrim { name="key"; annots=[]; args=[] } }
  | CTYPE_signature                       { EPrim { name="signature"; annots=[]; args=[] } }
  | CTYPE_timestamp                       { EPrim { name="timestamp"; annots=[]; args=[] } }
  | CTYPE_address                         { EPrim { name="address"; annots=[]; args=[] } }
  | TYPE_option cty                       { EPrim { name="option"; annots=[]; args=[ $2 ] } }
  | TYPE_or cty cty                       { EPrim { name="or";     annots=[]; args=[ $2; $3 ] } }
  | TYPE_pair cty cty more_ctys           { EPrim { name="pair";   annots=[]; args= $2 :: $3 :: $4 } }
;

more_ctys:
  | /* empty */                           { [] }
  | cty more_ctys                         { $1 :: $2 }
;

/* ===== Annotations: greedy Kleene (now productive) ===== */

annots5:
  | /* empty */                           { [] }
  | annots5 AT_ANNOT                      { $1 @ [ $2 ] }
  | annots5 PCT_ANNOT                     { $1 @ [ $2 ] }
  | annots5 COLON_ANNOT                   { $1 @ [ $2 ] }
  | annots5 AMP_ANNOT                     { $1 @ [ $2 ] }
  | annots5 HASH_ANNOT                    { $1 @ [ $2 ] }
;
