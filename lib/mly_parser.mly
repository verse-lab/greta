%{
  open Mly_types

  (* Helper to determine if a symbol is a terminal (uppercase or non-alphabetic) *)
  let is_terminal name =
    if String.length name = 0 then false
    else
      let c = name.[0] in
      (c >= 'A' && c <= 'Z') || not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

  let make_item name =
    let binding_rec = { name; binding = None } in
    if is_terminal name then T binding_rec else NT binding_rec

  let make_bound_item var_name sym_name =
    let binding_rec = { name = sym_name; binding = Some var_name } in
    if is_terminal sym_name then T binding_rec else NT binding_rec
%}

%token <string> IDENT
%token <string> OCAML_CODE
%token <string> ACTION
%token <string> TYPE_SPEC
%token PERCENT_PERCENT
%token PERCENT_TOKEN PERCENT_START PERCENT_TYPE
%token PERCENT_LEFT PERCENT_RIGHT PERCENT_NONASSOC
%token PERCENT_PREC
%token COLON PIPE SEMICOLON EQUALS
%token EOF

%start <Mly_types.parsed_mly> mly_file

%%

mly_file:
  | preamble PERCENT_PERCENT rules optional_postamble EOF
    { let (code_parts, annots) = $1 in
      { preamble = { code = String.concat "\n" (List.rev code_parts);
                     annotations = List.rev annots };
        productions = $3 } }

preamble:
  | (* empty *)
    { ([], []) }
  | preamble preamble_item
    { let (codes, annots) = $1 in
      match $2 with
      | `Code c -> (c :: codes, annots)
      | `Annot a -> (codes, a :: annots) }

preamble_item:
  | OCAML_CODE
    { `Code (Printf.sprintf "%%{\n%s\n%%}" $1) }
  | PERCENT_TOKEN type_spec_opt ident_list
    { let type_str = match $2 with Some t -> " <" ^ t ^ ">" | None -> "" in
      `Code (Printf.sprintf "%%token%s %s" type_str (String.concat " " $3)) }
  | PERCENT_START type_spec_opt IDENT
    { let prefix = match $2 with
        | Some t -> Printf.sprintf "%%start <%s>" t
        | None -> "%start"
      in
      `Annot { prefix; state = $3 } }
  | PERCENT_TYPE TYPE_SPEC IDENT
    { `Annot { prefix = Printf.sprintf "%%type <%s>" $2; state = $3 } }
  | PERCENT_LEFT ident_list
    { `Code (Printf.sprintf "%%left %s" (String.concat " " $2)) }
  | PERCENT_RIGHT ident_list
    { `Code (Printf.sprintf "%%right %s" (String.concat " " $2)) }
  | PERCENT_NONASSOC ident_list
    { `Code (Printf.sprintf "%%nonassoc %s" (String.concat " " $2)) }

type_spec_opt:
  | (* empty *)
    { None }
  | TYPE_SPEC
    { Some $1 }

ident_list:
  | IDENT
    { [$1] }
  | IDENT ident_list
    { $1 :: $2 }

rules:
  | rule_list
    { List.flatten $1 }

rule_list:
  | (* empty *)
    { [] }
  | rule rule_list
    { $1 :: $2 }

rule:
  | IDENT COLON production_list SEMICOLON
    { List.map (fun (rhs, action) -> { lhs = $1; rhs; action }) $3 }
  | IDENT COLON production_list
    { List.map (fun (rhs, action) -> { lhs = $1; rhs; action }) $3 }
  | IDENT COLON PIPE production_list SEMICOLON
    { List.map (fun (rhs, action) -> { lhs = $1; rhs; action }) $4 }
  | IDENT COLON PIPE production_list
    { List.map (fun (rhs, action) -> { lhs = $1; rhs; action }) $4 }

production_list:
  | production
    { [$1] }
  | production_list PIPE production
    { $1 @ [$3] }

production:
  | rhs_items optional_prec optional_action
    { ($1, $3) }

rhs_items:
  | (* empty *)
    { [] }
  | rhs_items rhs_item
    { $1 @ [$2] }

rhs_item:
  | IDENT
    { make_item $1 }
  | IDENT EQUALS IDENT
    { make_bound_item $1 $3 }

optional_prec:
  | (* empty *)
    { None }
  | PERCENT_PREC IDENT
    { Some $2 }

optional_action:
  | (* empty *)
    { "" }
  | ACTION
    { Printf.sprintf "{ %s }" $1 }

optional_postamble:
  | (* empty *)
    { () }
  | PERCENT_PERCENT postamble_content
    { () }

postamble_content:
  | (* empty *)
    { () }
  | postamble_content IDENT
    { () }
  | postamble_content OCAML_CODE
    { () }
  | postamble_content ACTION
    { () }
