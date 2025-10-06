type nonterminal = {
  name: string;
  binding: string option;
}

type item = 
  | NT of nonterminal
  | T of string

type production = {
  lhs: string;
  rhs: item list;
  action: string;
}

type parsed_mly = {
  preamble: string;
  productions: production list;
}

(* Helper to trim whitespace *)
let trim = String.trim

(* Check if a line contains only the separator %% *)
let separator = "%%"
let is_separator line =
  trim line = separator

(* Extract the preamble (everything before %%) *)
let extract_preamble lines =
  let rec aux acc = function
    | [] -> (String.concat "\n" (List.rev acc), [])
    | line :: rest ->
        if is_separator line then
          (String.concat "\n" (List.rev acc), rest)
        else
          aux (line :: acc) rest
  in
  aux [] lines

(* Remove comments from a string *)
let remove_comments s =
  let len = String.length s in
  let rec aux i in_comment result =
    if i >= len then
      String.concat "" (List.rev result)
    else if in_comment then
      (* Look for end of comment *)
      if i + 1 < len && s.[i] = '*' && s.[i+1] = '/' then
        aux (i + 2) false result
      else
        aux (i + 1) true result
    else
      (* Look for start of comment *)
      if i + 1 < len && s.[i] = '/' && s.[i+1] = '*' then
        aux (i + 2) true result
      else
        aux (i + 1) false (String.make 1 s.[i] :: result)
  in
  aux 0 false []

(* Tokenize a string into words *)
let tokenize_rhs s =
  let rec aux acc current = function
    | [] -> 
        let final = trim (String.concat "" (List.rev current)) in
        if final = "" then List.rev acc else List.rev (final :: acc)
    | c :: rest ->
        match c with
        | ' ' | '\t' | '\n' ->
            let token = trim (String.concat "" (List.rev current)) in
            if token = "" then aux acc [] rest
            else aux (token :: acc) [] rest
        | _ ->
            aux acc (String.make 1 c :: current) rest
  in
  aux [] [] (List.of_seq (String.to_seq s))

(* Extract the action from a production (content between { and }) *)
let extract_action s =
  let len = String.length s in
  let rec find_open i =
    if i >= len then None
    else match s.[i] with
      | '{' -> Some (i, find_close (i + 1) 1)
      | _ -> find_open (i + 1)
  and find_close i depth =
    if i >= len then len
    else match s.[i] with
      | '{' -> find_close (i + 1) (depth + 1)
      | '}' when depth = 1 -> i + 1
      | '}' -> find_close (i + 1) (depth - 1)
      | _ -> find_close (i + 1) depth
  in
  match find_open 0 with
  | Some (start, end_pos) -> 
      let action = String.sub s start (end_pos - start) in
      let rhs = String.sub s 0 start in
      (trim rhs, trim action)
  | None -> (trim s, "")

(* Check if a symbol is a terminal (all uppercase or special symbols) *)
let is_terminal sym =
  if sym = "" then false
  else
    (* Terminals are typically all uppercase or special symbols *)
    let c = sym.[0] in
    (c >= 'A' && c <= 'Z') || not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

(* Parse a token which may have a binding like "p=stmts" *)
let parse_token token =
  if String.contains token '=' then
    (* Has a binding *)
    match String.split_on_char '=' token with
    | binding :: sym :: _ -> 
        let binding = trim binding in
        let sym = trim sym in
        if is_terminal sym then
          T sym
        else
          NT { name = sym; binding = Some binding }
    | _ -> T token
  else
    (* No binding *)
    if is_terminal token then
      T token
    else
      NT { name = token; binding = None }

(* Parse a single production line *)
let parse_production lhs line =
  let line = trim line in
  (* Remove the leading | if present *)
  let line = if String.length line > 0 && line.[0] = '|' then
               trim (String.sub line 1 (String.length line - 1))
             else line in
  
  (* Remove comments first *)
  let line = remove_comments line in
  let line = trim line in
  
  (* If line is empty after removing comments, it's an epsilon production *)
  if line = "" || line = "{" then
    { lhs; rhs = []; action = "" }
  else
    let (rhs_str, action) = extract_action line in
    let rhs_str = trim rhs_str in
    
    (* If rhs_str is empty, it's an epsilon production *)
    if rhs_str = "" then
      { lhs; rhs = []; action }
    else
      let rhs_tokens = tokenize_rhs rhs_str in
      
      (* Parse each token into an item *)
      let rhs = List.filter_map (fun token ->
        if token = "" then None else Some (parse_token token)
      ) rhs_tokens in
      
      { lhs; rhs; action }

(* Parse all productions from the grammar section *)
let parse_productions lines =
  let rec collect_production_lines current_lhs lines =
    match lines with
    | [] -> ([], [])
    | line :: rest ->
        let trimmed = trim line in
        if trimmed = "" then
          collect_production_lines current_lhs rest
        else if String.starts_with ~prefix:"|" trimmed then
          let (more_lines, remaining) = collect_production_lines current_lhs rest in
          (line :: more_lines, remaining)
        else if String.contains trimmed ':' then
          (* New non-terminal, stop collecting *)
          ([], lines)
        else
          (* Might be a continuation without |, stop for safety *)
          ([], lines)
  in
  
  let rec aux acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let trimmed = trim line in
        (* Skip empty lines *)
        if trimmed = "" then
          aux acc rest
        (* Check if this is a new non-terminal definition *)
        else if String.contains trimmed ':' then
          let colon_idx = String.index trimmed ':' in
          let lhs = trim (String.sub trimmed 0 colon_idx) in
          let rhs_line = trim (String.sub trimmed (colon_idx + 1) 
                                (String.length trimmed - colon_idx - 1)) in
          
          (* Collect all production alternatives for this non-terminal *)
          let (alt_lines, remaining) = collect_production_lines lhs rest in
          let all_lines = if rhs_line = "" then alt_lines else (rhs_line :: alt_lines) in
          
          (* Parse each alternative as a separate production *)
          let prods = List.filter_map (fun l ->
            let l = trim l in
            if l = "" then None
            else Some (parse_production lhs l)
          ) all_lines in
          
          aux (List.rev_append prods acc) remaining
        else
          aux acc rest
  in
  aux [] lines

(* Main parsing function *)
let parse_mly_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let lines = read_lines [] in
  let (preamble, grammar_lines) = extract_preamble lines in
  let productions = parse_productions grammar_lines in
  { preamble; productions }

(* Pretty print the results *)
let print_item = function
  | T s -> Printf.sprintf "T(%s)" s
  | NT { name; binding = None } -> Printf.sprintf "NT(%s)" name
  | NT { name; binding = Some b } -> Printf.sprintf "NT(%s=%s)" b name

let print_results result =
  Printf.printf "=== PREAMBLE ===\n%s\n\n" result.preamble;
  Printf.printf "=== PRODUCTIONS ===\n";
  List.iter (fun prod ->
    if prod.rhs = [] then
      Printf.printf "%s -> ε\n" prod.lhs
    else
      Printf.printf "%s -> %s\n" prod.lhs 
        (String.concat " " (List.map print_item prod.rhs));
    if prod.action <> "" then
      Printf.printf "  Action: %s\n" prod.action;
    Printf.printf "\n"
  ) result.productions

let mly_of_ta (ta: Ta.ta) (mly: parsed_mly) (mly_production_of_symbol: Ta.symbol -> production): string = 
  let transitions = Hashtbl.to_seq ta.Ta.transitions in
  let state_transitions: (Ta.state, (Ta.symbol * Ta.beta list) list) Hashtbl.t = Hashtbl.create 10 in
  (* Group transitions by state *)
  Seq.iter (fun ((state, symbol), rhs) ->
    let existing = match Hashtbl.find_opt state_transitions state with
    | Some lst -> lst
    | None -> []
    in
    Hashtbl.replace state_transitions state ((symbol, rhs) :: existing)
  ) transitions;

  (* Build the mly string *)
  Hashtbl.fold (fun state transitions acc ->
    let state_str = Printf.sprintf "%s:\n" state in
    let trans_str = List.fold_left (fun t_acc (symbol, rhs) ->
      let mly_prod = mly_production_of_symbol symbol in
      let mly_rhs = mly_prod.rhs in
      let action = mly_prod.action in
      (assert (List.length rhs = List.length mly_rhs));
      let rhs_zip = List.combine rhs mly_rhs in
      let rhs_str = List.fold_left (fun s_acc (b, item) ->
        match b, item with
        | Ta.T t, T _ -> s_acc ^ t ^ " "
        | Ta.S s, NT { name = _; binding = None } -> s_acc ^ s ^ " "
        | Ta.S s, NT { name = _; binding = Some b } -> s_acc ^ (b ^ "=" ^ s) ^ " "
        | _, _ -> raise (Failure "mly_of_ta: Mismatched beta and item")
      ) "" rhs_zip in
      t_acc ^ Printf.sprintf "  | %s %s\n" rhs_str action
    ) "" transitions in
    acc ^ "\n" ^ state_str ^ trans_str ^ "  ;\n"
  ) state_transitions (mly.preamble ^ "\n" ^ separator ^ "\n")

