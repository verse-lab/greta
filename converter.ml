(* This file is to convert parser.mly to CFG and vice versa *)

type nonterm = string
type term = string
type prod = string * string * string list

type cfg = (* CFG := (V, \Sigma, S, P) *)
  { nonterms : nonterm list ref;  (* V - set of nonterminals/variables, eg, E, +  *)
    terms : term list ref;        (* \Sigma - set of terminals, eg, N             *)
    start : nonterm ref;          (* S - start symbol, \in V, eg, E               *)
    prods : prod list ref;        (* P - set of productions, eg, E -> E + E       *)
  }

let nonterms_temp : nonterm list ref = ref [] 
let terms_temp : term list ref = ref []
let start_temp : nonterm ref = ref "" 
let prods_temp : prod list ref = ref [] 
let relev_lines : string list ref = ref [] (* temp storage to extract prods *)

(* Set original cfg and cfg' as global variables *)
let cfg_origin : cfg = { nonterms = ref []; terms = ref []; start = ref ""; prods = ref [] }
let cfg_new : cfg = { nonterms = ref []; terms = ref []; start = ref ""; prods = ref [] }

let update_cfg (filename : string): unit =
  let open String in
  let flag = ref false in
  let t_res = ref [] in 
  let nt_res = ref [] in
  let read_line i = 
    try Some (input_line i) with End_of_file -> None in
  (** extract_terms_nonterms - literal data values (%token <type> ...) to terms, other %token to nonterms *)
  let extract_terms_nonterms (st: string): unit =
    if (starts_with ~prefix:"%token <int>" st) || (starts_with ~prefix:"%token <string>" st)
    then let st_ls = split_on_char ' ' st in List.iter (fun x -> 
        if (compare x "%token" <> 0) && (compare x "<int>" <> 0) && (compare x "<string>" <> 0)
        then t_res := x :: !t_res) st_ls; 
        t_res := List.map (fun y -> if y = "INT" then "N" else y) !t_res
    else if (starts_with ~prefix:"%token " st) && not (starts_with ~prefix:"%token EOF" st)
    then let st_ls = split_on_char ' ' st in List.iter (fun x ->
        if (compare x "%token" <> 0) then nt_res := x :: !nt_res) st_ls;
  (** 1. Traverse the lines and extract terms and nonterms *)
in let rec traverse inp acc : string list =
    match (read_line inp) with
    | None -> 
      relev_lines := List.rev !relev_lines; !relev_lines
    | Some s ->
      extract_terms_nonterms s; terms_temp := !terms_temp; nonterms_temp := !nonterms_temp;
      (* only takes lines that are relevant for productions *)
      if (starts_with ~prefix:"program" s) 
        then (start_temp := !start_temp; flag := true);
      if (!flag) then (relev_lines := (s::!relev_lines); traverse inp (s::acc)) 
      else traverse inp acc 
  in let prod_lines = traverse (open_in filename) [] 
  (** 2. Traverse the prod_lines and extract prod rules and start symbol *)
in List.iter () prod_lines
(** 3. Update the cfg_init based on existing parser.mly *)


(* prev version *)
let lines_from_file (filename : string): string list =
  let open String in
  let flag = ref false in
  let read_line i = 
    try Some (input_line i) with End_of_file -> None in
  let is_terminal_token st =
    (starts_with ~prefix:"%token <int>" st) || (starts_with ~prefix:"%token <string>" st) in
  let extract_term_token st =
    let res = ref "" in
    let st_ls = split_on_char ' ' st
    in List.iter (fun x -> 
      if (compare x "%token" <> 0) && (compare x "<int>" <> 0) && (compare x "<string>" <> 0) 
      then res := x) st_ls; if !res = "INT" then res := "N"; !res in
  let rec loop inp acc =
    match (read_line inp) with
    | None -> 
      (relev_lines := List.rev !relev_lines; List.rev acc)
    | Some s ->
      (* if literal data values (%token <type> ...), then add to terms *)
      if is_terminal_token s then (let to_add = extract_term_token s in terms_temp := (to_add::!terms_temp)); 
      (* only takes lines that are relevant for productions *)
      if (starts_with ~prefix:"program" s) 
        then (start_temp := !start_temp; flag := true);
      if (!flag) then (relev_lines := (s::!relev_lines); loop inp (s::acc)) 
      else loop inp acc in 
    loop (open_in filename) []

(* convert_to_cfg : take mly as input and convert to cfg *)
let mly_to_cfg () = 
  let open List in 
  let open Printf in
  lines_from_file "parser.mly" |> iter (printf "%s\n"); printf "\n";
  printf "%s" "Terminals : ";
  !terms_temp |> iter (printf "%s "); printf "\n\n";
  printf "%s" "Start symbol : ";
  !start_temp |> (printf "%s "); printf "\n\n";
  printf "%s\n" "Set of productions (in progress) : ";
  !relev_lines |> iter (printf "\t%s\n"); printf "\n"


(* convert_to_mly : take cfg as input and convert to mly *)
let cfg_to_mly () =
  Printf.printf "%s\n" "in progress.."



