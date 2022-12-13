type nonterminal = string
type terminal = string
type production = nonterminal * (terminal * string list)

type cfg = (* CFG := (V, \Sigma, S, P) *)
  { mutable nonterms : nonterminal list;  (* V - a set of nonterminals/variables, eg, E, +  *)
    mutable terms : terminal list;        (* \Sigma - a set of terminals, eg, N             *)
    mutable start : nonterminal;          (* S - start symbol, \in V, eg, E                 *)
    mutable prods : production list;      (* P - a set of productions, eg, E -> E + E       *)
  }

let parser_to_cfg (filename : string): cfg =
  let open Str in
  let open String in
  let open Printf in
  let cfg_res: cfg = { nonterms = []; terms = []; start = ""; prods = [] } in
  let wsopen: regexp = regexp ({|[ \n\r\t]*|} ^ "open") in
  let wsvertbar: regexp = regexp ({|[ \n\r\t]*|} ^ "|") in
  let wssemicol: regexp = regexp ({|[ \n\r\t]*|} ^ ";") in
  let argvar: regexp = regexp ("\\$" ^ {|[1-9]+|}) in
  let argvar_alt: regexp = regexp ("(\\$" ^ {|[1-9]+|}) in
  (** temp_storage *)
  let nonterms_temp: nonterminal list ref = ref [] in
  let terms_temp: terminal list ref = ref [] in
  let prods_temp: production list ref = ref [] in
  let prog_id: string ref = ref "nullID" in
  let prods_started = ref false in
  let debug_print = ref true in
  (** helpers **)
  let remove_colon (s: string): string = s |> split_on_char ':' |> List.hd in
  let read_line i = try Some (input_line i) with End_of_file -> None in
  let starts tk s = starts_with ~prefix:tk s in
  let ends tk s = ends_with ~suffix:tk s in
  let noteq a b = compare a b <> 0 in
  let true_exists, false_exists, b_inserted = ref false, ref false, ref false in
  let start_of_block s = not (starts "%%" s) 
      && not (string_match wsvertbar s 0) && not (string_match wssemicol s 0) && (ends ":" s) in
  let conds_to_exclude s = (s = "THEN") || (s = "Then") || (s = "ELSE") || (s = "Else") || (starts "Na" s) || (s = "Paren") in
  (** extract_terms :
    *   literals (%token <type> ...) to corresponding names, added to terms
    *   if \E %token TRUE && FALSE, then add B to terms, and other tokens to nonterms *)
  let extract_terms (st: string): unit =
    let ti, ts, tb = "%token <int>", "%token <string>", "%token <bool>" in
    let tt, tf = "%token TRUE", "%token FALSE" in
    let is_literal = (starts ti st) || (starts ts st) || (starts tb st) in
      if is_literal
      then let st_ls = split_on_char ' ' st in List.iter (fun x -> 
        if ((noteq x "%token") && (noteq x "<int>") && (noteq x "<string>") && (noteq x "<bool>"))
        then terms_temp := x :: !terms_temp) st_ls
        ; terms_temp := List.map (fun y -> if y = "INT" then "N" else if y = "BOOL" then (b_inserted := true; "B") else y) !terms_temp
      else 
        (if not !b_inserted && (starts tt st) then true_exists := true else 
         if not !b_inserted && (starts tf st) then false_exists := true);
         if not !b_inserted && !true_exists && !false_exists then (terms_temp := "B" :: !terms_temp; b_inserted := true);
        if (starts "%token" st) && not (starts "%token EOF" st) && not (starts tt st) && not (starts tf st) && not (st = "") && not is_literal
        then 
          let st_ls = split_on_char ' ' st in List.iter (fun x -> 
            if (noteq x "%token") then terms_temp := x :: !terms_temp) st_ls 
  (** extract_nonterms : 
    *   identify the start symbol based on the prog_id
    *   extract nonterms based on condition that it starts a block of transitions *)
  and extract_nonterms (st': string): unit =
    let ts' = "%start" in
    if starts ts' st' then split_on_char ' ' st' |> List.iter (fun x -> if noteq x ts' then prog_id := x)
    else if starts !prog_id st' then split_on_char ' ' st' |> List.iter (fun x -> if (noteq !prog_id x && noteq ":" x 
        && noteq "EOF" x && noteq "{" x && noteq "}" x && noteq "};" x && not (string_match argvar x 0)) 
        then (nonterms_temp := x :: !nonterms_temp; cfg_res.start <- x))
    else if start_of_block st' then split_on_char ' ' st' |> List.iter (fun x -> let nonterm_to_add = remove_colon x in 
        if not (List.mem nonterm_to_add !nonterms_temp) then nonterms_temp := nonterm_to_add :: !nonterms_temp)
  (** extract_prods :
    *   if it starts with nonterminals, read transitions until ";" *)
  in let rec extract_prods_from_block (nont_lhs: nonterminal) (blk_ls: string list): unit =
    let terms_ls = cfg_res.terms in match blk_ls with [] -> ()
    | blk_h :: blk_tl -> 
      let symbs_ls = blk_h |> split_on_char ' ' |> List.filter (fun x -> 
        noteq "|" x && noteq "{" x && noteq "}" x && noteq "};" x && noteq "Bool" x)
      in let rec collect_prod ls (term_rhs: terminal list) (symbs_rhs: string list): unit = 
          match ls with [] -> 
            (if List.length symbs_rhs = 0 then () 
            else if List.length term_rhs = 0 
            then (printf "Length of term_rhs is 0\n"; prods_temp := (nont_lhs, ("ε", List.rev symbs_rhs)) :: !prods_temp)
            else if List.length term_rhs = 1 
            then (printf "Length of term_rhs is 1\n";prods_temp := (nont_lhs, (List.hd term_rhs, List.rev symbs_rhs)) :: !prods_temp)
            else if List.length term_rhs = 2
            then let term_rhs_final = List.fold_left (fun a b -> a ^ b) "" (List.rev term_rhs)
              in (printf "Length of term_rhs is 2\n"; prods_temp := (nont_lhs, (term_rhs_final, List.rev symbs_rhs)) :: !prods_temp)
            else raise (Invalid_argument "RHS Terminal can have at most two symbols!")); 
            extract_prods_from_block nont_lhs blk_tl
          | h :: tl -> 
            if (List.mem h terms_ls && not (List.mem (uppercase_ascii h) term_rhs)) && not (conds_to_exclude h)
            then (printf "Collecting %s as term_rhs\n" h; collect_prod tl (h::term_rhs) symbs_rhs)
            else if (string_match argvar h 0) || (string_match argvar_alt h 0) || conds_to_exclude h || (List.mem (uppercase_ascii h) term_rhs)
            then collect_prod tl term_rhs symbs_rhs
            (* Below temporary fix for Int and Bool cases *)
            else if (h = "Int") then collect_prod tl term_rhs symbs_rhs else if (h = "INT") then collect_prod tl term_rhs ("N"::symbs_rhs)
            else if (h = "true" || h = "TRUE") then collect_prod tl term_rhs [] (* To skip this entire case so we have only 1 BOOL *)
            else if (h = "false") then collect_prod tl term_rhs symbs_rhs
            else if (h = "FALSE") then collect_prod tl term_rhs ("B"::symbs_rhs) else collect_prod tl term_rhs (h::symbs_rhs)
        in collect_prod symbs_ls [] []
  in
  let rec traverse inp acc : string list =
     match (read_line inp) with 
     | None -> cfg_res.terms <- (List.rev !terms_temp); List.rev acc
     | Some s -> 
      if not (starts "{%%" s) && not (starts "%}" s) && not (string_match wsopen s 0)
      then extract_terms s;
      (* only takes lines that are relevant for productions *)
      if (starts "%start" s) then prods_started := true;
      if (!prods_started) then traverse inp (s::acc) else traverse inp acc 
  in let relev_lines : string list = traverse (open_in filename) [] in 
  let _ = relev_lines |> List.iter (fun y -> extract_nonterms y); cfg_res.nonterms <- List.rev !nonterms_temp
  ;if (!debug_print) then (printf "Terminals: \n"; cfg_res.terms |> List.iter (printf " %s"); printf "\n";
                           printf "Non-terminals: \n" ;cfg_res.nonterms |> List.iter (printf " %s"); printf "\n\n")
  in let rec traverse_nxt (stls: string list) (acc_nont_lhs) (acc_block: string list): unit = 
      if (!debug_print) then printf "Calling traverse nxt now!\n";
      match stls with [] -> cfg_res.prods <- (List.rev !prods_temp)
      | shd :: stl -> 
        (if (starts !prog_id shd)
        then (printf "Starting with prog_id, so skip\n"; traverse_nxt stl acc_nont_lhs acc_block)
        else if (ends ";" shd || string_match wssemicol shd 0) 
        then (printf "Ending with ; so extract prods for \"%s\"\n" acc_nont_lhs; 
          extract_prods_from_block (remove_colon acc_nont_lhs) (List.rev acc_block); traverse_nxt stl "" [])
        else if List.mem (remove_colon shd) cfg_res.nonterms (* [prev] start_of_block shd *)
        then (printf "Start of the block, so loop with \"%s\" as nont_lhs\n" shd;traverse_nxt stl shd acc_block)
        else if (starts "%start" shd || starts "%%" shd) 
        then (printf "Either %%start or %%%%, so skip\n"; traverse_nxt stl acc_nont_lhs acc_block)
        else if string_match wsvertbar shd 0 
        then (printf "Vertical bar line so accumulate %s\n" shd; traverse_nxt stl acc_nont_lhs (shd :: acc_block))
        else (printf "Empty line so skip\n";traverse_nxt stl acc_nont_lhs acc_block))
  in let _ = if (!debug_print) then printf "Rel lines before running traverse_nxt\n"; 
    relev_lines |> List.iter (fun x -> printf "%s\n" x); traverse_nxt relev_lines "" [] 
  in cfg_res

let mly_to_cfg (filename: string): cfg = 
  let open List in 
  let open Printf in
  let cfg_original = parser_to_cfg filename in
  printf "\nOriginal CFG obtained from parser.mly : \n";
  printf "\tTerminals : { "; cfg_original.terms |> iter (printf "%s "); printf "}\n";
  printf "\tStart symbol : { "; cfg_original.start |> (printf "%s "); printf "}\n";
  printf "\tNonterminals : { "; cfg_original.nonterms |> iter (printf "%s "); printf "}\n";
  printf "\tSet of productions : { \n"; cfg_original.prods |> iter (fun x -> printf "\t\t\t\t%s -> ( %s " 
  (fst x) (fst (snd x)); (snd (snd x))|> iter (printf "%s "); printf ")\n"); printf "\t\t\t     }\n";
  cfg_original

(* let cfg_to_mly () =
  Printf.printf "%s\n" "in progress.." *)


