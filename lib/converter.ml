open Cfg
open Ta
open Utils
open Treeutils

exception State_with_no_matching_order

(* ******************** Part I. Conversion of parser.mly > CFG > TA ******************** *)

let parser_to_cfg (debug_print: bool) (filename : string): cfg =
  let open Str in
  let open String in
  let open Printf in
  printf "\n\nParse the file %s to its corresponding CFG\n" filename;
  let cfg_res: cfg = { nonterms = []; terms = []; start = ""; productions = [] } in
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
  (** helpers **)
  let remove_colon (s: string): string = s |> split_on_char ':' |> List.hd in
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
        noteq "" x && noteq "|" x && noteq "{" x && noteq "}" x && noteq "};" x && noteq "Bool" x)
      in let rec collect_prod ls (term_rhs: terminal list) (symbs_rhs: string list): unit = 
          match ls with [] -> 
            (if List.length symbs_rhs = 0 then ()
            else if List.length term_rhs = 0
            then (if (debug_print) then (printf "\tLength of term_rhs is 0.\n\n");
                  prods_temp := (nont_lhs, ("ε", List.rev symbs_rhs)) :: !prods_temp)
            else if List.length term_rhs = 1
            then (if (debug_print) then (printf "\tLength of term_rhs %s is 1 and attaching symbs_rhs " (List.hd term_rhs);
                 (List.rev symbs_rhs) |> List.iteri (fun i x -> if i = 0 then printf " %s" x else printf ", %s" x); printf "\n\n");
                 prods_temp := (nont_lhs, (List.hd term_rhs, List.rev symbs_rhs)) :: !prods_temp)
            else if List.length term_rhs = 2
            then let term_rhs_final = List.fold_left (fun a b -> a ^ b) "" (List.rev term_rhs)
              in (if (debug_print) then (printf "\tLength of term_rhs %s is 2 and attaching symbs_rhs " (List.hd term_rhs);
              (List.rev symbs_rhs) |> List.iteri (fun i x -> if i = 0 then printf "%s" x else printf ", %s" x); printf "\n\n");
              prods_temp := (nont_lhs, (term_rhs_final, List.rev symbs_rhs)) :: !prods_temp)
            else raise (Invalid_argument "RHS Terminal can have at most two symbols!"));
            extract_prods_from_block nont_lhs blk_tl
          | h :: tl ->
            if (debug_print) then printf "\n\tNow processing %s\n" h;
            if (List.mem h terms_ls && not (List.mem (uppercase_ascii h) term_rhs)) && not (conds_to_exclude h) 
            then (if debug_print then printf "\tCollecting %s as term_rhs\n" h; collect_prod tl (h::term_rhs) symbs_rhs)
            else if (string_match argvar h 0) || (string_match argvar_alt h 0) || conds_to_exclude h || (List.mem (uppercase_ascii h) term_rhs)
            then (if debug_print then printf "\t%s is argvar or cond to exclude, so skip..\n" h; collect_prod tl term_rhs symbs_rhs)
            (* Below temporary fix for Int and Bool cases *)
            else if (h = "Int") 
            then (if debug_print then printf "\t%s is Int, so skip..\n" h; collect_prod tl term_rhs symbs_rhs) 
            else if (h = "INT") 
            then (if debug_print then printf "\t%s is INT, so add \"N\" to symbs_rhs.\n" h; collect_prod tl term_rhs ("N"::symbs_rhs))
            else if (h = "true" || h = "TRUE") 
            then (if debug_print then printf "\t%s is true || TRUE, so start collect_prod over with [] symbs_rhs..\n" h; collect_prod tl term_rhs []) (* To skip this entire case so we have only 1 BOOL *)
            else if (h = "false") 
            then (if debug_print then printf "\t%s is false so skip..\n" h; collect_prod tl term_rhs symbs_rhs)
            else if (h = "FALSE") 
            then (if debug_print then printf "\t%s is FALSE so add \"B\" to symbs_rhs\n" h; collect_prod tl term_rhs ("B"::symbs_rhs)) 
            else (if debug_print then printf "\tAll other situations don't suffice for %s, so add %s to symbs_rhs.\n" h h; collect_prod tl term_rhs (h::symbs_rhs))
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
  ;if (debug_print) then (printf "\n\tTerminals: {"; cfg_res.terms |> List.iter (printf " %s"); printf " }\n";
                           printf "\n\tNon-terminals: {" ;cfg_res.nonterms |> List.iter (printf " %s"); printf " }\n\n")
  in let rec traverse_nxt (stls: string list) (acc_nont_lhs) (acc_block: string list): unit = 
      if (debug_print) then printf "\n\tCalling traverse_nxt now\n";
      match stls with [] -> printf "\tEnd of traverse_nxt\n"; cfg_res.productions <- (List.rev !prods_temp)
      | shd :: stl ->
        (if (starts !prog_id shd)
        then (if debug_print then printf "\tStarting with prog_id, so skip\n"; traverse_nxt stl acc_nont_lhs acc_block)
        else if (ends ";" shd || string_match wssemicol shd 0)
        then (if debug_print then printf "\tEnding with ; so extract prods for \"%s\"\n" acc_nont_lhs;
          extract_prods_from_block (remove_colon acc_nont_lhs) (List.rev acc_block); traverse_nxt stl "" [])
        else if List.mem (remove_colon shd) cfg_res.nonterms (* [prev] start_of_block shd *)
        then (if debug_print then printf "\tStart of the block, so loop with \"%s\" as nont_lhs\n" shd; traverse_nxt stl shd acc_block)
        else if (starts "%start" shd || starts "%%" shd)
        then (if debug_print then printf "\tEither %%start or %%%%, so skip\n"; traverse_nxt stl acc_nont_lhs acc_block)
        else if string_match wsvertbar shd 0
        then (if debug_print then printf "\tVertical bar line so accumulate %s\n" shd; traverse_nxt stl acc_nont_lhs (shd :: acc_block))
        else (if debug_print then printf "\tEmpty line so skip\n"; traverse_nxt stl acc_nont_lhs acc_block))
  in let _ = if (debug_print) then (printf "  >> Rel lines before running traverse_nxt\n\n";
    relev_lines |> List.iter (fun x -> printf "%s\n" x)); traverse_nxt relev_lines "" []
  in Printf.printf "\n\nCFG defined in %s : \n" filename; Pp.pp_cfg (cfg_res);
  cfg_res

(** enhance_appearance : helper to enhance the symbol representation *)
let enhance_appearance (a: ta): ta =
  let change_symbol s = 
    let s', s'' = fst s, snd s in 
    (* if (s' = "MUL") then "*", s'' else if (s' = "PLUS") then "+", s'' else  *)
    if (s' = "LPARENRPAREN") then "()", s'' else s in
  let alph_updated: symbol list = a.alphabet |> List.map (fun sym -> change_symbol sym) in
  let trans_updated: transition list = a.transitions |> List.map (fun (st, (sym, st_ls)) ->
    let sym_new = change_symbol sym in (st, (sym_new, st_ls))) in
  { states = a.states; alphabet = alph_updated 
  ; start_state = a.start_state ; transitions = trans_updated }

let cfg_to_ta (versatileTerminals: (terminal * int list) list) (debug_print: bool) (g: cfg): 
  ta * restriction list =
  let open List in
  let open Printf in
  (* helper assuming at most 2 occurrences of versatileTerminals *)
  let last_ind ls elem =
    let rec loop i i_acc l = match l with [] -> i_acc
    | h::tl -> loop (i+1) (if fst h = elem then i else i_acc) tl
    in loop 0 (-1) ls in
  let epsilon_symb: symbol = ("ε", 1) in
  let prods_rhs = g.productions |> map snd in
  (* helper to compute rank of given symbol *)
  let rank_of_symb (debug: bool) (s: string): int =
    if (s = "N" || s = "B") then (if debug then printf "\tSymbol N or S, so length 0.\n"; 0) else
    match assoc_opt s prods_rhs with
    | None -> raise (Failure "Infeasible: nonexisting symbol")
    | Some symb_ls -> (if debug then (printf "\tSymbol %s has" s; symb_ls |> iter (printf " %s"); 
      printf " so length is "; printf "%d.\n" (length symb_ls)); length symb_ls) in
  let ranked_alphabet: symbol list = 
    let rparen_exists = mem "RPAREN" g.terms in g.terms 
    |> filter (fun x -> not (x = "THEN") && not (x = "ELSE") && not (x = "RPAREN"))
    |> map (fun x -> if (x = "LPAREN" && rparen_exists) then "LPARENRPAREN" else x)
    |> map (fun x -> (x, rank_of_symb debug_print x)) 
    (* add "ε" symbol to the alphabet *)
    |> append [epsilon_symb] in
  (* helper to get restrictions from transitions *)
  let trans_to_restrictions (trans_ls: transition list) (init_st: state): restriction list = 
    let rec get_order_loop (ls: transition list) (curr_st: state) (acc: state list): state list = 
      match ls with [] -> List.rev acc (* reverse is necessary for higher # ~ deeper *)
      | (lhs_st, (s, rhs_sts)) :: tl -> 
        if (lhs_st = curr_st) && (syms_equals s epsilon_symb)
        then (let rhs_st = List.hd rhs_sts 
              in get_order_loop tl rhs_st (rhs_st::acc))
        else get_order_loop tl curr_st acc
    in let states_ordered = get_order_loop trans_ls init_st [init_st] 
    in let states_orders = states_ordered |> List.mapi (fun i st -> (st, i)) 
    in (if debug_print then (printf "\nOrder of states : \n\t"; 
      states_orders |> List.iter (fun (st, lvl) -> printf "(%s, %i) " st lvl); printf "\n\n")); 
    let rec get_o_base_precedence (tls: transition list) (acc_res: restriction list): restriction list =
      match tls with [] -> List.rev acc_res
      | (lhs_st, (sym, _)) :: tl -> 
        if (lhs_st = "cond_expr") || (syms_equals sym epsilon_symb) 
            || (syms_equals sym ("LPARENRPAREN", 1)) || (syms_equals sym ("N", 0))
        then get_o_base_precedence tl acc_res
        else (let ord = match (assoc_opt lhs_st states_orders) with 
                        | None -> raise State_with_no_matching_order
                        | Some o -> o 
              in get_o_base_precedence tl (Prec (sym, ord)::acc_res))
    in get_o_base_precedence trans_ls []
  in 
  let (trans, restrictions) : transition list * restriction list =
    let stat: state ref = ref "" in
    let trans_ls = 
      g.productions |> fold_left (fun acc (n, (t, ls)) ->  (n, ((t, (length ls)), ls)) :: acc) []
      |> map (fun (s, (op, s_ls)) -> 
        (* treat int (N) and bool (B) differently *)
        if (fst op = "ε" && ((hd s_ls = "N") || (hd s_ls = "B"))) then (s, ((hd s_ls, 0), "ϵ"::[])) 
        (* TODO (below stat): make this less computationally expensive *)
        else if (fst op = "ε") then (stat := s; (s, (op, s_ls))) else (stat := s; (s, (op, s_ls))))
        |> List.rev in 
    let restrictions_ls = trans_to_restrictions trans_ls g.start in 
    trans_ls, restrictions_ls
  in
  (* add versatile symbols -- with multiple ranks -- to alphabet *)
  let toadd_versterms (debug: bool): symbol list = versatileTerminals |> map fst |> map (fun term ->
    let lasti = last_ind prods_rhs term in
    let rank_of_lasti: int = match nth_opt prods_rhs lasti with
      | None -> raise (Failure "Infeasible")
      | Some (t, symb_ls) -> (if debug then (printf "\tSymbol %s has" t; symb_ls |> iter (printf " %s"); 
        printf " so length is "; printf "%d.\n" (length symb_ls)); length symb_ls) 
    in (term, rank_of_lasti)) in
  let ta_res = 
    { states = "ϵ"::g.nonterms; alphabet = ranked_alphabet @ (toadd_versterms debug_print)
    ; start_state = g.start; transitions = trans } |> enhance_appearance in
  printf "\nTA obtained from the original CFG : \n"; Pp.pp_ta ta_res; 
  printf "\nRestrictions O_bp obtained from the TA_g : "; Pp.pp_restriction_lst restrictions;
  ta_res, restrictions

let convertToTa (file: string) (versatiles: (terminal * int list) list) (debug_print: bool): 
  ta * restriction list = 
  (* Pass in terminals which can have multiple arities, eg, "IF" *)
  file |> parser_to_cfg debug_print |> cfg_to_ta versatiles debug_print



(* ******************** Part II. Conversion of TA > CFG > parser.mly ******************** *)

(** undo_enhancement : helper to undo enhancement done on symbols *)
let undo_enhancement (a: ta): ta =
  let undo_change_symbol s =
    let s', s'' = fst s, snd s in if (s' = "*") then "MUL", s'' else
    if (s' = "+") then "PLUS", s'' else if (s' = "()") then "LPARENRPAREN", s'' else s in
  let alph_updated = a.alphabet |> List.map (fun sym -> undo_change_symbol sym) in
  let trans_updated = a.transitions |> List.map (fun (st, (sym, st_ls)) ->
    let sym_new = undo_change_symbol sym in (st, (sym_new, st_ls))) in 
  { states = a.states ; alphabet = alph_updated
  ; start_state = a.start_state ; transitions = trans_updated }

let ta_to_cfg (versatileTerminals: (terminal * int list) list) (debug_print: bool) (a: ta): cfg = 
  let open Printf in
  let a' = undo_enhancement a in
  printf "\nConvert TA to its corresponding CFG:\n\n  Input TA:\n"; Pp.pp_ta a';
  if debug_print then (printf "\n  >> Versatile sybol list: [ ";
  versatileTerminals |> List.map fst |> List.iter (fun x -> printf "%s " x); printf "]\n");
  (** helpers *)
  let remove_dups ls =
    let unique_cons elem ls = if (List.mem elem ls) then ls else elem :: ls in
    List.fold_right unique_cons ls [] in
  let nonterms_excl_eps: nonterminal list = a'.states |> List.filter (fun x -> not (x = "ϵ")) in
  let unranked_terminals: terminal list = a'.alphabet |> List.filter (fun s -> not (sym_equals s "ε"))
    |> List.fold_left (fun acc (name, rank) -> match name with
    | "IF" -> if (rank = 3) then acc @ ["IF"; "THEN"; "ELSE"] else if (rank = 2) 
      then acc @ ["IF"; "THEN"] else raise (Failure "Rank of IF is neither 2 nor 3")
    | "LPARENRPAREN" -> acc @ ["LPAREN"; "RPAREN"]
    | s -> acc @ [s] ) [] |> remove_dups in
  let prods: production list = a'.transitions |> List.map (fun (st, (sym, st_ls)) ->
    if (sym_equals sym "N" || sym_equals sym "B") then (st, ("ε", [fst sym])) 
    else (st, (fst sym, st_ls))) in
  let cfg_res = 
    { nonterms = nonterms_excl_eps ; terms = unranked_terminals
    ; start = a'.start_state; productions = prods } in
  printf "\nCFG resulted from the TA : \n"; Pp.pp_cfg cfg_res;
  cfg_res


(** cfg_to_parser : once convert to grammar, write it on the parser.mly file *)
let cfg_to_parser (parser_file: string) (debug_print: bool) (g: cfg): unit =
  let open Printf in
  printf "\nWrite the grammar on parser file %s\n" parser_file;
  if debug_print then (printf "\n  Input grammar:\n"; Pp.pp_cfg g);
  (** helpers *)
  let append_strs ls = ls |> List.fold_left (fun acc x -> 
    if (acc = "") then x else acc ^ " " ^ x) "" in
  let replace_wgstart (lst: string list): string = 
    let rec loop (ls: string list) (after_colon: bool) (acc_ls: string list): string =
      match ls with [] -> List.rev acc_ls |> append_strs
      | h :: tl -> let start_new = g.start in
        if (after_colon) then loop tl false (start_new :: acc_ls) 
        else if (h = ":") then loop tl true (h :: acc_ls) 
        else  loop tl after_colon (h :: acc_ls)
    in loop lst false []
  in
  (** Store lines_to_keep until the beginning of productions *)
  let ic = open_in parser_file in
  let rec divide_lines inp before_prod acc_keep prog_id: string list =
    match (read_line inp) with
    | None -> List.rev ("" :: acc_keep)
    | Some s ->
      (* collect 'prog_id' and pass in to divide_lines *)
      if (starts "%start" s) then (let name = List.nth (s |> String.split_on_char ' ') 1 
      in divide_lines inp before_prod (s::acc_keep) name)
      else if (starts prog_id s) 
      (* if starts with 'prog_id', replace with 'start_new' and pass in 'str_new' *)
      then let str_new = replace_wgstart (String.split_on_char ' ' s) 
      in divide_lines inp false (str_new :: acc_keep) prog_id
      else if (before_prod) then divide_lines inp before_prod (s :: acc_keep) prog_id
      else List.rev ("" :: acc_keep)
  in let lines_to_keep = divide_lines ic true [] "%dummy_id" in
  (** collect production list in blocks *)
  let collect_blocks (lst: production list): (production list) list =
    let rec blocks_loop ls curr_nont acc_prods (acc_res: (production list) list) =
      match ls with [] -> List.rev (acc_prods :: acc_res)
      | (nont, _) as prod_h :: prods_tl -> 
        if (curr_nont = "") then blocks_loop prods_tl nont (prod_h :: acc_prods) acc_res else
        if (curr_nont = nont) then blocks_loop prods_tl curr_nont (prod_h :: acc_prods) acc_res else
        (* if not (curr_not = nont), change 'curr_nont' to 'nont' and pass 'acc_prods' to 'acc_res' *)
        let block = List.rev acc_prods in blocks_loop prods_tl nont (prod_h :: []) (block :: acc_res)
    in blocks_loop lst "" [] []
  in 
  (** specify rules on writing a corresponding line per production on parser.mly *)
  let corr_line (lhs: nonterminal) (op: terminal) (sls: string list): string list = 
    let beginning = "  | " in 
    if (is_cond_expr lhs && (List.hd sls = "B") && op = "ε") 
      then (beginning ^ "TRUE { Bool true }") :: (beginning ^ "FALSE { Bool false }") :: [] 
    else if ((List.hd sls = "N") && op = "ε") 
      then (beginning ^ "INT { Int $1 }") :: []
    else if ((List.length sls = 1) && op = "ε") 
      then (beginning ^ List.hd sls ^ " { $1 }") :: []
    else if ((List.length sls = 1) && op = "LPARENRPAREN") 
      then (beginning ^ "LPAREN " ^ (List.hd sls) ^ " RPAREN { Paren $2 }") :: []
    else if ((List.length sls = 2) && op = "IF")
      then (beginning ^ "IF " ^ List.nth sls 0 ^ " THEN " ^ List.nth sls 1
      ^ " { If ($2, Then ($4, Else Na)) }") :: []
    else if ((List.length sls = 3) && op = "IF")
      then (beginning ^ "IF " ^ List.nth sls 0 ^ " THEN " ^ List.nth sls 1 
      ^ " ELSE " ^ List.nth sls 2 ^ " { If ($2, Then ($4, Else Na)) }") :: []
    else (* if (op = "MUL" || op = "PLUS")  *)
      (* note: 'op_new' depends on how lexer defines operations *)
      let op_new = op |> String.lowercase_ascii |> String.capitalize_ascii in
      (beginning ^ List.nth sls 0 ^ " " ^ op ^ " " ^ List.nth sls 1 ^ " { " ^ op_new ^ " ($1, $3) }") :: []
  in
  let write_block (prods_blocks: production list): string list = 
    let rec write_loop ls curr_nont acc = 
      match ls with [] -> acc @ ["  ;"; ""]
      | (nont, (t, sls)) :: b_tl ->
        if (curr_nont = "") then 
          let str_fst = (nont ^ ":") :: [] in
          let str_snd = corr_line nont t sls in 
          write_loop b_tl nont (acc @ str_fst @ str_snd) 
        else if (curr_nont = nont) then 
          let str = corr_line nont t sls in
          write_loop b_tl nont (acc @ str)
        else raise (Failure "Block should have the same nonterminal on LHS.")
    in write_loop prods_blocks "" []
  in
  (* regroup production list so that all the ones with the same lhs *)
  let regroup_blocks (blks: production list list): production list list = 
    (* helper to remove any duplicate productions in the production list *)
    let rec remove_dup_prods (prods_acc: production list) (prods: production list): production list = 
      let prods_equal p1 p2: bool = match p1, p2 with 
      | (nont1, (t1, sls1)), (nont2, (t2, sls2)) -> 
        if (List.length sls1 = List.length sls2) then (nont1 = nont2) && (t1 = t2) 
          && (List.fold_left2 (fun ac s1 s2 -> (s1 = s2) && ac) true sls1 sls2) else false in
      match prods with [] -> prods_acc
      | prod_h :: prods_tl -> 
        if (List.exists (fun x -> prods_equal x prod_h) prods_acc) 
        then remove_dup_prods prods_acc prods_tl else remove_dup_prods (prod_h::prods_acc) prods_tl
    in
    (* helper to traverse all productions and collection productions with the same (lhs) nonterminal *)
    let traverse_blks (nont_to_match: nonterminal): production list = 
      (List.flatten blks) |> List.fold_left (fun acc ((nont, _) as prod) ->
        if (nont = nont_to_match) then prod :: acc else acc) [] in
    let rec loop blk_lst nonts_so_far (res_acc: production list list) =
     match blk_lst with [] -> res_acc
      | (nont, _) :: blk_tl -> 
         if (List.mem nont nonts_so_far) then loop blk_tl nonts_so_far res_acc else 
          (let res_blk = traverse_blks nont |> remove_dup_prods [] 
           in loop blk_tl (nont::nonts_so_far) (res_blk::res_acc))
    in loop (List.flatten blks) [] []
  in
  let lines_added: string list = 
    let prods_blocks: production list list = collect_blocks g.productions |> regroup_blocks in 
    if debug_print then (printf "\n  >> Collected blocks:\n\n"; prods_blocks |> List.iter (fun b -> 
      Pp.pp_productions b; printf "\n\n")); 
    List.fold_left (fun acc blks -> acc @ (write_block blks)) [] prods_blocks in
  if debug_print then (printf "\n  >> Lines added:\n\n"; lines_added |> List.iter (fun l ->
    printf "\t%s\n" l); printf "\n\n");
  let oc = open_out parser_file in
  lines_to_keep @ lines_added |> List.iter (fun ln -> fprintf oc "%s\n" ln);
  close_out oc

(** convertToGrammar : *)
let convertToGrammar (ta_inp: ta) (versatiles: (terminal * int list) list) (debug: bool) (file: string) =
  ta_inp |> ta_to_cfg versatiles debug |> cfg_to_parser file debug

(* ******************** Part III. Specify associativity > parser.mly ******************** *)

(** specify_associativity : specify associativity on parser file per user input (0, 1, 2) *)
let specify_associativity (parser_file: string) (ind: int) (trees: tree * tree) (debug_print: bool): unit =
  let assoc = match ind with 0 -> "%right" | 1 -> "%left" (* | 2 -> "%nonassoc" *)
    | _ -> raise (Failure "Incorrect input number for associativity") in
  let op: string = match node_symbol (fst trees) with 
    | "*" -> "MUL" | "+" -> "PLUS" | s -> s in
  let line_to_add: string = assoc ^ " " ^ op in
  let open Printf in if debug_print then
    printf "\nWrite associativity %s of %s on parser file %s\n" assoc op parser_file;
  (** Divide lines to add associativity in between them *)
  let ic = open_in parser_file in
  let rec divide_lines inp after_eof acc_prior acc_latter: string list * string list =
    match (read_line inp) with
    | None -> (List.rev acc_prior), (List.rev acc_latter)
    | Some s ->
      (* assume definitions of all tokens end with the line '%token EOF' *)
      if (after_eof) 
      then divide_lines inp after_eof acc_prior (s::acc_latter) 
      else if (starts "%token EOF" s) 
      then divide_lines inp true (s::acc_prior) acc_latter
      else divide_lines inp after_eof (s::acc_prior) acc_latter
  in let lines_prior, lines_latter = divide_lines ic false [] [] in 
  let oc = open_out parser_file in 
  lines_prior @ [""; line_to_add] @ lines_latter |> List.iter (fun l -> fprintf oc "%s\n" l);
  close_out oc

