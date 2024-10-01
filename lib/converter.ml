open Cfg
open Ta
open Utils
open Treeutils

exception State_with_no_matching_order
exception Trivial_symbols_not_found_in_prods

(* ******************** Part I. Conversion of parser.mly > CFG > TA ******************** *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let ( $ ) a b = a b

let extract_cfg (debug_print: bool) (filename : string) : cfg2 = 
  let lines = read_file filename in
  let sanitize line = 
    let line = String.trim line in
    if String.starts_with ~prefix:"#" line then ""
    else line
  in
  let clean lines = 
    List.map sanitize lines
    |> List.filter (fun x -> x <> "" && x <> "EOF")
  in
  (* extract sections *)
  let sectionToLine = Hashtbl.create 5 in

  let rec addLines section acc lines: unit =
    match lines with
    | h :: t -> (match h with
      | a when (String.starts_with ~prefix:"=" a) 
        -> let new_section = List.nth (String.split_on_char '=' a) 1 
          in 
            Hashtbl.add sectionToLine section acc;
            addLines new_section [] t
      | a 
        -> (addLines section (a :: acc) t)
      )
    | [] -> Hashtbl.add sectionToLine section acc
  in addLines "" [] lines;
  (* check sections *)
  assert (Hashtbl.mem sectionToLine "s");
  assert (Hashtbl.mem sectionToLine "nt");
  assert (Hashtbl.mem sectionToLine "t");
  assert (Hashtbl.mem sectionToLine "p");
  (* extract cfg *)
  let start = List.hd (clean $ Hashtbl.find sectionToLine "s") in
  let nonterms = ref (clean $ Hashtbl.find sectionToLine "nt") in
  let terms= clean $ Hashtbl.find sectionToLine "t" in
  let t_prods = clean $ Hashtbl.find sectionToLine "p" in
  let added_eps = ref false in
  let productions = List.mapi (fun i x -> 
      let split = Str.bounded_split (Str.regexp "->") x 2 in
      let lhs, rhs = 
        List.hd split, 
        if List.length split > 1 
          then List.nth split 1
          else (if not !added_eps 
            then (nonterms := "ϵ" :: !nonterms; added_eps := true);"ϵ")
      in
      let rhs = clean $ String.split_on_char ' ' rhs in
      (sanitize lhs, i,
      List.map (fun x ->
        if List.exists (fun y -> y = x) terms
          then T x
        else if List.exists (fun y -> y = x) !nonterms
          then Nt x
        else raise (Failure "RHS contains unknown symbols")) rhs)
    ) t_prods
  in
  let nonterms = !nonterms in
  if debug_print then
    Printf.printf "CFG extracted from %s:\n" filename;
    Printf.printf "Start: %s\n" start;
    Printf.printf "Nonterminals: %s\n" (String.concat " " nonterms);
    Printf.printf "Terminals: %s\n" (String.concat " " terms);
    Printf.printf "Productions:\n";
    List.iter (fun (lhs, i, rhs) -> Printf.printf "%d: %s -> %s\n" i lhs (String.concat " " (List.map (function T x -> x | Nt x -> x) rhs))) productions;
  { nonterms; terms; start; productions }

let cfg3_of_cfg2 (cfg2: cfg2): cfg3 =
  { 
    nonterms = cfg2.nonterms; 
    terms = cfg2.terms; 
    start = cfg2.start; 
    productions = cfg2.productions
      |> List.map (fun (lhs, _, rhs) ->
        let rec remove_first_t = function
          | [] -> (None, [])
          | T a :: t -> (Some (T a), t)
          | h :: t -> (match remove_first_t t with
            | (a, l) -> (a, h :: l))
        in
        let len = List.length rhs in
        let first_t, rhs' = match remove_first_t rhs with 
        | (Some (T x), l) when x = "LPAREN" 
          -> (("LPARENRPAREN", len - 2), l)
        | (Some (T x), l) -> ((x, len - 1), l)
        | (None, l) -> (("ε", len), l)
        | _ -> assert false
        in
        let proj = List.fold_right (fun a acc -> match a with
          T _ -> acc | Nt x -> x :: acc) rhs' []
        in
        (lhs, (first_t, proj), rhs)
      )
  }

(** enhance_appearance : helper to enhance the symbol representation *)
let enhance_appearance (a: ta): ta =
  let change_symbol s = 
    let s', s'' = fst s, snd s in
    if (s' = "LPARENRPAREN") then "()", s'' else s 
  in
  let alph_updated: symbol list = a.alphabet 
    |> List.map (fun sym -> change_symbol sym) 
  in
  let trans_updated: transition list =
    a.transitions 
    |> List.map (fun (st, (sym, st_ls)) ->
      let sym_new = change_symbol sym 
      in (st, (sym_new, st_ls))) 
  in
  {
    states = a.states; 
    alphabet = alph_updated; 
    start_state = a.start_state; 
    transitions = trans_updated;
    trivial_sym_nts = a.trivial_sym_nts 
  }

let optimize_cfg_starts (g: cfg3) (level: int) =
  let open List in
  let rec h (nts: nonterminal list) (starts: nonterminal list) (prods: production2 list) level =
    if level = 0 then (nts, starts, prods)
    else
      let next_starts = map
        (fun s -> filter 
          (fun (lhs, _, _) -> lhs = s) 
          prods
          |> partition (fun (_, (sym, _), _) -> sym = ("ε", 1))
          |> fun a -> (s, a)
        )
        starts
      in
      let (nts_to_remove, start_to_keep) = next_starts
        |> partition (fun (_, (_, nontrivial_prods)) -> nontrivial_prods = [])
        |> fun (a, b) -> (map fst a, map fst b)
      in
      let new_prods = filter
        (fun (lhs, _, _) -> not (mem lhs nts_to_remove))
        prods
      in
      let new_nts = filter 
        (fun nt -> not (mem nt nts_to_remove)) 
        nts 
      in
      let new_starts = next_starts
        |> map (fun (_, (trivial_prods, _)) -> trivial_prods)
        |> flatten
        |> map (fun (_, _, rhs) -> match hd rhs with Nt x -> x | _ -> assert false)
      in
      let (rec_nts, rec_starts, rec_prods) =
        h new_nts new_starts new_prods (level - 1)
      in
      (rec_nts, start_to_keep @ rec_starts, rec_prods)
  in h g.nonterms [g.start] g.productions level

let cfg_to_ta (debug_print: bool) (g: cfg3): 
  ta2 * restriction list * ((symbol * int) * sigma list) list * 
  ((int, symbol list) Hashtbl.t) * (symbol * state) list * symbol list =
  let open List in
  let open Printf in
  let (nonterms, starts, prods) = optimize_cfg_starts g 2 in
  if debug_print then 
    (printf "\n\t *** (debugging) Nonterminals\n\t"; nonterms |> Pp.pp_nonterminals; printf "\n");
  let ranked_alphabet = map 
    (fun (_, (a, _), _) -> a)
    prods
    |> remove_dups
  in
  (* helper to get restrictions from transitions *)
  let trans_to_restrictions trans_ls nt_ls init_sts =
    let rec fixpoint f (x: (nonterminal, int) Hashtbl.t ref) =
      let changed = f x in
      if changed then fixpoint f x else x
    in
    let nt_to_order = ref (Hashtbl.create 10) in
    let num_nt = length nt_ls in
    iter
      (fun st -> Hashtbl.add !nt_to_order st (num_nt + 1))
      nt_ls;
    iter (fun st -> Hashtbl.replace !nt_to_order st 0) init_sts;
    (* 
    Hashtbl.add !nt_to_order "ϵ" (num_nt + 1); (* pseudo nt *)
     *)
    let get_order table =
      fold_left (fun acc (st, (_, rhs), _) ->
          let changed = ref false in
          iter 
            (fun s ->
                (* *** debugging *** *)
                (* if debug_print then printf "\n\t *** (debugging) Getting order for nonterminal %s" s; *)
                if (String.equal s "ϵ") then 
                  (* Temporary fix *)
                  (Hashtbl.replace !table s (-1);
                  changed := false)
                else 
                  (let ord = Hashtbl.find !table s in
                  let ord' = Hashtbl.find !table st in
                  if ord > (ord' + 1) then (
                    Hashtbl.replace !table s (ord' + 1);
                    changed := true))
            ) rhs;
          acc || !changed
        )
        false
        trans_ls
    in
    let nt_to_order' = fixpoint get_order nt_to_order in
    let states_ordered = Hashtbl.fold
      (fun k v acc -> (k, v) :: acc) 
      !nt_to_order' [] 
    in
    (runIf debug_print (fun _ -> 
      printf "\nOrder of states : \n";
      iter
        (fun (st, lvl) -> printf "(%s, %i)\n" st lvl) 
        states_ordered)
    );
    let rec get_o_base_precedence trans acc_res =
      match trans with
      | [] -> List.rev acc_res
      | (lhs_st, (sym, _), rhs) :: tl ->
        let ord = match (assoc_opt lhs_st states_ordered) with
          | None ->
            (runIf debug_print (fun _ -> 
              printf "\n\nState %s has no matching order.\n" lhs_st));
            raise State_with_no_matching_order
          | Some o -> o
        in
        get_o_base_precedence tl ((Prec (sym, ord), (lhs_st, rhs))::acc_res)
    in get_o_base_precedence trans_ls []
  in
  (* let trans = fold_right 
    (fun (n, (t, ls), _) acc -> (n, (t, ls)) :: acc) 
    prods []
  in *)
  let restrictions : ((restriction * (nonterminal * sigma list)) list) = trans_to_restrictions
    prods nonterms starts 
  in 
  (* *** debug *** *)
  if debug_print then printf "\n\t *** (debugging) Check restrictions!\n";Pp.pp_restriction'_lst restrictions;
  (* trivial nts are nts with only zero arity productions *)
  let trivial_nts : state list = g.nonterms
    |> filter (fun nt ->
      filter (fun (lhs, _, _) -> lhs = nt) g.productions
      |> for_all (fun (_, ((_, a), _), _) -> a = 0)
    ) 
  in
  let find_symbol_from_productions (nt: nonterminal) (p: production2 list): symbol = 
    let rec loop prods =
      match prods with [] -> raise Trivial_symbols_not_found_in_prods
      | (n, ((term, i), _), _) :: tl -> 
        if nt = n then (term, i)
        else loop tl
    in loop p
  in
  let trivial_syms_nts : (symbol * nonterminal) list = trivial_nts |> map (fun nt -> 
    if (nt = epsilon_state) then (epsilon_symb, nt) else ((find_symbol_from_productions nt g.productions), nt)) 
  in 
  (* *** debug *** *)
  if debug_print then (printf "\n\t *** (debugging) Check trivial symbols and nonterminals\n"; 
    trivial_syms_nts |> List.iter (fun (s, nt) -> Pp.pp_symbol s; printf " --- paried with ---> State %s \n" nt ));
  (* ********************************************** *)
  (* Uncomment the following maps when ready to use *)
  (* ********************************************** *)
  let add htbl k v =
    if Hashtbl.mem htbl k
    then Hashtbl.add htbl k (v :: (Hashtbl.find htbl k))
    else Hashtbl.add htbl k [v]
  in
  (* o_bp : order -> symbol list *)
  let o_bp_tbl : (int, symbol list) Hashtbl.t  = Hashtbl.create (length prods) in
  (* transitions_tbl : lhs * symbol -> (sigma list) list   ----   (rhs <=> sigma list) *)
  let transitions_tbl : ((state * symbol), sigma list list) Hashtbl.t = 
    Hashtbl.create (length prods) in
    (iter (fun (prc, (lhs, rhs)) ->
      let s, o = match prc with
      | Prec x -> x
      | _ -> assert false
      in
      (* Only add non-trivial symbols to o_bp_tbl *)
      match s with (_, rnk) -> 
        let get_fst_rhs_sigma rhs_ls = 
          if (List.is_empty rhs_ls) then ""
          else begin 
            match (List.hd rhs_ls) with Nt st -> st 
            | T _ -> "" end 
        in
        if ((rnk != 0) && (not (List.mem (get_fst_rhs_sigma rhs) trivial_nts)))
        then begin 
          (* If key already exists, then simply add to existing ones *)
          let exist_val = Hashtbl.find_opt o_bp_tbl o in
          match exist_val with None -> add o_bp_tbl o s 
          | Some ls -> 
            (* *** debugging *** *)
            (if debug_print then printf "\n\t *** (debugging) For order %i" o;
            printf "\n\t *** already exist sym_lst so add this symbol to symbol list "; 
            ls |> List.iter Pp.pp_symbol; printf "\n";
            Hashtbl.replace o_bp_tbl o (s::ls))
        end;
        (if debug_print then printf "\n\t ****** (debugging) Add (State %s, " lhs; Pp.pp_symbol s;
        printf ") ---> RHS list "; rhs |> List.iter Pp.pp_sigma);
      let exist_in_trantbl = Hashtbl.find_opt transitions_tbl (lhs, s) 
      in match exist_in_trantbl with None -> add transitions_tbl (lhs, s) rhs
         | Some ls' -> 
           Hashtbl.replace transitions_tbl (lhs, s) (rhs::ls')
    ) restrictions);
    Hashtbl.iter (fun k v -> Hashtbl.replace o_bp_tbl k (remove_dups v)) o_bp_tbl; 
  (* ********************************************** *)
  let ta_res: ta2 =
    { 
      states = nonterms;
      alphabet = ranked_alphabet;
      start_states = starts;
      transitions = transitions_tbl;
      trivial_sym_nts = trivial_syms_nts
    } 
  (* |> enhance_appearance *) 
  in
  (* *** debug *** *)
  printf "\n >> Trivial non-terminals: [ ";
  trivial_syms_nts |> iter (fun (s, x) -> printf " ("; Pp.pp_symbol s; printf ", %s ) " x); printf "]\n";
  printf "\n >> Restrictions restriction'_lst O_bp obtained from the TA_g : \n"; Pp.pp_restriction'_lst restrictions;
  printf "\n >> Order -> symbol list O_bp map : \n"; Pp.pp_obp_tbl o_bp_tbl;
  printf "\n >> Transitions hashmap : \n"; Pp.pp_transitions_tbl transitions_tbl;
  printf "\nTA obtained from the original CFG : \n"; Pp.pp_ta2 ta_res;
  let trivial_syms = trivial_syms_nts |> map fst |> filter (fun x -> not (syms_equals epsilon_symb x)) in 
  printf "\n *** (debugging) Trivial symbols: [ "; trivial_syms |> List.iter Pp.pp_symbol; printf " ] \n";
  let restrictions_without_trivials : restriction list = 
    (restrictions |> split |> fst) |> filter (fun x -> match x with Prec (s, _) | Assoc (s, _) -> 
        List.fold_left (fun acc tsym -> acc && not (syms_equals s tsym)) true trivial_syms)
  in 
  (* restrictions : (restriction * (load * sigma list)) list *)
  let rec to_sym_ord_rhs_lst (rls: (restriction * (state * sigma list)) list) (acc: ((symbol * int) * sigma list) list) = 
    match rls with [] -> List.rev acc 
    | (r, (_lhs, sig_ls)) :: tl -> begin 
      match r with 
      | Prec (sym, o) -> 
        let to_acc = ((sym, o), sig_ls) in to_sym_ord_rhs_lst tl (to_acc::acc)
      | Assoc _ -> to_sym_ord_rhs_lst tl acc
    end 
  in
  let sym_ord_to_rhs_lst : ((symbol * int) * sigma list) list = to_sym_ord_rhs_lst restrictions []
  (* 
  List.map (fun (r, (_lhs, sig_ls)) -> 
      match r with Prec (s, o) -> ((s, o), sig_ls)) |> List.filter (fun ((s, o), _) -> 
        List.fold_left (fun acc tsym -> acc && not (syms_equals s tsym)) true trivial_syms
        | Assoc (s, a) -> )  *)
  in 
  ta_res, restrictions_without_trivials, sym_ord_to_rhs_lst, o_bp_tbl, trivial_syms_nts, trivial_syms

let convertToTa (file: string) (debug_print: bool):
  ta2 * restriction list * ((symbol * int) * sigma list) list * 
  ((int, symbol list) Hashtbl.t) * (symbol * state) list * symbol list = 
  (* Pass in terminals which can have multiple arities, eg, "IF" *)
  (* "./lib/parser.mly" |> parser_to_cfg debug_print |> cfg_to_ta versatiles debug_print *)
  file
  |>
  (runIf debug_print (fun _ -> Printf.printf "\n\nConvert parser.mly to its corresponding CFG\n"); 
  extract_cfg debug_print)
  |>
  (runIf debug_print (fun _ -> Printf.printf "\n\nConvert between CFG formats\n");
  cfg3_of_cfg2)
  |>
  (runIf debug_print (fun _ -> Printf.printf "\n\nConverting CFG to TA\n");
  cfg_to_ta debug_print)


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
  ; start_state = a.start_state ; transitions = trans_updated ; trivial_sym_nts = a.trivial_sym_nts }

let ta_to_cfg (versatileTerminals: (terminal * int list) list) (debug_print: bool) (a: ta): cfg = 
  let open Printf in
  let a' = undo_enhancement a in
  printf "\nConvert TA to its corresponding CFG:\n\n  Input TA:\n"; Pp.pp_ta a';
  if debug_print then (printf "\n  >> Versatile sybol list: [ ";
  versatileTerminals |> List.map fst |> List.iter (fun x -> printf "%s " x); printf "]\n");
  (* helpers *)
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
  let cfg_res: cfg = 
    { nonterms = nonterms_excl_eps ; terms = unranked_terminals
    ; start = a'.start_state; productions = prods } in
  printf "\nCFG resulted from the TA : \n"; Pp.pp_cfg cfg_res;
  cfg_res


(** cfg_to_parser : once convert to grammar, write it on the parser.mly file *)
let cfg_to_parser (parser_file: string) (debug_print: bool) (g: cfg): unit =
  let open Printf in
  printf "\nWrite the grammar on parser file %s\n" parser_file;
  if debug_print then (printf "\n  Input grammar:\n"; Pp.pp_cfg g);
  (* helpers *)
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
  (* Store lines_to_keep until the beginning of productions *)
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
  (* collect production list in blocks *)
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
  (* specify rules on writing a corresponding line per production on parser.mly *)
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
    List.fold_left (fun acc blks -> (write_block blks) @ acc) [] prods_blocks in
  if debug_print then (printf "\n  >> Lines added:\n\n"; lines_added |> List.iter (fun l ->
    printf "\t%s\n" l); printf "\n\n");
  let oc = open_out parser_file in
  lines_to_keep @ lines_added |> List.iter (fun ln -> fprintf oc "%s\n" ln);
  close_out oc

(** convertToGrammar : *)
let convertToGrammar (ta_inp: ta) (versatiles: (terminal * int list) list) (debug: bool) (file: string) =
  ta_inp |> ta_to_cfg versatiles debug |> cfg_to_parser file debug

(* Below: currently not taken into consideration  *)
(* ******************** Specify associativity > parser.mly ******************** *)

(** specify_associativity : specify associativity on parser file per user input (0, 1, 2) *)
let specify_associativity (parser_file: string) (ind: int) (trees: tree * tree) (debug_print: bool): unit =
  let assoc = match ind with 0 -> "%right" | 1 -> "%left" (* | 2 -> "%nonassoc" *)
    | _ -> raise (Failure "Incorrect input number for associativity") in
  let op: string = match node_symbol (fst trees) with 
    | "*" -> "MUL" | "+" -> "PLUS" | s -> s in
  let line_to_add: string = assoc ^ " " ^ op in
  let open Printf in if debug_print then
    printf "\nWrite associativity %s of %s on parser file %s\n" assoc op parser_file;
  (* Divide lines to add associativity in between them *)
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

