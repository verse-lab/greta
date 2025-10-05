open Cfg
open Ta
open Utils
open Cfgutils
(* open Treeutils *)

exception State_with_no_matching_order
exception Trivial_symbols_not_found_in_prods
exception Either_starts_or_types_first
exception State_no_match_in_states_map
exception Type_spec_line_last_should_be_nonterminal
exception No_nontrivial_prod_mapping
exception Nonterms_length_must_equal
exception Inconsistent_start_states

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



let extract_cfg (debug_print: bool) (filename : string) : cfg =
  let lines = read_file filename in
  let sanitize line = 
    let line = String.trim line in
    if String.starts_with ~prefix:"#" line then ""
    else line
  in
  let clean lines = 
    List.map sanitize lines
    |> List.filter (fun x -> x <> "") (*  && x <> "EOF" *)
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
  (* Note: raw_start_nonterms is always [start] *)
  let raw_start_nonterms: string list = clean $ Hashtbl.find sectionToLine "s" in
  let raw_start_nonterm: string = 
    if ((List.length raw_start_nonterms) = 1) then raw_start_nonterms |> List.hd else raise (Failure "") in
  let start_nonterms = ref [] in
  let nonterms = ref (clean $ Hashtbl.find sectionToLine "nt") in
  let terms= clean $ Hashtbl.find sectionToLine "t" in
  let t_prods = clean $ Hashtbl.find sectionToLine "p" in
  let added_eps = ref false in
  let raw_productions: (string * int * sigma list) list = List.mapi (fun i x -> 
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
          then Term x
        else if List.exists (fun y -> y = x) !nonterms
          then Nt x
        else 
          (if debug_print then Printf.printf "\nWhat is this? %s\n" x;
          raise (Failure "RHS contains unknown symbols"))) rhs)
    ) t_prods
  in
  let prods: production list = raw_productions |> List.map (fun (lhs, _i, rhs) -> (lhs, rhs))
    |> List.filter (fun (lhs, rhs) -> if (lhs = raw_start_nonterm) 
      then (start_nonterms := (start_nonterm_of_sigls rhs)::!start_nonterms); not (lhs = raw_start_nonterm)) in
  let nonterms = !nonterms in
  if debug_print then begin
    Printf.printf "CFG extracted from %s:\n" filename;
    Printf.printf "Start nonterminals: %s\n" (String.concat " " !start_nonterms);
    Printf.printf "Nonterminals: %s\n" (String.concat " " nonterms);
    Printf.printf "Terminals: %s\n" (String.concat " " terms);
    Printf.printf "Productions:\n";
    List.iter (fun (lhs, i, rhs) -> Printf.printf "%d: %s -> %s\n" i lhs (String.concat " " (List.map (function Term x -> x | Nt x -> x) rhs))) raw_productions;
  end;
  { nonterms = nonterms ; terms = terms ; starts = !start_nonterms ; productions = prods }

let collect_ta_trans_symbols_from_cfg (g: cfg) (_debug: bool): ((state * symbol) * beta list) list * symbol list =
  let rec collect_loop (ls: production list) (trans_acc: ((state * symbol) * beta list) list) (syms_acc: symbol list) = 
    match ls with 
    | [] -> List.rev trans_acc, List.rev syms_acc 
    | curr_prod :: rest_prods ->
      let nt, curr_sigls = (fst curr_prod), (snd curr_prod) in
      let curr_id = !count in (count := curr_id + 1);
      let curr_sym = (curr_id, (first_terminal_of curr_sigls), (List.length curr_sigls)) in
      let curr_beta_ls: beta list = curr_sigls |> sigma_list_to_beta_list in
      let trans_to_acc = ((nt, curr_sym), curr_beta_ls) in 
      collect_loop rest_prods (trans_to_acc::trans_acc) (curr_sym::syms_acc)
  in let trans_res, syms_res = collect_loop g.productions [] [] 
  in 
  (* if debug then (wrapped_printf debug "\nCollected transitions for TA:\n\n"; 
    Pp.pp_transitions trans_res; Pp.pp_alphabet syms_res); *)
  trans_res, syms_res

let collect_nonterm_orders (start_nonterms: nonterminal list) (nonterms: nonterminal list) (prods: production list) (debug: bool): 
  (nonterminal * int) list = 
  (* First group productions based on nonterminals *)
  let prods_grouped: (nonterminal * production list) list = group_productions nonterms prods debug
  in
  (* 
  if debug then 
    (wrapped_printf debug "\n\t* Prods grouped:\n"; prods_grouped |> List.iter (fun (nt, prods) -> 
      wrapped_printf debug "\tLHS nonterm: %s\n" nt; Pp.pp_productions prods));
   *)
  (* helper to find productions *)
  let prods_of_nonterm (nt: nonterminal): production list = 
    let nt_prods: (nonterminal * production list) list = 
      prods_grouped |> List.filter (fun (x, _prods_from_x) -> x = nt)
    in let res_prods_of_nonterm = if (List.is_empty nt_prods) then [] else nt_prods |> List.hd |> snd in
    (* 
    if debug then (wrapped_printf debug "\nProds for LHS nonterm %s" nt; 
      res_prods_of_nonterm |> Pp.pp_productions); 
    *)
    res_prods_of_nonterm
  in 
  (* Start from [(start, 0)] *)
  let init_sts_orders = start_nonterms |> List.map (fun s -> (s, 0)) 
  in
  (* Reasoning: 
     RHS nonterms that are diff from LHS nonterm get (order of LHS nonterm) + 1, as long as these nonterms have not been seen before
   *)
  let rec collect_nts_orders (nts_visited: nonterminal list) (nts_left: nonterminal list) (nt_orders_acc: (nonterminal * int) list): (nonterminal * int) list = 
    match nts_left with 
    | [] -> nt_orders_acc |> remove_dups
    | nt_hd :: nt_tl -> 
      (* 
      if debug then (wrapped_printf debug "\nFinding order for RHS starting from LHS nonterm %s" nt_hd);
      *)
      let curr_nt_prods = (prods_of_nonterm nt_hd) in
      let lhs_nonterm = (lhs_nonterm_of_prods curr_nt_prods) in
      let lhs_nonterm_order = (List.assoc lhs_nonterm nt_orders_acc) in
      let rhs_nonterms_from_prods = (rhs_nonterms_of_prods lhs_nonterm curr_nt_prods) in
      let rhs_nonterms_unvisited = rhs_nonterms_from_prods |> List.filter (fun x -> not (List.mem x nts_visited)) in
      let rhs_nonterms_orders_to_acc = rhs_nonterms_unvisited |> List.map (fun nt -> (nt, lhs_nonterm_order + 1)) in 
      collect_nts_orders (nt_hd::nts_visited) (nt_tl@rhs_nonterms_unvisited) (nt_orders_acc @ rhs_nonterms_orders_to_acc) 
  in let nt_orders_res = collect_nts_orders [] start_nonterms init_sts_orders 
  in 
  if debug then (wrapped_printf debug "\n Collected (order, nonterminal):\n\n"; 
    nt_orders_res |> List.iter (fun (nt, l) -> wrapped_printf debug "\t  Nonterminal  %s => Order %d\n" nt l));
  nt_orders_res

let collect_sym_orders_wrt_nonterm_order (nts_ordered: (nonterminal * int) list) (trans_from_cfg: ((state * symbol) * beta list) list) (debug: bool):
 (nonterminal * int * symbol list) list =
 (* helper to find order of nonterm based on 'nts_ordered' *)
  let order_of_nonterm (nt: nonterminal): int = List.assoc nt nts_ordered 
  in
  let rec collect_sym_orders (trans_ls: ((state * symbol) * beta list) list) (acc: (nonterminal * symbol list) list) = 
    match trans_ls with [] -> acc
    | ((nt, sym), _bls) :: trans_tl -> 
      let new_acc: (nonterminal * symbol list) list = 
        if (List.mem_assoc nt acc) 
        then 
          begin 
            let old_syms = List.assoc nt acc in 
            let after_removing_nt_old_syms: (nonterminal * symbol list) list = (List.remove_assoc nt acc) in 
            (nt, sym::old_syms) :: after_removing_nt_old_syms
          end
        else (nt, [sym])::acc
      in 
        collect_sym_orders trans_tl new_acc
  in let raw_nt_sym_orders: (nonterminal * symbol list) list = collect_sym_orders trans_from_cfg [] 
  in let nt_sym_orders: (nonterminal * int * symbol list) list = 
    raw_nt_sym_orders |> List.map (fun (nt, sls) -> (nt, (order_of_nonterm nt), sls)) in
  if debug then 
    (wrapped_printf debug "\n Collected (nonterminal, order, symbols):\n\n"; nt_sym_orders |> List.iter (fun (nt, i, sym_ls) -> 
      wrapped_printf debug "\t  Nonterminal %s  Order %d  =>  \t" nt i; sym_ls |> List.iter Pp.pp_symbol; wrapped_printf debug "\n"));
  nt_sym_orders


let cfg_to_ta (debug_print: bool) (g: cfg): 
  ta * restriction list * ((int, symbol list) Hashtbl.t) * (int * production) list =
  if debug_print then
    (wrapped_printf debug_print "\n\t Converting CFG to TA given the following TA \n"; Pp.pp_cfg g);
  
  (* 1. Go through prods to collect alphabet and transitions *)
  let (trans_from_cfg, symbols_from_cfg): ((state * symbol) * beta list) list * symbol list = 
    collect_ta_trans_symbols_from_cfg g debug_print
  in
  let trans_tbl = Hashtbl.create (List.length g.productions) in 
  (trans_from_cfg |> List.iter (fun ((nt, sym), bls) -> 
    Hashtbl.add trans_tbl (nt, sym) bls));
 
  (* 2. Collect O_bp *)
  let nonterms_ordered: (nonterminal * int) list = 
    collect_nonterm_orders g.starts g.nonterms g.productions debug_print 
  in 
  let nonterm_order_symls: (nonterminal * int * symbol list) list = 
    collect_sym_orders_wrt_nonterm_order nonterms_ordered trans_from_cfg debug_print 
  in
  let order_symls: (int * symbol list) list = 
    nonterm_order_symls |> List.map (fun (_nt, lvl, sym_ls) -> (lvl, sym_ls)) |> combine_syms_of_same_order
  in 
  let rest_ls: restriction list = 
    nonterm_order_symls |> List.fold_left (fun acc (_nt, lvl, sym_ls) -> 
      let sym_precedence_ls: restriction list = 
        sym_ls |> List.map (fun s -> Prec (s, lvl))
      in acc @ sym_precedence_ls) []
  in
  let rest_tbl = Hashtbl.create (List.length rest_ls) in 
  (order_symls |> List.iter (fun (lvl, sym_ls) -> 
    Hashtbl.add rest_tbl lvl sym_ls));

  (* 3. Find trivial symbol and nontrminal - Ignore for now *)
  (* 4. Create a map (assoc list) from ID to production *)
  let prods_map_res: (int * production) list =
    trans_from_cfg |> List.map (fun ((lhs_nt, sym), bls) -> 
      let sym_id = (id_of_sym sym) in 
      let curr_sigls: sigma list = production_of_beta_list bls in
      let curr_prod: production = (lhs_nt, curr_sigls) in
      (sym_id, curr_prod)
      )
  in 
  if debug_print then
    (wrapped_printf debug_print "\n ID -> Production Map \n"; 
    prods_map_res |> List.iter (fun (i, p) -> wrapped_printf debug_print "\t  %d -> " i; Pp.pp_production p));
  let res_ta: ta = {
    states = g.nonterms;
    alphabet = symbols_from_cfg;
    final_states = g.starts;
    terminals = g.terms;
    transitions = trans_tbl
    }
  in res_ta , rest_ls, rest_tbl, prods_map_res

let convertToTa (file: string) (debug_print: bool): 
  ta * restriction list * ((int, symbol list) Hashtbl.t) * (int * production) list = 
  let ta_res, obp_res, obp_tbl, prods_map_res = file |> 
  (runIf debug_print (fun _ -> wrapped_printf debug_print "\n\nConvert parser.mly to its corresponding CFG\n");
  extract_cfg debug_print) 
  |>
  (runIf debug_print (fun _ -> wrapped_printf debug_print "\n\nConverting CFG to TA\n");
  cfg_to_ta debug_print)
  in 
  if debug_print then begin
    wrapped_printf debug_print "\nTA obtained from the original CFG : \n"; Pp.pp_ta ta_res;
    (* wrapped_printf debug_print "\n >> Trivial non-terminals: [ ";
    ta_res.trivial_sym_nts |> iter (fun (s, x) -> wrapped_printf debug_print " ("; Pp.pp_symbol s; wrapped_printf debug_print ", %s ) " x); wrapped_printf debug_print "]\n"; *)
    wrapped_printf debug_print "\nOrder -> symbol list O_bp map : \n"; Pp.pp_obp_tbl obp_tbl;
    
  end;
  ta_res, obp_res, obp_tbl, prods_map_res

(* 

(* ******************** Part II. Conversion of TA > CFG > parser.mly ******************** *)

let ta_to_cfg (debug_print: bool) (a: ta2): cfg2 = 
  let wrapped_printf fmt =
    if debug_print then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let nonterms_excl_eps: nonterminal list = a.states |> List.filter (fun x -> not (x = "ϵ")) in
  let unranked_terminals: terminal list = a.alphabet |> List.filter (fun s -> not (sym_equals s "ε"))
    |> 
    List.fold_left (fun acc (name, _rnk) -> match name with
    | "LBRACERBRACE" -> acc @ ["LBRACE"; "RBRACE"]
    | "LBRACKETRBRACKET" -> acc @ ["LBRACKET"; "RBRACKET"]
    | s -> acc @ [s] ) [] 
    |> remove_dups in
  let prods: p list = 
    Hashtbl.fold (fun (st, _sym) sig_lsls acc -> 
      (* wrapped_printf "\n\t For state %s and symbol " st; Pp.pp_symbol sym; wrapped_printf "\n"; *)
      if (List.length sig_lsls) > 1
      then raise Invalid_sigma_list
      else 
        (let sig_ls = List.hd sig_lsls in
         (st, -1, sig_ls) :: acc) (* Temp number since it does not matter at this point *)
      ) a.transitions []
      |> List.sort compare
  in
  let triv_term_nonterm_ls = 
    a.trivial_sym_nts |> List.map (fun (sym, nts) -> ((fst sym), nts)) in
  let cfg_res: cfg2 = 
    { nonterms = nonterms_excl_eps ; terms = unranked_terminals
    ; starts = a.start_states; productions = prods ; triv_term_nonterm_list = triv_term_nonterm_ls } in
  if debug_print then (wrapped_printf "\nCFG (cfg2) resulted from the TA : \n"; Pp.pp_cfg2 cfg_res);
  cfg_res


(** cfg_to_parser : once convert to grammar, write it on the parser.mly file *)
let cfg_to_parser (parser_file: string) (sts_rename_map: (state * state) list) 
  (init_states: state list) (debug_print: bool) (to_write: string) (flag:optimization) (g: cfg2) : unit =
  let wrapped_printf fmt =
    if debug_print then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let open Printf in
  wrapped_printf "\nWrite the grammar on parser file %s\n" parser_file;
  if debug_print then (wrapped_printf "\n  Input grammar:\n"; Pp.pp_cfg2 g);
  (* --- helpers --- *)
  let append_strs ls = ls |> List.fold_left (fun acc x -> 
    if (acc = "") then x else acc ^ " " ^ x) "" 
  in
  let find_mapped_state_of (st: state) (sts_map: (state * state) list) = 
    if debug_print then wrapped_printf "\n\t Looking for mapped state of State %s\n" st;
    match List.assoc_opt st sts_map with Some st -> st
    | None -> raise State_no_match_in_states_map
  in 
  let replace_last (old_id: string) (new_id: string) (type_line: string): string = 
    let str_ls = String.split_on_char ' ' (strip type_line) in
    let len = List.length str_ls in 
    let replaced_ls = 
      let last_elem = List.nth str_ls (len-1) in
      if (String.equal old_id last_elem)
      then str_ls |> List.mapi (fun i s -> if (i = (len-1)) then new_id else s) 
      else raise Type_spec_line_last_should_be_nonterminal
    in append_strs replaced_ls
  in
  let get_prod_nonterm (s: string): string =
    let idx = String.index s ':' 
    in String.sub s 0 idx |> strip_string
  in 
  let get_nonterm_prod (s: string): string = 
    let idx = String.index s ':' in
    let last_idx = (String.length s) - 1
    in String.sub s (idx+1) (last_idx-idx)
  in
  let contains_only_one_colon (s: string) : bool = 
    let first_appear_colon s' = 
      match String.index_from_opt s' 0 ':' with None -> -100 | Some i -> i 
    in 
    let last_appear_colon s' = 
      let str_len = String.length s' in 
      match String.rindex_from_opt s'(str_len - 1) ':' with None -> 999 | Some i -> i 
    in 
    (String.contains s ':') && ((first_appear_colon s) = (last_appear_colon s))
  in
  (* Replace lines w.r.t. start specs *)
  let replace_wgstarts (start_id_lines: (string * string) list) (states_map: (state * state) list): 
    string list * (state * state) list = 
    let rec start_loop ls lins_acc stats_map_acc cnt = 
      match ls with [] -> List.rev lins_acc, stats_map_acc
      | (hd_id, hd_line) :: ltl -> 
        let new_start_state = hd_id in
        let to_replace = Str.regexp hd_id in
        let new_line = Str.global_replace to_replace new_start_state hd_line in
        let new_stats_map = (hd_id, new_start_state) in
        start_loop ltl (new_line::lins_acc) (new_stats_map::stats_map_acc) (cnt+1) 
    in 
    let res_start_lines, start_states_map = start_loop start_id_lines [] [] 0 in
    res_start_lines, (start_states_map @ states_map)
  in
  (* Replace lines w.r.t. type specs *)
  let replace_wgtypes (type_lines: (string * string) list) (states_map: (state * state) list):
    string list = 
    let rec type_loop ls lins_acc = 
      match ls with [] -> lins_acc
      | (hd_id, hd_line) :: ltl -> 
        let mapped_id = find_mapped_state_of hd_id states_map in
        let new_line = replace_last hd_id mapped_id hd_line in 
        type_loop ltl (new_line::lins_acc)
    in type_loop type_lines []
  in
  (* Store lines before and after prods *)
  let ic = open_in parser_file in
  let rec divide_lines inp before_prod acc_keep acc_prods acc_starts acc_types starts_first types_first: 
    string list * string list * (string * string) list * (string * string) list * (bool * bool) =
    match (read_line inp) with
    | None -> List.rev acc_keep, List.rev acc_prods, List.rev acc_starts, acc_types, (starts_first, types_first) 
    | Some s ->
      if (before_prod) 
      then begin
            (* collect lines starting with '%start' and accumulate start_id's *)
            if (starts "%start" s) 
            then (let start_id = List.nth (s |> String.split_on_char ' ') 1 in
                  if types_first 
                  then divide_lines inp before_prod acc_keep acc_prods ((start_id, s)::acc_starts) acc_types starts_first types_first
                  else divide_lines inp before_prod acc_keep acc_prods ((start_id, s)::acc_starts) acc_types true types_first)
            else 
              (* collect lines starting with '%type' *)
              if (starts "%type" s)
              then (let type_id = 
                      let ls = s |> String.split_on_char ' ' |> List.filter (fun x -> not (String.equal x "")) in 
                      let len = List.length ls in 
                      List.nth ls (len - 1) 
                    in
                    if starts_first 
                    then divide_lines inp before_prod acc_keep acc_prods acc_starts ((type_id, s)::acc_types) starts_first types_first
                    else divide_lines inp before_prod acc_keep acc_prods acc_starts ((type_id, s)::acc_types) starts_first true)
              else 
                (* productions starting from '%%' so mark before_prod 'false' *)
                if (starts "%%" s) 
                then divide_lines inp false acc_keep acc_prods acc_starts acc_types starts_first types_first
                else divide_lines inp before_prod (s::acc_keep) acc_prods acc_starts acc_types starts_first types_first
           end
      else divide_lines inp before_prod acc_keep (s::acc_prods) acc_starts acc_types starts_first types_first 
  in 
  let lines_bef_prods, lines_prods, start_id_lines, type_id_lines, (starts_fst, types_fst) = 
    divide_lines ic true [] [] [] [] false false
  in 
  let states_mapping = 
    let start_states_map: (state * state) list = [] in
     start_states_map @ sts_rename_map
  in 
  (* find corresponding lines wrt %start's *)
  let res_start_lines, after_starts_states_mapping = replace_wgstarts start_id_lines states_mapping
  in
  let triv_states_mapping = 
    g.triv_term_nonterm_list |> List.map (fun (_term, nt) -> (nt, nt)) |> remove_dups
  in 
  let res_states_mapping = 
    after_starts_states_mapping @ triv_states_mapping
  in
  let res_states_mapping_primary = 
    let mapped_prods_in_new_cfg st = 
      g.productions |> List.filter (fun (nt, _i, _sigls) -> nt = st) |> List.length
    in
    let rec remove_less_dominant_dup_state (ls: (state * state) list) (acc: (state * state) list) =
      match ls with [] -> List.rev acc 
      | (curr_st_old, curr_st_new) :: tl -> 
        let old_states = acc |> List.map fst in
        if (List.mem curr_st_old old_states)
        then 
          (let other_st_new = List.assoc curr_st_old acc in
            if (mapped_prods_in_new_cfg curr_st_new) > (mapped_prods_in_new_cfg other_st_new)
            then 
              (let acc_wo_other_pair = acc |> List.filter (fun (s, _) -> not (String.equal s curr_st_old)) in
               let acc_new = (curr_st_old, curr_st_new) :: acc_wo_other_pair in 
               remove_less_dominant_dup_state tl acc_new)
            else 
              remove_less_dominant_dup_state tl acc)
        else remove_less_dominant_dup_state tl ((curr_st_old, curr_st_new)::acc)
    in let res_states_mapping_prim_res = remove_less_dominant_dup_state after_starts_states_mapping []
    in res_states_mapping_prim_res 
    (* Sort so that states are ordered from the longer one to shorter one
     - This way, you can replace 'stmts' before replacing 'stmt' *)
    |> List.sort compare |> List.rev (* @ triv_states_mappingeeee *)
  in
  let res_states_mapping_starts: (state * state) list = 
    let start_states = g.starts in 
      if (List.length start_states) = 1
      then 
        begin 
          if (List.length init_states) = 1
          then ((List.hd init_states), (List.hd start_states)) :: []
          else raise Inconsistent_start_states
        end
      else 
        List.map2 (fun x1 x2 -> (x1, x2)) init_states start_states
  in
  (* find corresponding lines wrt %type's *)
  let res_type_lines = replace_wgtypes type_id_lines res_states_mapping 
  in
  let starts_types_lines = 
    match starts_fst, types_fst with 
    | true, false -> res_start_lines @ res_type_lines @ ["%%"]
    | false, true -> res_type_lines @ res_start_lines @ ["%%"]
    | true, true | false, false -> raise Either_starts_or_types_first
  in
  if debug_print then 
    (wrapped_printf "\n\t States mapping : \n\t"; Pp.pp_raw_states res_states_mapping;
    wrapped_printf "\n\t States mapping (primary) : \n\t"; Pp.pp_raw_states res_states_mapping_primary;
    wrapped_printf "\n\t States mapping (starts) : \n\t"; Pp.pp_raw_states res_states_mapping_starts);
  (* collection of start nonterms, triv nonterms for easy access *)
  let _start_nonterms, triv_nonterms = 
    let start_nts = 
      start_id_lines |> List.map fst |> List.map (fun nt -> 
        find_mapped_state_of nt res_states_mapping) in
    let triv_nts = 
      g.triv_term_nonterm_list |> List.map snd in 
      start_nts, triv_nts
  in
  (* Collect prod blocks from the original grammar (to keep the format) *)
  let collect_prod_blocks prods_ls = 
    let rec collect_prods_loop (ls: string list) (prod_id: string) (prod_lines: string list) 
    (prods_acc: (string * string list) list): (string * string list) list =
      match ls with 
      | [] -> 
        let old_prod_id_and_lines = (prod_id, List.rev prod_lines) 
        in List.rev (old_prod_id_and_lines::prods_acc)
      | shd :: stl -> 
        (* TODO: if line only contains ";" then ignore and continue to run the loop *)
        (* if line involves ":" (colon), then check if it's about triv productions and collect *)
        if (contains_only_one_colon shd) && (not (String.contains shd '}'))
        then 
          (* prod_id refers to nonterminal (by which productions are grouoped) *)
          (let new_prod_id = get_prod_nonterm shd in 
          let old_prod_id_prods = 
            let prod_lines_wo_empty = prod_lines |> List.rev |> List.filter (fun s -> (not (String.equal "" s))) 
            in (prod_id, prod_lines_wo_empty) 
          in
          collect_prods_loop stl new_prod_id (shd::[]) (old_prod_id_prods::prods_acc))
        else 
          if (contains_only_one_colon shd) && (String.contains shd '}')
          then 
            (let new_prod_id = get_prod_nonterm shd in
            let _new_prod = get_nonterm_prod shd in 
            let old_prod_id_prods = 
              let prod_lines_wo_empty = prod_lines |> List.rev |> List.filter (fun s -> (not (String.equal "" s))) 
              in (prod_id, prod_lines_wo_empty) 
            in
            collect_prods_loop stl new_prod_id (shd::[]) (old_prod_id_prods::prods_acc))
          else
            (* when (not (contains_only_one_colon shd)) -> not at the end of the line, so keep accumulating on 'prod_lines' *)
            if (String.equal shd "")
            then collect_prods_loop stl prod_id prod_lines prods_acc
            else collect_prods_loop stl prod_id (shd::prod_lines) prods_acc
    in collect_prods_loop prods_ls "" [] [] 
  in let prods_blocks = collect_prod_blocks lines_prods in 
  if debug_print then (wrapped_printf "\n >> Collected production blocks: \n"; 
    prods_blocks |> List.iter (fun (nt, prods) -> wrapped_printf "\t ** Nonterminal %s mapped to \n\n" nt; 
    prods |> List.iter (wrapped_printf "\t%s\n"); wrapped_printf "\n\n"));
  (* --- helper --- *)
  let replace_str_wrt_primary_mapped_state (s: string) (states_map: (state * state) list): string = 
    let rec replace_str_loop ls str_acc = 
      match ls with [] -> str_acc 
      | (old_st_hd, mapped_st_hd) :: tl -> 
        let old_st = Str.regexp old_st_hd in
        let new_acc = Str.global_replace old_st mapped_st_hd str_acc in
        replace_str_loop tl new_acc
    in replace_str_loop states_map s
  in
  (* Collect start and trivial nonterminals' productions *)
  let orig_start_nonterms = start_id_lines |> List.map fst 
  in if debug_print then 
    (wrapped_printf "\nOriginal start nonterminals: "; List.iter (fun s -> wrapped_printf  "\n\t %s" s) orig_start_nonterms; wrapped_printf "\n");
  let start_triv_prods: string list = 
    List.append 
    (prods_blocks |> List.fold_left (fun acc (nt, prods) -> 
      wrapped_printf "\n\t\t\tNonterminal is %s\n" nt;
      if (List.mem nt orig_start_nonterms) (* e.g., `program` *)
      then 
        (let new_prods = prods |> List.map (fun s -> 
          let replaced_str = 
            replace_str_wrt_primary_mapped_state s res_states_mapping_starts
          in if debug_print then (wrapped_printf "\n\t  Original string %s\n\t  Replaced string %s\n" s replaced_str); replaced_str) (* Str.global_replace old_st mapped_st s *)
        in acc@[""]@(new_prods))
      else if (List.mem nt triv_nonterms) then acc@[""]@(prods) else acc) [])
    [""]
  in
  if debug_print then (wrapped_printf "\n\t *** Start and trivial productions : \n"; 
    start_triv_prods |> List.iter (wrapped_printf "%s\n"));
  let nontriv_prods: (string * string list) list = 
    prods_blocks |> List.filter (fun (nt, _prods) -> 
      not (List.mem nt orig_start_nonterms) && not (List.mem nt triv_nonterms) && not (String.equal nt ""))
  in
  if debug_print then (wrapped_printf "\n\t *** Nontrivial productions : \n"; 
    nontriv_prods |> List.iter (fun (s, ss) -> wrapped_printf "%s\n" s; ss |> List.iter (wrapped_printf "\t%s\n")));
  (* Find out there is no exact 1-to-1 mapping *)
  let (inconsistent_states, consistent_states): state list * state list = 
    let all_states_cnt_tbl: (state, int) Hashtbl.t = 
      Hashtbl.create (List.length res_states_mapping) in
      res_states_mapping |> List.iter (fun (st_old, _) -> 
        match (Hashtbl.find_opt all_states_cnt_tbl st_old) with 
        | None -> Hashtbl.add all_states_cnt_tbl st_old 1
        | Some n -> Hashtbl.replace all_states_cnt_tbl st_old (n+1));
    let all_states = res_states_mapping |> List.map fst in
    let multiple_mapped_states = all_states 
      |> List.filter (fun st -> (Hashtbl.find all_states_cnt_tbl st) > 1) 
      |> remove_dups in 
    let single_mapped_states = all_states
      |> List.filter (fun st -> (Hashtbl.find all_states_cnt_tbl st) = 1)
      |> remove_dups in 
    multiple_mapped_states, single_mapped_states
  in
  if debug_print then (wrapped_printf "\n *** Multiple mapped states: \t "; 
    Pp.pp_states inconsistent_states; wrapped_printf "\n *** Single mapped states: \t";
    Pp.pp_states consistent_states);

  (*  --- New Helper to correclty replace nonterminals b/c it's not always replaced with primary state --- *)
  let before_curly_or_comment (sls: string list): string list = 
    let hit_curly, hit_comment = ref false, ref false in 
    sls |> List.fold_left (fun acc x -> 
      if (!hit_curly || !hit_comment) then acc else if (String.equal x "{") then (hit_curly := true; acc) else 
        if (String.equal x "/*") then (hit_comment := true; acc) else acc@[x] ) []
  in 
  let after_equal_sign (x: string): string = 
    if (String.contains x '=') 
    then 
      (let from = (String.index x '=') in 
       let len = String.length x in
       String.sub x (from+1) (len-from-1))
    else x
  in 
  let correct_str_lst (s: string): string list = 
    Str.split (Str.regexp "[ \n\r\x0c\t]+") s |> List.filter (fun x -> 
      not ((String.equal x "|") || (String.equal x ";"))) |> before_curly_or_comment 
      |> List.map (fun x -> after_equal_sign x)
  in 
  let extract_terminal (s: string): string = 
    let sls = correct_str_lst s in
    let res = 
      let res_ls = sls |> List.filter (fun s -> String.equal s (String.uppercase_ascii s)) in
      if List.is_empty res_ls then "" else List.hd res_ls 
    in 
      res
      (* [fix]
        if (String.equal res "LPAREN") then "LPARENRPAREN" else res 
      *)
  in
  let extract_nonterms (s: string): string list = 
    let sls = correct_str_lst s in
      sls |> List.filter (fun s -> String.equal s (String.lowercase_ascii s)) 
  in
  let extract_terminal_sigls (ls: sigma list): string = 
    let res = ls |> List.fold_left (fun acc sg -> match sg with T ter -> acc @ [ter] | Nt _ -> acc) [] in 
      if (List.is_empty res) then (wrapped_printf "\n\tExtract terminal sigls : empty!\n";"") else 
        (let fst_term = List.hd res in 
          (* if (String.equal fst_term "LPAREN") then "LPARENRPAREN" else fst_term *)
          fst_term 
          )
  in 
  let extract_nonterms_sigls (ls: sigma list): string list = 
    ls |> List.fold_left (fun acc sg -> match sg with Nt nt -> acc @ [nt] | T _ -> acc) []
  in 
  let find_nonterms_for (nt: string) (term: string) (old_nonts: string list): string list =
    let nt_p_lst: p list = g.productions |> List.filter (fun (nt', _i, _sigls) -> (String.equal nt nt')) in
        (* nt_p_lst |> List.iter (fun x -> Pp.pp_p x); *)
    let only_consist_of_one_trivnonterm (nonts: string list) = 
      if (List.is_empty nonts) then false else (List.mem (List.hd nonts) triv_nonterms)
    in
    if (List.length old_nonts) = 1 && (only_consist_of_one_trivnonterm old_nonts)
    then 
      (let res_nonterms = nt_p_lst |> List.map (fun (_nt, _i, sigls) -> sigls) |> List.map (fun sigls -> extract_nonterms_sigls sigls) 
       in if (List.mem old_nonts res_nonterms) then old_nonts else old_nonts 
        (* [new_fix] just resturn old_nonts even in triv nonterms case*)
        (* raise (Failure "find_nonterms_for : triv nonterms case") *)
        )
    else 
      (let sig_lst: sigma list = nt_p_lst |> List.fold_left (fun acc (_nt, _i, sigls) -> 
          let new_nonts = extract_nonterms_sigls sigls in
          if (String.equal (extract_terminal_sigls sigls) term) && ((List.length old_nonts) = (List.length new_nonts)) then acc @ sigls else acc) [] in
          extract_nonterms_sigls sig_lst)
  in 
  (* --- helper to set traversal direction --- *)
  let to_scan_forward (prev_nonts: string list) (new_nonts: string list): bool = 
    let rec traverse_both prevs news matched_covered = 
      match prevs with 
      | [] -> if (List.is_empty news) then true 
              else 
                (* false  *)
                raise (Failure "to_scan_forward: prev_nts and new_nts to have same length")
      | prev_nt_hd :: prev_nt_tl -> 
        let new_nt_matched = List.hd news in 
        (* 
        [Alternative]
        if (List.mem new_nt_matched matched_covered) then false 
        else traverse_both prev_nt_tl (List.tl news) (prev_nt_hd::matched_covered)
        *)
        if (List.mem prev_nt_hd matched_covered) then false 
        else traverse_both prev_nt_tl (List.tl news) (new_nt_matched::matched_covered)
    in traverse_both prev_nonts new_nonts [] 
  in
  let new_replace_str_wrt_mapped_states (nt: string) (ln: string): string = 
    let term = extract_terminal ln in 
    (* let term' = if (String.equal term "LPAREN") then "LPARENRPAREN" else term in  *)
    if debug_print then wrapped_printf "\n\t\t term %s " term;
    let nonts = extract_nonterms ln in 
    if debug_print then (wrapped_printf "\n\t\t old nonterms -->"; nonts |> List.iter (fun x -> wrapped_printf "%s " x); wrapped_printf "\n");
    let correct_nonts = find_nonterms_for nt term nonts in 
    if debug_print then (wrapped_printf "\n\t\t finding correct nonterms for Nonterm %s and Term %s -->" nt term; 
      correct_nonts |> List.iter (fun x -> wrapped_printf "%s " x); wrapped_printf "\n\n"); 
    if (String.equal term "") && (List.length nonts) = 1 && (List.length correct_nonts) = 1 
    then (let old_st = Str.regexp (List.hd nonts) in 
          Str.global_replace old_st (List.hd correct_nonts) ln)
    else 
      begin  
        let scan_forward = to_scan_forward nonts correct_nonts in 
        let (nonts_init, correct_nonts_init) = 
          if scan_forward 
          then (wrapped_printf "\n\t\t\t Scan Forward\n\n"; (nonts, correct_nonts)) 
          else (wrapped_printf "\n\t\t\t Scan Backward\n\n"; (List.rev nonts, List.rev correct_nonts)) 
        in 
        let rec replace_loop old_sts new_sts str_acc = 
          match old_sts with [] -> 
            if (List.is_empty new_sts) then str_acc else raise Nonterms_length_must_equal
          | old_st_hd :: old_sts_tl -> 
            let old_st = Str.regexp old_st_hd in 
            let new_st = List.hd new_sts in 
            let new_acc = 
              (if scan_forward 
               then Str.replace_first old_st new_st str_acc 
               else str_replace_last old_st_hd new_st str_acc) 
            in 
            wrapped_printf "\n\t --- replacing %s with %s \n" old_st_hd new_st;
            replace_loop old_sts_tl (List.tl new_sts) new_acc
            (* Tentative fix is to traverse in reverse direction. ref: G0a-000->0 scenario *)
        in replace_loop nonts_init correct_nonts_init ln
      end
  in 
  let no_terms_or_nonterms (ln: string): bool = 
    if debug_print then wrapped_printf "\n\t Does %s involve NO terms or nonterms?\n" ln;
    let res = 
      if (contains_only_one_colon ln) then true 
      else
        let t = extract_terminal ln in 
        let nts = extract_nonterms ln in 
        (String.equal t "") && (List.is_empty nts) 
    in if debug_print then (if res then wrapped_printf "\t\t YES\n" else wrapped_printf "\t\t NO\n"); res
  in 
  (* Keep the productions as they are except for replacing with right states names *)
  let unchanged_nontriv_prods: string list = 
  let nonterm = ref "" in 
    nontriv_prods 
    |> List.filter (fun (nt, _prods) -> (List.mem nt consistent_states)) 
    |> List.fold_left (fun acc (old_nt, prods) -> 
        let new_st = List.assoc old_nt res_states_mapping in
        (wrapped_printf "\n ** Which state?! %s\n" new_st);
        nonterm := new_st;
        let _old_st = Str.regexp old_nt in
        let new_prods = prods |> List.map (fun ln -> 
          (* [fixed] b/c not always replaced with primary state *)
          (* replace_str_wrt_primary_mapped_state ln res_states_mapping_primary *)
          if (String.ends_with ~suffix:":" ln) then (new_st ^ ":") else
          if (no_terms_or_nonterms ln) 
          then replace_str_wrt_primary_mapped_state ln res_states_mapping_primary
          else new_replace_str_wrt_mapped_states !nonterm ln
          ) in (new_prods |> List.iter (fun s -> wrapped_printf "\n\t  *** Production accumulated  %s" s));
          acc @ (new_prods@[""])
      ) []
  in
  (* Create mappings for correct formatting and mapping of the string *)
  let prods_in_question: string list = 
    nontriv_prods 
    (* |> List.filter (fun (nt, _prods) -> (List.mem nt inconsistent_states))  *)
    |> List.map snd |> List.map (fun prod_blk -> List.tl prod_blk)
    |> List.flatten
  in  
  let contains_terminal_string (s:string): bool = (* b/c terminals are capitalized *)
    let s_after_equal = (after_equal_sign s) in 
    let is_capitalized (s': string): bool = (String.equal s' (String.capitalize_ascii s')) 
    in (List.mem s_after_equal (g.terms)) || (is_capitalized s_after_equal)
  in 
  let extract_nonterminal (s:string): string = 
    (* --- helper to result in "" nonterminal for line e.g., "| x=VAR { Var (snd x) }" --- *)
    let terminal_after_equal (ln: string) (eq_idx: int): bool = 
      let str_after = Str.string_after ln (eq_idx + 1) in 
      if (contains_terminal_string str_after) then true else false
    in 
    if (String.contains s '=')
    then
      (let len = String.length s in 
      let equal_sign = Str.regexp "=" in
      let idx = Str.search_backward equal_sign s len in 
      if (terminal_after_equal s idx) then "" else Str.string_after s (idx + 1))
    else s
  in
  let collect_terms_and_nonterms (sls: string list): string list * string list = 
    let rec loop ls terms_acc nonterms_acc =
      match ls with [] -> List.rev terms_acc, List.rev nonterms_acc
      | str_hd :: str_tl -> 
        if ((String.equal str_hd "{") || (String.equal str_hd "/*"))
        then List.rev terms_acc, List.rev nonterms_acc
        else if (contains_terminal_string str_hd)
          then (let terminal_strhd = extract_terminal str_hd
                in loop str_tl (terminal_strhd::terms_acc) nonterms_acc)
          else (let nonterm_shd = extract_nonterminal str_hd in 
                if (String.equal nonterm_shd "") then loop str_tl terms_acc nonterms_acc
                else loop str_tl terms_acc (nonterm_shd::nonterms_acc))
    in loop sls [] []
  in
  (* Use 2 different mappings for correctly formatting the changed nontrivial nonterminals transitions *)
  let rec prods_map_loop (ls: string list) (acc: ((string list * int) * (string * string list)) list): 
    ((string list * int) * (string * string list)) list = 
    match ls with [] -> List.rev acc
    | curr_prod :: stl -> 
      let sls = String.split_on_char ' ' curr_prod in
      let (sterms, snonterms): string list * string list = sls |> List.filter (fun s -> 
        (not (String.equal "|" s) && (not (String.equal "" s)))) 
        |> collect_terms_and_nonterms
      in prods_map_loop stl (((sterms, (List.length snonterms)), (curr_prod, snonterms))::acc)
  in 
  let nontriv_prods_terms_ntnum_mapping = prods_map_loop prods_in_question [] in
  if debug_print then 
    (wrapped_printf "\n*** Nontrivial prods mapping (terms, nonterms_num) -> prod \n"; Pp.pp_prods_mapping nontriv_prods_terms_ntnum_mapping);
  
  let collect_terminals_from_sigls (sls: sigma list): string list = 
    sls |> List.fold_left (fun acc s -> match s with T term -> (term::acc) | Nt _ -> acc) [] |> List.rev
  in
  let collect_nonterminals_from_sigls (sls: sigma list): string list = 
    sls |> List.fold_left (fun acc s -> match s with Nt nt -> (nt::acc) | T _ -> acc) [] |> List.rev
  in
  (* collect production list in blocks *)
  let collect_blocks (lst: p list): ((string list * string list * p) list) list =
    let rec blocks_loop ls curr_nont acc_prods (acc_res: ((string list * string list * p) list) list) =
      match ls with [] -> List.rev (acc_prods :: acc_res)
      | (nont, _, sig_ls) as prod_h :: prods_tl -> 
        let terms = collect_terminals_from_sigls sig_ls in
        let nonterms = collect_nonterminals_from_sigls sig_ls in
        if (curr_nont = "") 
        then blocks_loop prods_tl nont ((terms, nonterms, prod_h) :: acc_prods) acc_res
        else if (curr_nont = nont) 
        then blocks_loop prods_tl curr_nont ((terms, nonterms, prod_h) :: acc_prods) acc_res
        else
          (* if not (curr_not = nont), change 'curr_nont' to 'nont' and pass 'acc_prods' to 'acc_res' *)
          let block = List.rev acc_prods in blocks_loop prods_tl nont ((terms, nonterms, prod_h) :: []) (block :: acc_res)
    in blocks_loop lst "" [] []
  in
  let genereated_multiple_mapped_states = 
    sts_rename_map |> List.filter (fun (st_old, _st_new) -> 
      (List.mem st_old inconsistent_states)) |> List.map snd
  in
  (* prods_blocks : (nonterm, (terminal list, nonterminal list, production) list) list *)
  let inconsistent_nonterm_prods_blocks: (nt * (string list * string list * p) list) list = 
    g.productions |> collect_blocks |> List.map (fun terms_nonterms_ps -> 
      let (_ts, _nts, (fst_nt, _i, _sigls)) = List.hd terms_nonterms_ps in (fst_nt, terms_nonterms_ps)) 
    |> List.filter (fun (nt, _) -> (List.mem nt genereated_multiple_mapped_states))
  in 
  if debug_print then (wrapped_printf "\n  >> Collected blocks:\n\n"; 
    inconsistent_nonterm_prods_blocks |> List.iter (fun (nt, ts_nts_p_ls) -> 
        wrapped_printf "\t Nonterm %s -> \n" nt; ts_nts_p_ls |> List.iter (fun (ts, nts, p) -> 
        wrapped_printf "\n\t\t Terminals  < "; ts |> List.iter (wrapped_printf "%s "); wrapped_printf "> ";
        wrapped_printf " Nonterminals < "; nts |> List.iter (wrapped_printf "%s "); wrapped_printf ">   ==>  "; 
        Pp.pp_p p; wrapped_printf "\n")); wrapped_printf "\n");

  
  let find_assoc_all_new (terms: string list) (num_nonterms: int) (prods_mapping: ((string list * int) * (string * string list)) list) 
    (triv_nonterms: string list) (debug: bool): (string list * string) list = 
    wrapped_printf "\n\t\tTRIV nonterms are? "; triv_nonterms |> List.iter (fun x -> wrapped_printf "%s " x);
    let rec find_all_loop prods_ls (acc: (string list * string) list) = 
      match prods_ls with [] -> List.rev acc
      | ((hd_terms, hd_nt_num), (hd_prod, hd_nts)) :: prods_tl -> 
        if (num_nonterms = hd_nt_num) && (string_lists_equal hd_terms terms) 
        then let nt' = extract_nonterminal hd_prod in
          if nt' = "" then find_all_loop prods_tl ((hd_nts, hd_prod)::acc) else
          (if (List.mem nt' triv_nonterms) then find_all_loop prods_tl acc else find_all_loop prods_tl ((hd_nts, hd_prod)::acc))
        else find_all_loop prods_tl acc 
    in let nts_prod_ls = find_all_loop prods_mapping [] in
    let new_nts_prod_ls = nts_prod_ls |> List.filter (fun (nts_ls, _prod) -> 
      if (not flag.onoff_opt) then true else 
      if (List.length nts_ls) = 1 then 
      (let only_nonterm = List.hd nts_ls in not (List.mem only_nonterm triv_nonterms)) else true ) in
    if debug then (wrapped_printf "\n\t\t Mapped (nts, prod) list -> \n"; 
    new_nts_prod_ls |> List.iter (fun (nts, prod) -> nts |> List.iter (fun x -> wrapped_printf " %s" x); 
      wrapped_printf "\n\t\t mapped to production: %s \n" prod)); new_nts_prod_ls in

  (* --- helper to find (prod string, nonterms) from 'nontriv_prods_terms_ntnum_mapping' --- *)
  let find_nontriv_prod_from_prods_mapping (terms: string list) (new_nts: string list) (nt_num: int): 
    string * string list = 
    (* --- helper of this helper --- created for G2 scenario *)
    let rec triv_nontriv_nonterms_match (nts1: string list) (nts2: string list): bool = 
      match nts1, nts2 with 
      | [], [] -> true 
      | nts1_hd :: nts1_tl, nts2_hd :: nts2_tl -> 
        if (List.mem nts1_hd triv_nonterms) 
        then (if (List.mem nts2_hd triv_nonterms) then triv_nontriv_nonterms_match nts1_tl nts2_tl else false)
        else (if (List.mem nts2_hd triv_nonterms) then false else triv_nontriv_nonterms_match nts1_tl nts2_tl)
      | _, [] | [], _ -> raise (Failure "triv_nontriv_nonterms_match - lengths do not match")
    in
    if debug_print then (wrapped_printf "\n\t Find nontriv production from prods mapping from terminal list "; 
      terms |> Pp.pp_terminals; wrapped_printf "\n\t AND nt_num %d \n" nt_num);
      match List.assoc_opt (terms, nt_num) nontriv_prods_terms_ntnum_mapping with 
      | Some (p, nts) -> 
        let nts_prods_mapped: (string list * string) list = 
          find_assoc_all_new terms nt_num nontriv_prods_terms_ntnum_mapping triv_nonterms debug_print 
        in
          (* [new_fix] tentative fix to account for ident & const happening for nontriv_state-starting trans *)
          if (List.length nts_prods_mapped = 1) || (List.length nts_prods_mapped = 2) then begin 
            let p' = if (triv_nontriv_nonterms_match nts new_nts) then p else "" in
              if debug_print then (wrapped_printf "\n\t FOUND production %s \n" p; 
              wrapped_printf " with nonterms "; nts |> Pp.pp_nonterminals); 
              if (String.equal p' "") then ("", []) else (p, nts) end 
          else 
            begin
              if List.length nts_prods_mapped >= 2 then wrapped_printf "\n\t\tOVER HERE ~ !!\n\n";
              if (List.is_empty terms) && (nt_num = 1) && flag.onoff_opt then ("", []) else
            let raw_nts_p' = nts_prods_mapped |> List.filter (fun (nts, _prod) -> 
              string_lists_equal nts new_nts) in 
            let p' = if (List.is_empty raw_nts_p') then p else raw_nts_p' |> List.hd |> snd in 
            (p', nts) end
      | None -> 
        if debug_print then (wrapped_printf "\n\t NOT FOUND so look for prod elsewhere \n"); ("", [])
  in
  let change_str_per_nts (old_nts: string list) (new_nts : string list) (ln: string) (scan_forward: bool): string =
    if (List.length old_nts) != (List.length new_nts) then raise Nonterms_length_must_equal;
    let rec loop old_sts new_sts (str_acc: string) =
      match old_sts with [] -> str_acc
      | old_st_hd :: old_sts_tl ->
        let old_st = Str.regexp old_st_hd in
        let new_st = List.hd new_sts in 
        let new_acc = 
          (if scan_forward 
          then Str.replace_first old_st new_st str_acc
          else str_replace_last old_st_hd new_st str_acc)in
        loop old_sts_tl (List.tl new_sts) new_acc
    in loop old_nts new_nts ln
  in
  let create_epsilon_prod_rhs (rhs: sigma list): string = 
    if (List.length rhs) != 1 then raise Not_possible;
    let elem = List.hd rhs in
    match elem with 
    | Nt r  -> (sprintf "  | %s { $1 }" r)
    | T x -> (wrapped_printf "\n\t\t\t Terminal %s" x); raise (Failure "Create epsilon prod rhs - terminal")
  in
  let changed_nonterm_prods: string list = 
    inconsistent_nonterm_prods_blocks |> List.fold_left (fun acc (nt, ts_nts_p_ls) -> 
      let new_prods = ts_nts_p_ls |> List.fold_left (fun acc (ts, new_nts, prod) -> 
        if debug_print then (wrapped_printf "\nNow looking at prod starting from %s \n\t\t " nt; Pp.pp_p prod);
        let new_nts' = new_nts |> List.filter (fun nt -> not (String.equal nt epsilon_state)) in
        wrapped_printf "\n\t New Nonts':\n\t\t"; new_nts' |> List.iter (fun x -> wrapped_printf " %s" x); wrapped_printf "\n";
        let old_prod_line, old_nts_ls = find_nontriv_prod_from_prods_mapping ts new_nts' (List.length new_nts') in
        if (String.equal "" old_prod_line) && (List.is_empty old_nts_ls) 
        then 
          (let new_str = match prod with (_nt, _i, sig_ls) -> create_epsilon_prod_rhs sig_ls
           in new_str :: acc) 
        else 
          let scan_forward' = to_scan_forward old_nts_ls new_nts' in
          let (old_nonts_init', correct_nonts_init') = 
            if scan_forward' 
            then (wrapped_printf "\n\t\t\t Scan Forward\n\n"; (old_nts_ls, new_nts')) 
            else (wrapped_printf "\n\t\t\t Scan Backward\n\n"; (List.rev old_nts_ls, List.rev new_nts')) 
          in
          (* 
          (let changed_str = change_str_per_nts old_nts_ls new_nts' old_prod_line 
           in changed_str :: acc)
           *)
          (let changed_str = change_str_per_nts old_nonts_init' correct_nonts_init' old_prod_line scan_forward'
           in changed_str :: acc)) [] 
      in 
        (nt ^ ":") :: new_prods @ ["  ;";""] @ acc) []
  in
  let oc = open_out to_write in (* parser_file *)
  lines_bef_prods @ starts_types_lines @ start_triv_prods @ unchanged_nontriv_prods @ changed_nonterm_prods
  |> List.iter (fun ln -> fprintf oc "%s\n" ln);
  close_out oc

(** convertToGrammar : *)
let convertToGrammar (ta_inp: ta2) (states_rename_map: (state * state) list) 
  (init_states: state list) (file: string) (to_write: string) (flag:optimization) (debug: bool) =
  ta_inp |> ta_to_cfg debug |> cfg_to_parser file states_rename_map init_states debug to_write flag

(* Below: currently not taken into consideration  *)
(* ******************** Specify associativity > parser.mly ******************** *)

(** specify_associativity : specify associativity on parser file per user input (0, 1, 2) *)
let specify_associativity (parser_file: string) (ind: int) (trees: tree * tree) (debug_print: bool): unit =
  let wrapped_printf fmt =
    if debug_print then Printf.printf fmt
    else Printf.ifprintf stdout fmt
  in

  let assoc = match ind with 0 -> "%right" | 1 -> "%left" (* | 2 -> "%nonassoc" *)
    | _ -> raise (Failure "Incorrect input number for associativity") in
  let op: string = match node_symbol (fst trees) with 
    | "*" -> "MUL" | "+" -> "PLUS" | s -> s in
  let line_to_add: string = assoc ^ " " ^ op in
  let open Printf in if debug_print then
    wrapped_printf "\nWrite associativity %s of %s on parser file %s\n" assoc op parser_file;
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
 *)
