open Cfg
open Ta
open Utils
open Treeutils

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
  let starts = clean $ Hashtbl.find sectionToLine "s" in
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
        else 
          (if debug_print then Printf.printf "\nWhat is this? %s\n" x;
          raise (Failure "RHS contains unknown symbols"))) rhs)
    ) t_prods
  in
  let nonterms = !nonterms in
  if debug_print then
    Printf.printf "CFG extracted from %s:\n" filename;
    Printf.printf "Start nonterminals: %s\n" (String.concat " " starts);
    Printf.printf "Nonterminals: %s\n" (String.concat " " nonterms);
    Printf.printf "Terminals: %s\n" (String.concat " " terms);
    Printf.printf "Productions:\n";
    List.iter (fun (lhs, i, rhs) -> Printf.printf "%d: %s -> %s\n" i lhs (String.concat " " (List.map (function T x -> x | Nt x -> x) rhs))) productions;
  { nonterms; terms; starts; productions; triv_term_nonterm_list = [] }

let cfg3_of_cfg2 (cfg2: cfg2): cfg3 =
  { 
    nonterms = cfg2.nonterms; 
    terms = cfg2.terms; 
    starts = cfg2.starts; 
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
          -> (("LPAREN", len), l)
        | (Some (T x), l) -> ((x, len), l)
        | (None, l) -> (("ε", len), l)
        | _ -> assert false
        in
        let proj = List.fold_right (fun a acc -> match a with
          T _ -> acc | Nt x -> x :: acc) rhs' []
        in
        (lhs, (first_t, proj), rhs)
      );
      triv_term_nonterm_list = cfg2.triv_term_nonterm_list;
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
  in h g.nonterms g.starts g.productions level

let cfg_to_ta (opt: optimization) (debug_print: bool) (g: cfg3): 
  ta2 * restriction list * ((symbol * int) * sigma list) list * 
  ((int, symbol list) Hashtbl.t) * (symbol * state) list * symbol list =
  let open List in
  let open Printf in
  let triv_opt = opt.triv_opt in
  let (nonterms, starts, prods) = optimize_cfg_starts g 2 in
  if debug_print then 
    (printf "\n\t Extracted following nonterminals\n\t"; nonterms |> Pp.pp_nonterminals; printf "\n";
    printf "\n\t Extracted following prods\n\t"; prods |> Pp.pp_productions2);
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
    (* *** debugging (TODO) Below logic of updating 'nt_to_order' can be improved *** *)
    iter (fun st -> Hashtbl.replace !nt_to_order st 0) init_sts;
    (* 
    Hashtbl.add !nt_to_order "ϵ" (num_nt + 1); (* pseudo nt *)
     *)
    Hashtbl.iter (fun s ord -> printf "\n\t\t Nonterm %s Order %i" s ord) !nt_to_order;
    let get_order table =
      fold_left (fun acc (st, (_, rhs), _) ->
          let changed = ref false in
          iter 
            (fun s ->
                (* *** debugging *** *)
                (* if debug_print then printf "\n\t *** Getting order for nonterminal %s" s; *)
                if (String.equal s "ϵ") then 
                  (* Temporary fix *)
                  (Hashtbl.replace !table s (-1);
                  changed := false)
                else 
                  (let ord = Hashtbl.find !table s in
                  (* printf "\n\t  For %s found ord %d \n" s ord; *)
                  let ord' = Hashtbl.find !table st in
                  (* printf "\n\t  For %s found ord' %d \n" st ord'; *)
                  if ord > (ord' + 1) then (
                    (* printf "\n\t  !! ord %d > ord' %d + 1\n" ord ord'; *)
                    Hashtbl.replace !table s (ord' + 1);
                    changed := true))
            ) rhs;
          acc || !changed
        )
        false
        trans_ls
    in
    let nt_to_order' = fixpoint get_order nt_to_order in
    (* *** debugging *** *)
    Hashtbl.iter (fun s ord -> printf "\n\t\t  Nonterm %s Order %i" s ord) !nt_to_order';
    let states_ordered: (state * int) list = Hashtbl.fold
      (fun st o acc -> (st, o) :: acc) 
      !nt_to_order' [] 
    in
    (runIf debug_print (fun _ -> 
      printf "\nOrder of states : \n";
      iter
        (fun (st, lvl) -> printf "(%s, %i)\n" st lvl) 
        states_ordered) (* fixed based on 'fix_sts_order' *)
    );
    (* *** [Tentative fix] to optimize the process later *** *)
    (* *** not very efficient so can do better *** *)
    let state_order st st_ord_ls = 
      match List.assoc_opt st st_ord_ls with Some o -> o | None -> raise Not_possible
    in 
    let replace_st_and_upper st_ord_ls covered rhs_st ord = 
      st_ord_ls |> List.map (fun (s, o) -> 
        if (List.mem s covered) then (s, o) else
        if (String.equal s rhs_st) then (s, (ord+1)) else 
        (* --- [fix] --- if (o >= ord) then (s, (o+1)) else  *)
          (s, o))
    in 
    let one_nonterminal_list (sig_ls: sigma list): bool = 
      if (List.length sig_ls) = 1
      then 
        (match (List.hd sig_ls) with 
        | T _ -> false
        | Nt _ -> true)
      else false
    in
    let rec fix_sts_order (ls: production2 list) sts_orders covered =
      match ls with [] -> sts_orders
      | (lhs_st, ((ter, rnk), rhs_ntls), rhs_sigls) :: prods_tl -> 
        if (String.equal ter (fst epsilon_symb)) && (rnk = 1) && (one_nonterminal_list rhs_sigls)
        then
          begin 
            if (List.mem lhs_st init_sts)
            then fix_sts_order prods_tl sts_orders covered
            else 
              (let lhs_ord = state_order lhs_st sts_orders in 
              let rhs_st = List.hd rhs_ntls in
              printf "\n\n\t\t LHS state %s order %d ->_{eps_symb} RHS state %s \n" lhs_st lhs_ord rhs_st;
              (* --- [fix] --- *)
              (let covered' = lhs_st::covered in
               let new_sts_orders = replace_st_and_upper sts_orders covered' rhs_st lhs_ord in 
                printf "\n\t Intermediate States orders : \n\t"; 
                new_sts_orders |> List.iter (fun (st, lvl) -> printf " (%s, %i) " st lvl);
                fix_sts_order prods_tl new_sts_orders (lhs_st::rhs_st::covered)))
          end
        else 
          fix_sts_order prods_tl sts_orders covered 
    in let new_states_ordered = fix_sts_order trans_ls states_ordered [] in 
    (runIf debug_print (fun _ -> 
      printf "\nNew Order of states : \n";
      iter
        (fun (st, lvl) -> printf "(%s, %i)\n" st lvl) 
        new_states_ordered)
    );

    let rec get_o_base_precedence trans acc_res =
      match trans with
      | [] -> List.rev acc_res
      | (lhs_st, (sym, _), rhs) :: tl ->
        let ord = match (assoc_opt lhs_st new_states_ordered) with (* fixed based on 'fix_sts_order' *)
          | None ->
            (runIf debug_print (fun _ -> 
              printf "\n\nState %s has no matching order.\n" lhs_st));
            raise State_with_no_matching_order
          | Some o -> o
        in
        get_o_base_precedence tl ((Prec (sym, ord), (lhs_st, rhs))::acc_res)
    in get_o_base_precedence trans_ls []
  in
  let restrictions : ((restriction * (nonterminal * sigma list)) list) = trans_to_restrictions
    prods nonterms starts 
  in 
  (* trivial nts are nts with only zero arity productions *)
  let trivial_nts : state list = g.nonterms
    |> filter (fun nt ->
      filter (fun (lhs, _, _) -> lhs = nt) g.productions
      |> for_all (fun (_, ((_, a), _), _) -> a = 1) 
    ) |> filter (fun x -> (not (String.equal epsilon_state x)))
  in
  (* --- helper to identify level-changing eps-trasition --- *)
  let level_changing_eps_transition (s: symbol) (sigls: sigma list): bool = 
    if (syms_equals s epsilon_symb) && (List.length sigls) = 1
    then (let sig_elem = List.hd sigls in 
          match sig_elem with T _ -> false | Nt st' -> not (List.mem st' trivial_nts) && not (String.equal st' epsilon_state))
    else false
  in
  let restrictions_new = 
    if triv_opt then 
      restrictions |> List.filter (fun (r, (_lhs_nt, sigls)) -> 
        match r with Prec (sym, _o) -> not (level_changing_eps_transition sym sigls)
        | Assoc _ -> false)
    else restrictions
  in

  (* *** debug *** *)
  if debug_print then printf "\n\t *** Check restrictions (modified) \n";Pp.pp_restriction'_lst restrictions;
  
  let _find_symbol_from_productions (nt: nonterminal) (p: production2 list): symbol = 
    let rec loop prods =
      match prods with [] -> raise Trivial_symbols_not_found_in_prods
      | (n, ((term, i), _), _) :: tl -> 
        if nt = n then (term, i)
        else loop tl
    in loop p
  in 
  let find_nonterm_from_prods (sym: symbol) (p: production2 list): nonterminal = 
    let rec loop prods = 
      match prods with [] -> raise Trivial_symbols_not_found_in_prods
      | (nt, ((term, i), _), _) :: tl -> 
        if (Ta.syms_equals sym (term, i)) then nt 
        else loop tl
    in loop p
  in
  let raw_trivial_syms_nts : (symbol * nonterminal) list = 
    (* new version: to take all the trivial symbols into account *)
    ranked_alphabet |> List.filter (fun a -> snd a = 1) |> List.map (fun sym -> 
      (sym, (find_nonterm_from_prods sym g.productions))
      ) 
  in 
  let only_consists_of_trivial_trans (sym: symbol) (nt: nonterminal) (raw_triv_syms: symbol list) 
    (p: production2 list): bool = 
    let rec loop prods acc = 
      match prods with [] -> acc 
      | (lhs_n, ((term, i), _rhs_nts), _rhs_sigs) :: tl -> 
        if (lhs_n = nt) 
        then 
          (if (syms_equals (term, i) sym) || (List.mem (term, i) raw_triv_syms)
           then loop tl (true && acc)
           else false)
        else loop tl acc
    in loop p true
  in 
  let trivial_syms_nts : (symbol * nonterminal) list = 
    let init_triv_syms = raw_trivial_syms_nts |> List.map fst 
    (* Debugging in progress! *)
    in (printf "\n\t *** HERE! ***\n\n"; raw_trivial_syms_nts |> Pp.pp_sym_nts_ls);
        raw_trivial_syms_nts |> List.filter (fun (sym, nt) -> 
        only_consists_of_trivial_trans sym nt init_triv_syms g.productions)
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
    (iter (fun (prc, (lhs, rhss)) ->
      let s, o = match prc with
      | Prec x -> x
      | _ -> assert false
      in
      (* Only add non-trivial symbols to o_bp_tbl *)
      match s with (_, rnk) -> 
          begin 
            if ((rnk != 0) || not triv_opt) (*** [fix] for G0a INT triv trans case ***)
            then begin 
              (* If key already exists, then simply add to existing ones *)
              let exist_val = Hashtbl.find_opt o_bp_tbl o in
              printf "\n\nWhich symbol?? "; Pp.pp_symbol s; printf"\n\n";
              match exist_val with None -> add o_bp_tbl o s 
              | Some ls -> 
                (* *** debugging *** *)
                (if debug_print then printf "\n\t   For order %i" o;
                printf "\n\t *** already exist sym_lst so add this symbol to symbol list "; 
                ls |> List.iter Pp.pp_symbol; printf "\n";
                Hashtbl.replace o_bp_tbl o (s::ls))
            end;
            (if debug_print then printf "\n\t ****** Add (State %s, " lhs; Pp.pp_symbol s;
            printf ") ---> RHS list "; rhss |> List.iter Pp.pp_sigma);
          let exist_in_trantbl = Hashtbl.find_opt transitions_tbl (lhs, s) in
          let triv_syms = trivial_syms_nts |> List.map fst 
          in match exist_in_trantbl with 
              | None -> if (List.mem s triv_syms) 
                        then add transitions_tbl (lhs, s) [(T (fst s))] (* [prev] (Nt epsilon_state) *)
                        else add transitions_tbl (lhs, s) rhss
              | Some ls' -> if (List.mem s triv_syms)
                            then Hashtbl.replace transitions_tbl (lhs, s) ([(T (fst s))]::ls') (* [prev] (Nt epsilon_state) *)
                            else Hashtbl.replace transitions_tbl (lhs, s) (rhss::ls')
          end
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
  in
  (* *** debug *** *)
  printf "\n >> Trivial non-terminals: [ ";
  trivial_syms_nts |> iter (fun (s, x) -> printf " ("; Pp.pp_symbol s; printf ", %s ) " x); printf "]\n";
  printf "\n >> Restrictions restriction'_lst O_bp obtained from the TA_g : \n"; Pp.pp_restriction'_lst restrictions_new;
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
        (* --- added not to account for eps-trans to next-level state (excluding eps-trans to trivial state) --- *)
        (* if (level_changing_eps_transition sym sig_ls) then acc
        else  *)
          (let to_acc = ((sym, o), sig_ls) in to_sym_ord_rhs_lst tl (to_acc::acc))
      | Assoc _ -> to_sym_ord_rhs_lst tl acc
    end 
  in
  let sym_ord_to_rhs_lst : ((symbol * int) * sigma list) list = to_sym_ord_rhs_lst restrictions_new []
  (* 
  List.map (fun (r, (_lhs, sig_ls)) -> 
      match r with Prec (s, o) -> ((s, o), sig_ls)) |> List.filter (fun ((s, o), _) -> 
        List.fold_left (fun acc tsym -> acc && not (syms_equals s tsym)) true trivial_syms
        | Assoc (s, a) -> )  *)
  in 
  ta_res, restrictions_without_trivials, sym_ord_to_rhs_lst, o_bp_tbl, trivial_syms_nts, trivial_syms

let convertToTa (file: string) (opt: optimization) (debug_print: bool):
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
  cfg_to_ta opt debug_print)


(* ******************** Part II. Conversion of TA > CFG > parser.mly ******************** *)

(** undo_enhancement : helper to undo enhancement done on symbols *)
let undo_enhancement (a: ta): ta =
  let undo_change_symbol s =
    let s', s'' = fst s, snd s in if (s' = "*") then "MUL", s'' else
    if (s' = "+") then "PLUS", s'' else s in
  let alph_updated = a.alphabet |> List.map (fun sym -> undo_change_symbol sym) in
  let trans_updated = a.transitions |> List.map (fun (st, (sym, st_ls)) ->
    let sym_new = undo_change_symbol sym in (st, (sym_new, st_ls))) in 
  { states = a.states ; alphabet = alph_updated
  ; start_state = a.start_state ; transitions = trans_updated ; trivial_sym_nts = a.trivial_sym_nts }

let ta_to_cfg (debug_print: bool) (a: ta2): cfg2 = 
  let open Printf in
  if debug_print then printf "\nConvert TA to its corresponding CFG:\n\n  Input TA:\n"; Pp.pp_ta2 a;
  (* helpers *)
  let remove_dups ls =
  let unique_cons elem ls = if (List.mem elem ls) then ls else elem :: ls in
    List.fold_right unique_cons ls [] in
  let nonterms_excl_eps: nonterminal list = a.states |> List.filter (fun x -> not (x = "ϵ")) in
  let unranked_terminals: terminal list = a.alphabet |> List.filter (fun s -> not (sym_equals s "ε"))
    |> List.fold_left (fun acc (name, _rnk) -> match name with
    | "LBRACERBRACE" -> acc @ ["LBRACE"; "RBRACE"]
    | "LBRACKETRBRACKET" -> acc @ ["LBRACKET"; "RBRACKET"]
    | s -> acc @ [s] ) [] |> remove_dups in
  let prods: p list = 
    Hashtbl.fold (fun (st, _sym) sig_lsls acc -> 
      (* printf "\n\t For state %s and symbol " st; Pp.pp_symbol sym; printf "\n"; *)
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
  if debug_print then (printf "\nCFG (cfg2) resulted from the TA : \n"; Pp.pp_cfg2 cfg_res);
  cfg_res


(** cfg_to_parser : once convert to grammar, write it on the parser.mly file *)
let cfg_to_parser (parser_file: string) (sts_rename_map: (state * state) list) 
  (init_states: state list) (debug_print: bool) (to_write: string) (flag:optimization) (g: cfg2) : unit =
  let open Printf in
  printf "\nWrite the grammar on parser file %s\n" parser_file;
  if debug_print then (printf "\n  Input grammar:\n"; Pp.pp_cfg2 g);
  (* --- helpers --- *)
  let append_strs ls = ls |> List.fold_left (fun acc x -> 
    if (acc = "") then x else acc ^ " " ^ x) "" 
  in
  let find_mapped_state_of (st: state) (sts_map: (state * state) list) = 
    if debug_print then printf "\n\t Looking for mapped state of State %s\n" st;
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
    (printf "\n\t States mapping : \n\t"; Pp.pp_raw_states res_states_mapping;
    printf "\n\t States mapping (primary) : \n\t"; Pp.pp_raw_states res_states_mapping_primary;
    printf "\n\t States mapping (starts) : \n\t"; Pp.pp_raw_states res_states_mapping_starts);
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
  if debug_print then (printf "\n >> Collected production blocks: \n"; 
    prods_blocks |> List.iter (fun (nt, prods) -> printf "\t ** Nonterminal %s mapped to \n\n" nt; 
    prods |> List.iter (printf "\t%s\n"); printf "\n\n"));
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
    (printf "\nOriginal start nonterminals: "; List.iter (fun s -> printf  "\n\t %s" s) orig_start_nonterms; printf "\n");
  let start_triv_prods: string list = 
    List.append 
    (prods_blocks |> List.fold_left (fun acc (nt, prods) -> 
      printf "\n\t\t\tNonterminal is %s\n" nt;
      if (List.mem nt orig_start_nonterms) (* e.g., `program` *)
      then 
        (let new_prods = prods |> List.map (fun s -> 
          let replaced_str = 
            replace_str_wrt_primary_mapped_state s res_states_mapping_starts
          in if debug_print then (printf "\n\t  Original string %s\n\t  Replaced string %s\n" s replaced_str); replaced_str) (* Str.global_replace old_st mapped_st s *)
        in acc@[""]@(new_prods))
      else if (List.mem nt triv_nonterms) then acc@[""]@(prods) else acc) [])
    [""]
  in
  if debug_print then (printf "\n\t *** Start and trivial productions : \n"; 
    start_triv_prods |> List.iter (printf "%s\n"));
  let nontriv_prods: (string * string list) list = 
    prods_blocks |> List.filter (fun (nt, _prods) -> 
      not (List.mem nt orig_start_nonterms) && not (List.mem nt triv_nonterms) && not (String.equal nt ""))
  in
  if debug_print then (printf "\n\t *** Nontrivial productions : \n"; 
    nontriv_prods |> List.iter (fun (s, ss) -> printf "%s\n" s; ss |> List.iter (printf "\t%s\n")));
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
  if debug_print then (printf "\n *** Multiple mapped states: \t "; 
    Pp.pp_states inconsistent_states; printf "\n *** Single mapped states: \t";
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
      if (List.is_empty res) then (printf "\n\tExtract terminal sigls : empty!\n";"") else 
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
    if debug_print then printf "\n\t\t term %s " term;
    let nonts = extract_nonterms ln in 
    if debug_print then (printf "\n\t\t old nonterms -->"; nonts |> List.iter (fun x -> printf "%s " x); printf "\n");
    let correct_nonts = find_nonterms_for nt term nonts in 
    if debug_print then (printf "\n\t\t finding correct nonterms for Nonterm %s and Term %s -->" nt term; 
      correct_nonts |> List.iter (fun x -> printf "%s " x); printf "\n\n"); 
    if (String.equal term "") && (List.length nonts) = 1 && (List.length correct_nonts) = 1 
    then (let old_st = Str.regexp (List.hd nonts) in 
          Str.global_replace old_st (List.hd correct_nonts) ln)
    else 
      begin  
        let scan_forward = to_scan_forward nonts correct_nonts in 
        let (nonts_init, correct_nonts_init) = 
          if scan_forward 
          then (printf "\n\t\t\t Scan Forward\n\n"; (nonts, correct_nonts)) 
          else (printf "\n\t\t\t Scan Backward\n\n"; (List.rev nonts, List.rev correct_nonts)) 
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
            printf "\n\t --- replacing %s with %s \n" old_st_hd new_st;
            replace_loop old_sts_tl (List.tl new_sts) new_acc
            (* Tentative fix is to traverse in reverse direction. ref: G0a-000->0 scenario *)
        in replace_loop nonts_init correct_nonts_init ln
      end
  in 
  let no_terms_or_nonterms (ln: string): bool = 
    if debug_print then printf "\n\t Does %s involve NO terms or nonterms?\n" ln;
    let res = 
      if (contains_only_one_colon ln) then true 
      else
        let t = extract_terminal ln in 
        let nts = extract_nonterms ln in 
        (String.equal t "") && (List.is_empty nts) 
    in if debug_print then (if res then printf "\t\t YES\n" else printf "\t\t NO\n"); res
  in 
  (* Keep the productions as they are except for replacing with right states names *)
  let unchanged_nontriv_prods: string list = 
  let nonterm = ref "" in 
    nontriv_prods 
    |> List.filter (fun (nt, _prods) -> (List.mem nt consistent_states)) 
    |> List.fold_left (fun acc (old_nt, prods) -> 
        let new_st = List.assoc old_nt res_states_mapping in
        (printf "\n ** Which state?! %s\n" new_st);
        nonterm := new_st;
        let _old_st = Str.regexp old_nt in
        let new_prods = prods |> List.map (fun ln -> 
          (* [fixed] b/c not always replaced with primary state *)
          (* replace_str_wrt_primary_mapped_state ln res_states_mapping_primary *)
          if (String.ends_with ~suffix:":" ln) then (new_st ^ ":") else
          if (no_terms_or_nonterms ln) 
          then replace_str_wrt_primary_mapped_state ln res_states_mapping_primary
          else new_replace_str_wrt_mapped_states !nonterm ln
          ) in (new_prods |> List.iter (fun s -> printf "\n\t  *** Production accumulated  %s" s));
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
    (printf "\n*** Nontrivial prods mapping (terms, nonterms_num) -> prod \n"; Pp.pp_prods_mapping nontriv_prods_terms_ntnum_mapping);
  
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
  if debug_print then (printf "\n  >> Collected blocks:\n\n"; 
    inconsistent_nonterm_prods_blocks |> List.iter (fun (nt, ts_nts_p_ls) -> 
        printf "\t Nonterm %s -> \n" nt; ts_nts_p_ls |> List.iter (fun (ts, nts, p) -> 
        printf "\n\t\t Terminals  < "; ts |> List.iter (printf "%s "); printf "> ";
        printf " Nonterminals < "; nts |> List.iter (printf "%s "); printf ">   ==>  "; 
        Pp.pp_p p; printf "\n")); printf "\n");

  
  let find_assoc_all_new (terms: string list) (num_nonterms: int) (prods_mapping: ((string list * int) * (string * string list)) list) 
    (triv_nonterms: string list) (debug: bool): (string list * string) list = 
    printf "\n\t\tTRIV nonterms are? "; triv_nonterms |> List.iter (fun x -> printf "%s " x);
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
    if debug then (printf "\n\t\t Mapped (nts, prod) list -> \n"; 
    new_nts_prod_ls |> List.iter (fun (nts, prod) -> nts |> List.iter (fun x -> printf " %s" x); 
      printf "\n\t\t mapped to production: %s \n" prod)); new_nts_prod_ls in

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
    if debug_print then (printf "\n\t Find nontriv production from prods mapping from terminal list "; 
      terms |> Pp.pp_terminals; printf "\n\t AND nt_num %d \n" nt_num);
      match List.assoc_opt (terms, nt_num) nontriv_prods_terms_ntnum_mapping with 
      | Some (p, nts) -> 
        let nts_prods_mapped: (string list * string) list = 
          find_assoc_all_new terms nt_num nontriv_prods_terms_ntnum_mapping triv_nonterms debug_print 
        in
          (* [new_fix] tentative fix to account for ident & const happening for nontriv_state-starting trans *)
          if (List.length nts_prods_mapped = 1) || (List.length nts_prods_mapped = 2) then begin 
            let p' = if (triv_nontriv_nonterms_match nts new_nts) then p else "" in
              if debug_print then (printf "\n\t FOUND production %s \n" p; 
              printf " with nonterms "; nts |> Pp.pp_nonterminals); 
              if (String.equal p' "") then ("", []) else (p, nts) end 
          else 
            begin
              if List.length nts_prods_mapped >= 2 then printf "\n\t\tOVER HERE ~ !!\n\n";
              if (List.is_empty terms) && (nt_num = 1) && flag.onoff_opt then ("", []) else
            let raw_nts_p' = nts_prods_mapped |> List.filter (fun (nts, _prod) -> 
              string_lists_equal nts new_nts) in 
            let p' = if (List.is_empty raw_nts_p') then p else raw_nts_p' |> List.hd |> snd in 
            (p', nts) end
      | None -> 
        if debug_print then (printf "\n\t NOT FOUND so look for prod elsewhere \n"); ("", [])
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
    | T x -> (printf "\n\t\t\t Terminal %s" x); raise (Failure "Create epsilon prod rhs - terminal")
  in
  let changed_nonterm_prods: string list = 
    inconsistent_nonterm_prods_blocks |> List.fold_left (fun acc (nt, ts_nts_p_ls) -> 
      let new_prods = ts_nts_p_ls |> List.fold_left (fun acc (ts, new_nts, prod) -> 
        if debug_print then (printf "\nNow looking at prod starting from %s \n\t\t " nt; Pp.pp_p prod);
        let new_nts' = new_nts |> List.filter (fun nt -> not (String.equal nt epsilon_state)) in
        printf "\n\t New Nonts':\n\t\t"; new_nts' |> List.iter (fun x -> printf " %s" x); printf "\n";
        let old_prod_line, old_nts_ls = find_nontriv_prod_from_prods_mapping ts new_nts' (List.length new_nts') in
        if (String.equal "" old_prod_line) && (List.is_empty old_nts_ls) 
        then 
          (let new_str = match prod with (_nt, _i, sig_ls) -> create_epsilon_prod_rhs sig_ls
           in new_str :: acc) 
        else 
          let scan_forward' = to_scan_forward old_nts_ls new_nts' in
          let (old_nonts_init', correct_nonts_init') = 
            if scan_forward' 
            then (printf "\n\t\t\t Scan Forward\n\n"; (old_nts_ls, new_nts')) 
            else (printf "\n\t\t\t Scan Backward\n\n"; (List.rev old_nts_ls, List.rev new_nts')) 
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

