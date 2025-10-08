(** utilities for gluing togther the Menhir parser with the Sedlex lexer  *)
(* let revised_parse (lexbuf: Sedlexing.lexbuf): Ast.t =
  let tok () =
    let tok = Lexer.token lexbuf in
    let (st,ed) = Sedlexing.lexing_positions lexbuf in
    (tok,st,ed) in
  MenhirLib.Convert.Simplified.traditional2revised
    Parser.program tok

let parse lexbuf =
  try revised_parse lexbuf with Parser.Error -> failwith "failed to parse" 

let parse_string str =
  parse (Sedlexing.Utf8.from_string str) *)

let id x = x

let remove_dups ls =
  let unique_cons elem ls = if (List.mem elem ls) then ls else elem :: ls in
  List.fold_right unique_cons ls []

let runIf debug f = if debug then f ()

(** utilities for reading lines from parser *)
let read_line i = try Some (input_line i) with End_of_file -> None

let starts tk s = String.starts_with ~prefix:tk s

let starts_with_any_of tks s = 
  tks |> List.fold_left (fun acc tk -> (starts tk s) || acc) false

(** utilities for checking the existence of conflicts file *)
let check_conflicts (conflicts_file: string) (debug_print: bool): bool =
  let open Printf in 
  let conflicts_file_exist = Sys.file_exists conflicts_file in
  let conflicts_file_nonempty = 
    let rec loop inp acc =
      match (read_line inp) with 
      | None -> (List.length acc) != 0
      | Some s -> loop inp (s::acc)
    in loop (open_in conflicts_file) []
  in
  let res = conflicts_file_exist && conflicts_file_nonempty in
  if debug_print then (printf "\n\n  >> Is there any conflicts in grammar?\n";
  if res then printf "\t\t\tYES\n\n" else printf "\t\t\tNO\n\n");
  res

let assoc_all (a: Ta.symbol) (ord: int) (ab_ls: ((Ta.symbol * int) * Cfg.sigma list) list) (debug_print: bool): Cfg.sigma list list = 
  let _take_out_epsilon_state (lsls: Cfg.sigma list list): Cfg.sigma list list = 
    let eps_ls = [(Cfg.Nt Ta.epsilon_state)] in
    lsls |> List.filter (fun ls -> not (ls = eps_ls))
  in
  let rec loop ls acc =
    match ls with [] -> List.rev acc
    | ((x, o), xs) :: tl ->
      if (Ta.syms_equals a x) && (o = ord) then loop tl (xs::acc)
      else loop tl acc
  in let res = loop ab_ls [] in (* |> take_out_epsilon_state *)
  let open Pp in let open Printf in 
  if debug_print then (printf "\n\n\t   For symbol "; pp_symbol a; printf " collected:\t"; 
  res |> List.iter (fun s_ls -> printf " [ "; s_ls |> List.iter pp_sigma; printf "] "));
  res


let merge ~into:tab1 tab2 =
  Hashtbl.fold (fun key elt () -> Hashtbl.add tab1 key elt) tab2 ();
  tab1

(* let sig_sig_assoc_all (sym: Ta.symbol) (trans: (Ta.symbol * (Cfg.sigma * Cfg.sigma) list) list)
  : (Cfg.sigma * Cfg.sigma) list = 
  let rec accumulate ls acc = 
    match ls with [] -> List.rev acc 
    | (s, sig_sig_ls) :: tl ->
      if Ta.syms_equals s sym 
      then accumulate tl (sig_sig_ls @ acc)
      else accumulate tl acc
  in accumulate trans [] *)

let beta_beta_assoc_all (sym: Ta.symbol) (trans: (Ta.symbol * (Ta.beta * Ta.beta) list) list)
  : (Ta.beta * Ta.beta) list = 
  let rec accumulate ls acc = 
    match ls with [] -> List.rev acc 
    | (s, sig_sig_ls) :: tl ->
      if Ta.syms_equals s sym 
      then accumulate tl (sig_sig_ls @ acc)
      else accumulate tl acc
  in accumulate trans []

let strip_string s =
  Str.global_replace (Str.regexp "[\r\n\t ]") "" s

let str_rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))

let str_replace_last (old_substr: string) (new_substr: string) (str: string): string = 
  let rev_str = str_rev str in 
  let old_substr_raw = str_rev old_substr in 
  let (old_substr', new_substr') = (Str.regexp old_substr_raw, str_rev new_substr) in 
  let res_rev_str = Str.replace_first old_substr' new_substr' rev_str in 
  str_rev res_rev_str

let rec sublist i j l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if j=0 then [] else sublist (i-1) (j-1) t in
     if i>0 then tail else h :: tail

let strip str = 
  let str = Str.replace_first (Str.regexp "^ +") "" str in
  Str.replace_first (Str.regexp " +$") "" str

let are_paren_terminals (ts: string list): bool = 
  match ts with 
  | fst :: snd :: [] -> (String.equal fst "LPAREN") && (String.equal snd "RPAREN")
  | _ -> false

let string_lists_equal (ls1: string list) (ls2: string list) = 
  (List.length ls1 = List.length ls2) &&
  ls1 |> List.fold_left (fun acc s1 -> acc && (List.mem s1 ls2)) true 

  
let find_assoc_all (terms: string list) (num_nonterms: int) (prods_mapping: ((string list * int) * (string * string list)) list) 
  (_dummy: string list) (debug: bool): (string list * string) list = 
  let open Printf in 
  let rec find_all_loop prods_ls (acc: (string list * string) list) = 
    match prods_ls with [] -> List.rev acc
    | ((hd_terms, hd_nt_num), (hd_prod, hd_nts)) :: prods_tl -> 
      if (num_nonterms = hd_nt_num) && (string_lists_equal hd_terms terms) 
      then find_all_loop prods_tl ((hd_nts, hd_prod)::acc)
      else find_all_loop prods_tl acc 
  in let nts_prod_ls = find_all_loop prods_mapping [] in 
  if debug then (printf "\n\t\t Mapped (nts, prod) list -> \n"; 
    nts_prod_ls |> List.iter (fun (nts, prod) -> nts |> List.iter (fun x -> printf " %s" x); 
    printf "\n\t\t mapped to production: %s \n" prod)); nts_prod_ls

module StringMap = Map.Make (String)

let min_by_key (ls: (Ta.state * int) list): (Ta.state * int) list =
  let update key v acc =
    match StringMap.find_opt key acc with
    | None -> StringMap.add key v acc
    | Some old -> StringMap.add key (min v old) acc
  in
  let map = List.fold_left (fun acc (k, v) -> update k v acc) StringMap.empty ls in
  StringMap.bindings map

module PairOrd = struct
  type t = int * Ta.state
  let compare = compare
end

module PairMap = Map.Make(PairOrd)

(* group_by_lvl_state: ((int * state) * symbol) list -> ((int * state) * symbol list) list *)
let group_by_lvl_state ls =
  ls |> List.fold_left (fun acc (k, v) -> 
    let vs = match PairMap.find_opt k acc with
             | Some xs -> v :: xs
             | None -> [v]
    in PairMap.add k vs acc) PairMap.empty |> PairMap.bindings

let combine_syms_of_same_order (ls: (Cfg.nonterminal * int * Ta.symbol list) list): (int * (Ta.symbol list) list) list =
  let add acc (_nt, i, sym_ls) =
    let old = try List.assoc i acc with Not_found -> [] 
    in
      (i,  sym_ls :: old) :: List.remove_assoc i acc
  in
  List.fold_left add [] ls 