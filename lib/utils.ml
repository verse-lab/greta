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

let sig_sig_assoc_all (sym: Ta.symbol) (trans: (Ta.symbol * (Cfg.sigma * Cfg.sigma) list) list)
  : (Cfg.sigma * Cfg.sigma) list = 
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
