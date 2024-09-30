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

let assoc_all (a: Ta.symbol) (ab_ls: (Ta.symbol * Cfg.sigma list) list) (debug_print: bool): Cfg.sigma list list = 
  let rec loop ls acc =
    match ls with [] -> List.rev acc
    | (x, xs) :: tl ->
      if (Ta.syms_equals a x) then loop tl (xs::acc)
      else loop tl acc
  in let res = loop ab_ls [] in 
  let res_epsilon_acc = if (res = []) then [[(Cfg.Nt Ta.epsilon_state)]] else res in
  if debug_print then 
    (let open Pp in let open Printf in printf "\n\n\t   For symbol "; pp_symbol a; 
     printf " collected:\t"; res_epsilon_acc |> List.iter (fun s_ls -> pp_sigma_list ("", s_ls)));
  res_epsilon_acc
