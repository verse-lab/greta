open Ta
open Examples


(* let learn_ta (e: tree): ta =
  match e with
  | Leaf s ->
  | Node (s, ls) ->
  null_ta *)

let learner (e: tree) (a: symbol list): ta =
  let open Printf in 
  let debug_print = true in
  if debug_print then (printf "\nExample: "; Pp.pp_example e; 
  printf "\nAlphabet: "; a |> List.iter Pp.pp_symbol; printf "\n");
  null_ta


