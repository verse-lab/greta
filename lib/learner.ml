open Ta
open Examples

let height (e: tree): int =
  let max_ls ls = List.fold_left max 0 ls in 
  let rec loop t acc = match t with 
    | Leaf _ -> acc
    | Node (_, _, ts) -> ts 
      |> List.map (fun x -> loop x (acc+1)) 
      |> max_ls
  in loop e 0

let redefine_tree (e: tree): tree =
  let open Pp in let open Printf 
  in printf "\nRedefining tree: "; pp_tree e;
  let rec loop e d =
    match e with 
    | Leaf s -> 
      let s' = (String.capitalize_ascii s) ^ "_" ^ string_of_int d in
      Leaf s'
    | Node (sym, ld, ts) -> 
      let ts' = ts |> List.map (fun x-> loop x (d + 1)) in
      let ld' = (String.capitalize_ascii ld) ^ "_" ^ string_of_int d in
      Node (sym, ld', ts')
  in let e' = loop e 1 
  in printf "\nRedefined example: "; pp_tree e'; e'


let learner (e: tree) (a: symbol list): ta =
  let open Printf in 
  let debug_print = true in
  if debug_print then (printf "\nInputs are... \n\tExample: "; Pp.pp_tree e; 
  printf "\n\tAlphabet: { "; a |> List.iter Pp.pp_symbol; printf "}\n");
  (* Define Q with Expr_1..Expr_h+1 for height h *)
  let rec gen_states n acc_ss: state list =
    if (n = 0) then acc_ss
    else gen_states (n-1) (("Expr_" ^ (string_of_int n)) ::acc_ss)
  in let state_ls = gen_states (height e + 1) [] in 
  (* Redefine e as e' wrt Expr_1..Expr_h+1 *)
  let _ (* e' *) = redefine_tree e in
  { states = state_ls ; alphabet = []; start_state = "Expr_1"; transitions = [] }




