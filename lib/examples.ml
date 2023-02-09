open Ta
(* open QCheck *)

(* TODO : rearrange test suite with examples below *)
(* ex00 : expr *)
let ex00 = Leaf "expr"

(* ex01 : N *)
let ex01 = Node (("N", 0), [Leaf "ϵ"])

(* ex02 : expr `+` expr *)
let ex02 = Node (("+", 2), [Leaf "expr"; Leaf "expr"])

(* ex03 : expr `+` (expr `*` expr) *)
let ex03 = Node (("+", 2), 
  [Node (("*", 2), [Leaf "expr"; Leaf "expr"]); 
  Leaf "expr"])

(* let ex03 = Node (("IF", 2),
  []) *)

(** gen_examples : gen examples from conflicts in CFG *)
let gen_examples (filename: string): tree list = 
  if Sys.file_exists filename
  then Printf.printf "Conflicts exist\n\n"
  else Printf.printf "Conflicts do NOT exist\n\n"; []
  (* TODO: Continue from here ...  *)
  (** read lines from parser.conflicts *)
  (* 
     let _ (* conflict_lines *) : string list =
    let ic = open_in filename in
    let try_read () = 
      try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = 
      match try_read () with
      | Some s -> loop (s :: acc)
      | None -> close_in ic; List.rev acc
    in loop [] 
  in
  Format.print_string filename *)

(** randomly generator of trees with a specific pattern 
    - to stress test Learner.learner *)
let rec rand_tree (a: symbol list) (debug_print: bool) (dep: int): tree =
  let open Pp in
  let open Printf in
  let len = List.length a in
  let rind = Random.self_init (); Random.int len in
  let sym = List.nth a rind in 
    if debug_print then (printf "\nRandomly generating tree given : \n\t"; pp_alphabet a;
    printf "\nRandomly selected symbol is "; pp_symbol sym);
  let ar = arity sym in
  let tree_res = match ar with
  | 0 -> Node (sym, [Leaf "ϵ"])
    (* if debug_print then (pp_repeat dep "\t" ; pp_tree t'; printf "\n");  *)
  | num -> begin match fst sym with "IF" -> 
      if (ar = 2) then Node (sym, [Leaf "cond_expr"; Leaf "expr"])
      else Node (sym, [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])
    | _ -> let trees_ls: tree list = 
      List.init num (fun _ -> rand_tree a debug_print (dep+1)) in Node (sym, trees_ls) end 
  in printf "\n\nTree Generated: \n";pp_repeat dep "  "; pp_tree tree_res; printf "\n"; tree_res

(* let gen_rand_trees n a debug_print: tree list = 
  if debug_print then Printf.printf "\nRandom trees generated : { \n\t"; 
  List.init n (fun _ ->
  let t = rand_tree a debug_print in Pp.pp_tree t; t) *)


