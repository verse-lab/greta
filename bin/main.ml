open Greta
open Stdlib

module C = Converter
module O = Operation
module E = Examples
module L = Learner
module U = Treeutils
module G = Cfg
module T = Ta

(* *************** Grammar REpair with Tree Automata *************** *)
(*                                                                   *)
(* Step 0: User feeds in input                                       *)
(* - 'parser_file' grammar of the input language                     *)
(*                                                                   *)
(* Step 1: Input CFG is converted to TA                              *)
(*                                                                   *)
(* Step 2: User specifies preferences                                *)
(* - 0 or 1 given two parsing options                                *)
(*                                                                   *)
(* Step 3: TA is learned via original CFG and user preferences       *)
(*                                                                   *)
(* Step 4: Learned TA and TA from original CFG are intersected       *)
(*                                                                   *)
(* Step 5: Resulted TA is converted back to CFG                      *)
(* - If ambiguities still exist, repeat Step 2                       *)
(* - Repeat 2-5 until all addressable ambiguities are resolved       *)
(*                                                                   *)
(* ***************************************************************** *)
let safe_arg idx =
  if idx < Array.length Sys.argv then Array.get Sys.argv idx
  else ""

let () =
  (* *** Step 0: Initial grammar provided by the user *** *)
  let parser_file = ref (safe_arg 1) in
  let conflicts_file = ref (safe_arg 2) in
  let cfg_file = ref (safe_arg 3) in
  let tree_file = ref (safe_arg 4) in

  let is_empty = String.equal String.empty in
  if !parser_file |> is_empty && 
    !conflicts_file |> is_empty && 
    !cfg_file |> is_empty && 
    !tree_file |> is_empty
  then begin
    parser_file := "./lib/parser.mly";
    conflicts_file := "./_build/default/lib/parser.conflicts";
    cfg_file := "./_build/default/lib/parser.cfg";
    tree_file := "./_build/default/lib/parser.trees";
  end;

  Array.to_list Sys.argv |> List.iter (fun arg -> print_string (arg ^ " ")); print_newline ();
  print_string ("Parser file: " ^ !parser_file); print_newline ();
  print_string ("Conflicts file: " ^ !conflicts_file); print_newline ();
  print_string ("CFG file: " ^ !cfg_file); print_newline ();
  print_string ("Tree file: " ^ !tree_file); print_newline ();

  (* Check that the path exists *)
  if (not (Sys.file_exists !parser_file)) then 
    (print_endline "Error: Parser file does not exist. Exiting..."; exit 1)
  else if (not (Sys.file_exists !conflicts_file)) then 
    (print_endline "Error: Conflicts file does not exist. Exiting..."; exit 1)
  else if (not (Sys.file_exists !cfg_file)) then 
    (print_endline "Error: CFG file does not exist. Exiting..."; exit 1)
  else

  let debug = true in
  (* let opt_flag: T.optimization = { eps_opt = true; paren_opt = false; triv_opt = false; onoff_opt = true } in  *)
  
  if (Utils.check_conflicts !conflicts_file debug) then
  begin
    let _convert_start = Sys.time () in
    (* Step 1: Input CFG is converted to TA and learn O_bp wrt CFG *)

    let (ta_initial, o_bp, o_bp_tbl, prods_map): 
      T.ta * T.restriction list * ((int, T.symbol list) Hashtbl.t) * (int * G.production) list = 
      C.convertToTa !cfg_file debug in
      
    let _convert_elapsed = Sys.time () -. _convert_start in
    let ranked_symbols = ta_initial.alphabet 
    in
    
    let tree_pairs_lst: ((string list * T.tree * (bool * bool) * T.restriction list) * (string list * T.tree * (bool * bool) * T.restriction list)) list =
      E.gen_examples !tree_file ranked_symbols debug
    in 
    if (List.is_empty tree_pairs_lst) then () else 
    (** Step 2: Interact with the user to learn user-preferred T (and T to O_a and O_p) *)
    let file_postfix = ref "" in
    let interact_with_user (inp_lst: ((string list * T.tree * (bool * bool) * T.restriction list) * (string list * T.tree * (bool * bool) * T.restriction list)) list):
      (string list * T.tree * (bool * bool) * T.restriction list) list = 
        let rec loop lst acc = 
          match lst with [] -> acc
          | ((texpr_ls1, t1, (oa1, op1), rls1), (texpr_ls2, t2, (oa2, op2), rls2)) :: tl -> 
           (U.present_tree_pair (t1, t2);
            let chosen_index = read_int () in
            file_postfix := !file_postfix ^ (string_of_int chosen_index);
            if (chosen_index = 0) 
            then loop tl ((texpr_ls1, t1, (oa1, op1), rls1)::acc)
            (* if user selects 1 or any other number, 2nd tree gets selected *)
            else loop tl ((texpr_ls2, t2, (oa2, op2), rls2)::acc))
        in loop inp_lst []
    in
    (* let learned_example_trees: (string list * T.tree * (bool * bool) * T.restriction list) list = 
        interact_with_user tree_pairs_lst in  *)

    (* Time output *)
    let _learn_start = Sys.time () in
    let _learned_example_trees: (string list * T.tree * (bool * bool) * T.restriction list) list = 
        interact_with_user tree_pairs_lst 
    in 
    (* 
    let ta_learned: T.ta2 = 
      L.learn_ta learned_example_trees o_bp_tbl ta_initial.trivial_sym_nts ranked_symbols sym_ord_rhs_lst triv_syms_states 
      sts_order_syms_lsls opt_flag debug
    in
    let _learn_ta_elapsed = Sys.time () -. _learn_start in

    (** Step 3: Get disambiguated grammar and write on 'parser_file' *)
    let intersect_start = Sys.time () in
    let (ta_intersected, states_rename_map): T.ta2 * (T.state * T.state) list = 
      O.intersect ta_initial ta_learned triv_syms triv_syms_states opt_flag debug 
    in     
    let intersect_elapsed = Sys.time () -. intersect_start in
    (* ta_intersected.trivial_sym_nts |> List.iter (fun (sym, st) -> Pp.pp_symbol sym; Printf.printf "\t ---> State %s" st); *)
    let file_written = "./test/grammars/Ga/Ga_results/Gaa.mly" in 
    let grammar = 
      String.split_on_char '.' !parser_file |> List.hd 
      (* "Gaa"   *)
    in
    let _file_written = U.test_results_filepath grammar !file_postfix in 
    C.convertToGrammar ta_intersected states_rename_map ta_initial.start_states !parser_file file_written opt_flag debug;
    
    Printf.printf "\n\n\t\tGrammar written to %s\n\n" file_written;
    Printf.printf "\n\n\t\tTime elapsed for converting TA: %f\n\n" _convert_elapsed;
    Printf.printf "\n\n\t\tTime elapsed for learning TA: %f\n\n" _learn_ta_elapsed;
    Printf.printf "\n\n\t\tTime elapsed for intersecting TA: %f\n\n" intersect_elapsed;
    (* Time for convering back to CFG *)
   *) 
    ()
  
end
else U.no_conflicts_message !parser_file


(*** Assumptions made on the language designer (user of this tool):
 *   * Non-terminals representing boolean are specified with "cond" ^ s*
 *     - If this assumption changes, change data type to represent state to a tuple 
 *     - so that when taking X of lists of states, you won't generate unnecesary many states
 *     - but only the relevant ones, e.g., cond_exprCond_expr
 ***)

(* Notes: 
 *      - To add a loop until user selects the right index
 *      - To draw trees using Graphics (starting from let _ = D.draw_tree tree_test "testA") 
 *      - Tested with "./test/parser0.conflicts" as 'conflicts_file'
 *      - Tested with test_parser_file (test_parser_file = "./test/test_parser.mly")
 *)
  

(* 
create

*)