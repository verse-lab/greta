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
(*   - 'parser_file' grammar of the input language                   *)
(*                                                                   *)
(* Step 1: Input CFG is converted to TA                              *)
(*                                                                   *)
(* Step 2: Generate trees wrt ambiguities                            *)
(*                                                                   *)
(* Step 3: User specifies preferences                                *)
(* - 0 or 1 given two parsing options                                *)
(*                                                                   *)
(* Step 4: Learn O_p, O_a wrt. tree examples                         *)
(*                                                                   *)
(* Step 5: TA is learned via original CFG and O_p, O_a.              *)
(*                                                                   *)
(* Step 6: Learned TA and TA from original CFG are intersected       *)
(*                                                                   *)
(* Step 7: Resulted TA is converted back to CFG                      *)
(*                                                                   *)
(* Step 8: Resulted CFG is written on the parser.mly file            *)
(*   - If ambiguities still exist, repeat Step 2                     *)
(*   - Repeat 2-7 until all addressable ambiguities are resolved     *)
(*                                                                   *)
(* ***************************************************************** *)
let safe_arg idx =
  if idx < Array.length Sys.argv then Array.get Sys.argv idx
  else ""

let () =
  (* ----------------------------------------------------------------- *)
  (* Step 0: Initial grammar provided by the user -------------------- *)
  (* ----------------------------------------------------------------- *)
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
  
  if (Utils.check_conflicts !conflicts_file debug) then
  begin
    (* ----------------------------------------------------------------- *)
    (* Step 1: Input CFG is converted to TA and learn O_bp wrt CFG ----- *)
    (* ----------------------------------------------------------------- *)

    let convert_start = Sys.time () 
    in
    let (ta_initial, o_bp, o_bp_tbl, prods_map): 
      T.ta * T.restriction list * ((int, T.symbol list) Hashtbl.t) * (int * G.production) list = 
      C.convertToTa !cfg_file debug 
    in  
    let _convert_elapsed = Sys.time () -. convert_start in
    let ranked_symbols = ta_initial.alphabet 
    in
    
    
    (* ----------------------------------------------------------------- *)
    (* Step 2: Generate a set of tree examples wrt ambiguities --------- *)
    (* ----------------------------------------------------------------- *)

    let tree_pairs_lst: ((string list * T.tree * (bool * bool * bool) * T.restriction list) * (string list * T.tree * (bool * bool * bool) * T.restriction list)) list =
      E.gen_examples !tree_file ranked_symbols prods_map debug
    in 
    if (List.is_empty tree_pairs_lst) then () else 
    
    
    (* ----------------------------------------------------------------- *)
    (* Step 3: User specifies preferences, and collect trees chosen ---- *)
    (* ----------------------------------------------------------------- *)

    let file_postfix = ref "" in
    let interact_with_user (inp_lst: ((string list * T.tree * (bool * bool * bool) * T.restriction list) * (string list * T.tree * (bool * bool * bool) * T.restriction list)) list):
      (string list * T.tree * (bool * bool * bool) * T.restriction list) list = 
        let rec loop lst acc = 
          match lst with [] -> acc
          | ((texpr_ls1, t1, (oa1_pos, oa1_neg, op1), rls1), (texpr_ls2, t2, (oa2_pos, oa2_neg, op2), rls2)) :: tl -> 
            (U.present_tree_pair (t1, t2);
            let chosen_index = read_int () in
            file_postfix := !file_postfix ^ (string_of_int chosen_index);
            (* If oa, then add logic of whether it's pos/neg wrt. choice and collect all trees *)
            if (U.is_oa_tree t1) && (U.is_oa_tree t2) 
            then 
              (if (chosen_index = 0)
              then loop tl ((texpr_ls1, t1, (true, false, op1), rls1) :: (texpr_ls2, t2, (false, true, op2), rls2) :: acc)
              else loop tl ((texpr_ls1, t1, (false, true, op1), rls1) :: (texpr_ls2, t2, (true, false, op2), rls2) :: acc))
            else if (chosen_index = 0) 
            then loop tl ((texpr_ls1, t1, (oa1_pos, oa1_neg, op1), rls1)::acc)
            else 
              (* if user selects 1 or any other number, 2nd tree gets selected *)
              loop tl ((texpr_ls2, t2, (oa2_pos, oa2_neg, op2), rls2)::acc))
        in loop inp_lst []
    in
    (* Time output *)
    let learn_start = Sys.time () in
    let learned_example_trees: (string list * T.tree * (bool * bool * bool) * T.restriction list) list = 
      interact_with_user tree_pairs_lst 
    in


    (* ----------------------------------------------------------------- *)
    (* Step 4: Learn O_a, O_p wrt. tree examples ----------------------- *)
    (* ----------------------------------------------------------------- *)
    
    let oa_neg_learned: T.restriction list =
       L.learn_oa_neg learned_example_trees debug in
    let op_learned: (int, T.symbol list) Hashtbl.t = 
      L.learn_op o_bp_tbl learned_example_trees oa_neg_learned debug in
    

    (* ----------------------------------------------------------------- *)
    (* Step 5: TA is learned via original CFG and O_p, O_a (neg) ------- *)
    (* ----------------------------------------------------------------- *)
    
    let ta_learned: T.ta = 
      L.learn_ta op_learned oa_neg_learned prods_map debug
    in
    let _learn_ta_elapsed = Sys.time () -. learn_start in
    

    (* ----------------------------------------------------------------- *)
    (* Step 6: Intersect learned TA and TA from original CFG ----------- *)
    (* ----------------------------------------------------------------- *)

    let intersect_start = Sys.time () in
    let _ta_intersected: T.ta = O.intersect ta_initial ta_learned debug 
    in
    let _intersect_elapsed = Sys.time () -. intersect_start in
    
    (* ----------------------------------------------------------------- *)
    (* Step 7: Resulted TA is converted back to CFG -------------------- *)
    (* ----------------------------------------------------------------- *)

    (* let cfg_res =  
      C.convertToGrammar ta_intersected states_rename_map debug in *)

    
    (* ----------------------------------------------------------------- *)
    (* Step 8: Resulted CFG is written on the output file -------------- *)
    (* ----------------------------------------------------------------- *)

    (*
    let file_written = "./test/grammars/Ga/Ga_results/Gaa.mly" in 
    let grammar = 
      String.split_on_char '.' !parser_file |> List.hd 
    in
    let _file_written = U.test_results_filepath grammar !file_postfix in 
     !parser_file file_written
    
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