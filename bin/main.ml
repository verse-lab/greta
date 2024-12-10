open Greta
open Stdlib

module C = Converter
module O = Operation
module E = Examples
module L = Learner
module U = Treeutils
module D = Draw
module G = Cfg
module T = Ta

(* *************** Grammar REpair with Tree Automata *************** *)
(*                                                                   *)
(* Step 1: User feeds in inputs                                      *)
(* - 'versatile_syms' symbols that can have multiple arities         *)
(* - 'parser_file' grammar of the input language                     *)
(* - 'conflicts_file' {path}/{parser-file-name}.conflicts            *)
(*                                                                   *)
(* Step 2: User specifies preferences                                *)
(* - 0 or 1 given two different expressions                          *)
(*                                                                   *)
(* Step 3: New grammar is written on 'parser_file'                   *)
(* - If ambiguities still exist, ask user to run GRETA again         *)
(* - Repeat these steps until all the ambiguities are resolved       *)
(*                                                                   *)
(* ***************************************************************** *)

let () =
  (** Step 1: Initial inputs provided by the user *)
  let parser_file = "./lib/parser.mly" in
  let conflicts_file = "./_build/default/lib/parser.conflicts" in
  let cfg_file = "./_build/default/lib/parser.cfg" in
  (* Learn TA and O_bp wrt 'parser_file' *)
  let debug = true in
  if (Utils.check_conflicts conflicts_file debug) then
  begin
    let (ta_initial, o_bp, sym_ord_rhs_lst, o_bp_tbl, triv_syms_states, triv_syms): 
      T.ta2 * T.restriction list * ((T.symbol * int) * G.sigma list) list * 
      ((int, T.symbol list) Hashtbl.t) * (T.symbol * T.state) list * T.symbol list = 
      C.convertToTa cfg_file debug in
    let ranked_symbols = ta_initial.alphabet 
    in
    (* (TODO) Generate trees in <base>.trees instead *)
      let tree_pairs_lst: ((string list * T.tree * (bool * bool) * T.restriction list) * (string list * T.tree * (bool * bool) * T.restriction list)) list =
      E.gen_examples conflicts_file ranked_symbols debug 
    in
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
    in let learned_example_trees: (string list * T.tree * (bool * bool) * T.restriction list) list = 
        interact_with_user tree_pairs_lst 
    in 
    (* 'opt_flag' for different grammars:
     * G0, G1 -> opt_flag 
     * G2 -> opt_flag2 *)
    let _opt_flag: T.optimization = { eps_opt = true; paren_opt = true } in
    let opt_flag2: T.optimization = { eps_opt = false; paren_opt = false } in
    let o_a: T.restriction list = U.collect_oa_restrictions learned_example_trees debug in 
    let o_tmp: T.restriction list = U.collect_op_restrictions learned_example_trees debug in 
    let o_p: T.restriction list = U.combine_op_restrictions_in_pairs o_bp o_tmp debug in 
    let ta_learned: T.ta2 = 
      L.learn_ta o_a o_p o_bp_tbl ta_initial.trivial_sym_nts ranked_symbols sym_ord_rhs_lst triv_syms_states opt_flag2 debug 
    in     
    (** Step 3: Get disambiguated grammar and write on 'parser_file' *)
    let (ta_intersected, states_rename_map): T.ta2 * (T.state * T.state) list = 
      O.intersect ta_initial ta_learned triv_syms triv_syms_states opt_flag2 debug 
    in 
    (* let file_written = "./test/grammars/G0/G0_results/G0a"
    in  *)
    let grammar = "G2a" in
    let file_written = U.test_results_filepath grammar !file_postfix in 
    C.convertToGrammar ta_intersected states_rename_map ta_initial.start_states parser_file file_written debug;
    
end
else U.no_conflicts_message parser_file
(* 
U.success_message 100 (* !interact_counter *)
*)
  
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