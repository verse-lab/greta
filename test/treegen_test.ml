open Greta
open Ta
open Treeutils

module E = Examples

let ex02 = Node (("+", 2), [Leaf "expr"; Leaf "expr"])
let ex03 = Node (("+", 2), [Leaf "expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
let ex04 = Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
let ex05 = Node (("IF", 2), [Leaf "cond_expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])

let es = 
  [ (* e0 : expr *)
    Leaf "expr"
  ; (* e1 : 'N' *) 
    Node (("N", 0), [Leaf "ϵ"])
  ; (* e2 : expr '+' expr *)
    Node (("+", 2), [Leaf "expr"; Leaf "expr"])
  ; (* e3 : expr '+' (expr '*' expr) *)
    Node (("+", 2), [Leaf "expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e4 : expr '*' (expr '+' expr) *)
    Node (("*", 2), [Leaf "expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e5 : 'IF' cond_expr 'THEN' expr 'ELSE' (expr '+' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e6 : expr '+' ('IF' cond_expr 'THEN' expr 'ELSE' expr) *)
    Node (("+", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])])
  ; (* e7 : 'IF' cond_expr 'THEN' expr 'ELSE' (expr '*' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e8 : expr '*' ('IF' cond_expr 'THEN' expr 'ELSE' expr) *)
    Node (("*", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])])
  ; (* e9 : 'IF' cond_expr 'THEN' (expr '+' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e10 : expr '+' ('IF' cond_expr 'THEN' expr) *)
    Node (("+", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"])])
  ; (* e11 : 'IF' cond_expr 'THEN' (expr '*' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e12 : expr '*' ('IF' cond_expr 'THEN' expr) *)
    Node (("*", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"])])
  ]

let es_neg = 
  [ (* e0_neg : expr *)
    Leaf "expr"
  ; (* e1_neg : 'N' *) 
    Node (("N", 0), [Leaf "ϵ"])
  ; (* e2_neg : expr '+' expr *)
    Node (("+", 2), [Leaf "expr"; Leaf "expr"])
  ; (* e3_neg : expr '*' (expr '+' expr) *)
    Node (("*", 2), [Leaf "expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e4_neg : expr '+' (expr '*' expr) *)
    Node (("+", 2), [Leaf "expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e5_neg : expr '+' ('IF' cond_expr 'THEN' expr 'ELSE' expr) *)
    Node (("+", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])])
  ; (* e6_neg : 'IF' cond_expr 'THEN' expr 'ELSE' (expr '+' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e7_neg : expr '*' ('IF' cond_expr 'THEN' expr 'ELSE' expr) *)
    Node (("*", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Leaf "expr"])])
  ; (* e8_neg : 'IF' cond_expr 'THEN' expr 'ELSE' (expr '*' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e9_neg : expr '+' ('IF' cond_expr 'THEN' expr) *)
    Node (("+", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"])])
  ; (* e10_neg : 'IF' cond_expr 'THEN' (expr '+' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Node (("+", 2), [Leaf "expr"; Leaf "expr"])])
  ; (* e11_neg : expr '*' ('IF' cond_expr 'THEN' expr) *)
    Node (("*", 2), [Leaf "expr"; Node (("IF", 2), [Leaf "cond_expr"; Leaf "expr"])])
  ; (* e12_neg : 'IF' cond_expr 'THEN' (expr '*' expr) *)
    Node (("IF", 2), [Leaf "cond_expr"; Node (("*", 2), [Leaf "expr"; Leaf "expr"])])
  ]

let check_equality_of_trees (ts1: tree list) (ts2: tree list) (debug_print: bool): bool =
  List.fold_left2 (fun acc t1 t2 -> acc && (trees_equal t1 t2 debug_print)) true ts1 ts2

let () = 
  let open Printf in
  let debug_print = true in
  printf "\n\nTesting  ...\n\n";
  let res_tls = es |> List.map (E.negate_pat debug_print) in
  let comp_res = check_equality_of_trees res_tls es_neg debug_print in
  if comp_res then printf "\n\n\tTests passed\n" else printf "\n\n\tTests Failed\n";
  (* let e0, e1, e2 = (List.nth es 0), (List.nth es 1), (List.nth es 2) in
  let e3, e4, e5 = (List.nth es 3), (List.nth es 4), (List.nth es 5) in
  let e6, e7, e8 = (List.nth es 6), (List.nth es 7), (List.nth es 8) in
  let e9, e10, e11, e12 = (List.nth es 9), (List.nth es 10), (List.nth es 11), (List.nth es 12) in
  let _ = (E.negate_pat debug_print e0) 
  and _ = (E.negate_pat debug_print e1)
  and _ = (E.negate_pat debug_print e2)
  and _ = (E.negate_pat debug_print e3)
  and _ = (E.negate_pat debug_print e4)
  and _ = (E.negate_pat debug_print e5)
  and _ = (E.negate_pat debug_print e6)
  and _ = (E.negate_pat debug_print e7)
  and _ = (E.negate_pat debug_print e8)
  and _ = (E.negate_pat debug_print e9)
  and _ = (E.negate_pat debug_print e10)
  and _ = (E.negate_pat debug_print e11)
  and _ = (E.negate_pat debug_print e12) in *)
  ()












