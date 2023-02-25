open Greta
open Ta

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


