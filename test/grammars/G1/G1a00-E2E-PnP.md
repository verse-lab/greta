# G2a E2E Illustration

## User Interaction 1

### UI 1 - Step 1: Extract CFG $G$

* V = { bexp1, bexp2 }
* $\Sigma$ = { VAR, ARR, BAR, AMPER, LPAREN, RPAREN, TILDE, TRUE, FALSE }
* S = { bexp1 }
* P = {
  bexp1 $\to$ VAR
  bexp1 $\to$ bexp1 ARR bexp2
  bexp1 $\to$ bexp1 BAR bexp2
  bexp1 $\to$ bexp2 AMPER bexp1
  bexp1 $\to$ bexp2

  bexp2 $\to$ TRUE
  bexp2 $\to$ FALSE
  bexp2 $\to$ TILDE bexp2
  bexp2 $\to$ LPAREN bexp1 RPAREN
  
  }

### UI 1 - Step 2: Convert CFG to TA $A_{g}$

* $Q_{g}$ = { bexp1, bexp2, $\epsilon$ }
* $F$ = { (VAR, 0), (ARR, 2), (BAR, 2), (AMPER, 2), (LPARENRPAREN, 1), (TILDE, 1), (TRUE, 0), (FALSE, 0) }
* $I_{g}$ = { bexp1 }
* $\Delta_{g}$ = {
  bexp1 $\to_{(\texttt{VAR}, 0)}$ $\epsilon$
  bexp1 $\to_{(\texttt{ARR}, 2)}$ bexp1 ARR bexp2
  bexp1 $\to_{(\texttt{BAR}, 2)}$ bexp1 BAR bexp2
  bexp1 $\to_{(\texttt{AMPER}, 2)}$ bexp2 AMPER bexp1
  bexp1 $\to_{(\varepsilon, 1)}$ bexp2

  bexp2 $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$
  bexp2 $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$
  bexp2 $\to_{(\texttt{TILDE}, 1)}$ TILDE bexp2
  bexp2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN bexp1 RPAREN
  
  }

Note that following terminals are _not_ included in function symbols: ELSE

### UI 1 - Step 3: Find $O_{bp}$

* Trivial symbols : { }
* Traverse $\Delta$ to get $L_{Q}$:
  { (bexp1, 0), (bexp2, 1) }
* Based on $\Delta$ and $L_{Q}$, get $O_{bp}$:
  { <(VAR, 0), 0>,  <($\varepsilon$, 1), 0>,  <(MUL, 2), 1>,  <(IF, 3), 1>,  <(IF, 5), 1>,  <(INT, 0), 1>,  <(LPARENRPAREN, 1), 1>,  <(TRUE, 0), 2>,  <(FALSE, 0), 2> }
* That is, $O_{bp}$ is order $\to$ symbols mapping

  - 0 $\to$ [ (PLUS, 2), ($\varepsilon$, 1) ]
  - 1 $\to$ [ (MUL, 2), (IF, 3), (IF, 5), (INT, 0), (LPARENRPAREN, 1) ]
* Trivial symbols: { (TRUE, 0), (FALSE, 0), (INT, 0) }
* Trivial states: { cond_expr }

### UI 1 - Step 4: Interact with user to collect preferences

* Based on the user preference, collect $O_{tmp}$:
  { <(MUL, 2), -1>, <(IF, 5), 0>, <(MUL, 2), -1>, <(IF, 3), 0> }
* $O_{a}$ := <(MUL, 2), "l">
* Trivial symbols: { (TRUE, 0), (FALSE, 0), (INT, 0) }
* Trivial states: { cond_expr }

### UI 1 - Step 5: Based on Step 4 and $O_{bp}$, learn $O_{p}$

* Based on Step 4 and $O_{bp}$, compute $O_{p}$:
  - Intermediate:
    + -1 $\to$ [ (MUL, 2) ]
    + 0 $\to$ [ (PLUS, 2), ($\varepsilon$, 1) ]
    + 1 $\to$ [ (IF, 3), (IF, 5), (INT, 0), (LPARENRPAREN, 1) ]
  - Final:
    + 0 $\to$ [ (MUL, 2) ]
    + 1 $\to$ [ (PLUS, 2), ($\varepsilon$, 1) ]
    + 2 $\to$ [ (IF, 3), (IF, 5), (INT, 0), (LPARENRPAREN, 1) ]
* $O_{a}$ := <(MUL, 2), "l">
* Trivial symbols: { (TRUE, 0), (FALSE, 0), (INT, 0) }
* Trivial states: { cond_expr }

### UI 1 - Step 6: Learn $A_{r}$

* $Q_{r}$ = { e1, e2, e3, cond_expr, $\epsilon$ }
* $F$ = { (INT, 0), (TRUE, 0), (FALSE, 0), (IF, 3), (IF, 5), (PLUS, 2), (MUL, 2), (LPARENRPAREN, 1), ($\varepsilon$, 1) }
* $I_{r}$ = { e1 }
* $\Delta_{r}$ = {
  e1 $\to_{(\texttt{MUL}, 2)}$ e2 MUL e1 
  _e1 $\to_{(\varepsilon, 1)}$ e2_

  e2 $\to_{(\texttt{PLUS}, 2)}$ e2 PLUS e2 
  e2 $\to_{(\varepsilon, 1)}$ e2 
  _e2 $\to_{(\varepsilon, 1)}$ e3_

  e3 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN e3 
  e3 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN e3 ELSE e3 \
  e3 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e3 RPAREN 
  _e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN_

  _cond\_expr $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$_ 
  _cond\_expr $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$_

  }

### UI 1 - Step 7: Take intersection of the tree automata ($A_{g} \cap A_{r}$)

* $Q$ = { expr1_e1, expr1_e2, expr2_e2, expr2_e1, expr2_e3, expr1_e3, cond_expr_cond_expr }
* $I$ = { expr1_e1 }
* $\Delta$ = {
  **expr1_e1** $\to_{(\texttt{PLUS}, 2)}$ **expr1_e2** PLUS expr1_e2 
  expr1_e1 $\to_{(\texttt{MUL}, 2)}$ **expr2_e2** MUL **expr2_e1** 
  expr1_e1 $\to_{(\varepsilon, 1)}$ expr2_e2 
  expr1_e1 $\to_{(\texttt{IF}, 3)}$ IF cond_expr_cond_expr THEN **expr2_e3** 
  expr1_e1 $\to_{(\texttt{IF}, 5)}$ IF cond_expr_cond_expr THEN expr2_e3 ELSE expr2_e3 
  expr1_e1 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  expr1_e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN **expr1_e3** RPAREN 
  expr1_e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e1 RPAREN

  **expr1_e2** $\to_{(\texttt{PLUS}, 2)}$ expr1_e2 PLUS expr1_e2 
  expr1_e2 $\to_{(\varepsilon, 1)}$ expr2_e2 
  expr1_e2 $\to_{(\texttt{IF}, 3)}$ IF cond_expr_cond_expr THEN expr2_e3 
  expr1_e2 $\to_{(\texttt{IF}, 5)}$ IF cond_expr_cond_expr THEN expr2_e3 ELSE expr2_e3 
  expr1_e2 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  expr1_e2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e3 RPAREN 
  expr1_e2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e1 RPAREN

  **expr2_e2** $\to_{(\texttt{IF}, 3)}$ IF cond_expr_cond_expr THEN expr2_e3 
  expr2_e2 $\to_{(\texttt{IF}, 5)}$ IF cond_expr_cond_expr THEN expr2_e3 ELSE expr2_e3 
  expr2_e2 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  expr2_e2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e3 RPAREN 
  expr2_e2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e1 RPAREN

  **expr2_e1** $\to_{(\texttt{MUL}, 2)}$ expr2_e2 MUL expr2_e1 
  expr2_e1 $\to_{(\varepsilon, 1)}$ expr2_e2 
  expr2_e1 $\to_{(\texttt{IF}, 3)}$ IF cond_expr_cond_expr THEN expr2_e3 
  expr2_e1 $\to_{(\texttt{IF}, 5)}$ IF cond_expr_cond_expr THEN expr2_e3 ELSE expr2_e3 
  expr2_e1 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  expr2_e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e3 RPAREN 
  expr2_e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e1 RPAREN

  **expr2_e3** $\to_{(\texttt{IF}, 3)}$ IF cond_expr_cond_expr THEN expr2_e3 
  expr2_e3 $\to_{(\texttt{IF}, 5)}$ IF cond_expr_cond_expr THEN expr2_e3 ELSE expr2_e3 
  expr2_e3 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  expr2_e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e3 RPAREN 
  expr2_e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e1 RPAREN

  **expr1_e3** $\to_{(\texttt{IF}, 3)}$ IF cond_expr_cond_expr THEN expr2_e3 
  expr1_e3 $\to_{(\texttt{IF}, 5)}$ IF cond_expr_cond_expr THEN expr2_e3 ELSE expr2_e3 
  expr1_e3 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  expr1_e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e3 RPAREN 
  expr1_e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN expr1_e1 RPAREN

  _cond\_expr\_cond\_expr $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$_ 
  _cond\_expr\_cond\_expr $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$_

  }

### UI 1 - Step 8: Identify a list of duplicate state pairs

* { (expr2_e2, expr2_e3), (expr2_e2, expr1_e3) }

### UI 1 - Step 9: Remove duplicate states and rename states

* $Q$ = { expr1_e1, expr1_e2, expr2_e2, expr2_e1, cond_expr }

  - Rename maps:
    + expr1_e1 $\to$ e1
    + expr1_e2 $\to$ x1
    + expr2_e2 $\to$ x2
    + expr2_e1 $\to$ x3
* $I$ = { e1 }
* $\Delta$ = {
  e1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 
  e1 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 
  e1 $\to_{(\varepsilon, 1)}$ x2 
  e1 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 
  e1 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 
  e1 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x2 RPAREN 
  e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  x1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 
  x1 $\to_{(\varepsilon, 1)}$ x2 
  x1 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 
  x1 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 
  x1 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  x1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x2 RPAREN 
  x1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  x2 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 
  x2 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 
  x2 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  x2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x2 RPAREN 
  x2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  x3 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 
  x3 $\to_{(\varepsilon, 1)}$ x2 
  x3 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 
  x3 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 
  x3 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  x3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x2 RPAREN 
  x3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  _cond\_expr $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$_ 
  _cond\_expr $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$_

  }

### UI 1 - Step 10: Introduce epsilon transitions to simplify the transitions

* $Q$ = { e1, x1, x2, x3, cond_expr, $\epsilon$ }
* $I$ = { e1 }
* Intermediate (Step 1) $\Delta$ = {e1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 e1 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 e1 $\to_{(\varepsilon, 1)}$ x2 e1 $\to_{(\varepsilon, 1)}$ x2

  x1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 x1 $\to_{(\varepsilon, 1)}$ x2 x1 $\to_{(\varepsilon, 1)}$ x2

  x3 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 x3 $\to_{(\varepsilon, 1)}$ x2 x3 $\to_{(\varepsilon, 1)}$ x2

  x2 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 x2 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 x2 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ x2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x2 RPAREN x2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  _cond\_expr $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$_ _cond\_expr $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$_

  }
* Intermediate (Step2) $\Delta$ = {e1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 e1 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 e1 $\to_{(\varepsilon, 1)}$ x2

  x1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 x1 $\to_{(\varepsilon, 1)}$ x2

  x3 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 x3 $\to_{(\varepsilon, 1)}$ x2

  x2 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 x2 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 x2 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ x2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  _cond\_expr $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$_ _cond\_expr $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$_

  }
* Final (Ver 1) $\Delta$ = {e1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 e1 $\to_{(\varepsilon, 1)}$ x3

  x1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 x1 $\to_{(\varepsilon, 1)}$ x2

  x3 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 x3 $\to_{(\varepsilon, 1)}$ x2

  x2 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 x2 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 x2 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ x2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  _cond\_expr $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$_ _cond\_expr $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$_

  }
* Final (Ver 2) $\Delta$ = {
  e1 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 
  e1 $\to_{(\varepsilon, 1)}$ x1

  x1 $\to_{(\texttt{PLUS}, 2)}$ x1 PLUS x1 
  x1 $\to_{(\varepsilon, 1)}$ x2

  x3 $\to_{(\texttt{MUL}, 2)}$ x2 MUL x3 
  x3 $\to_{(\varepsilon, 1)}$ x2

  x2 $\to_{(\texttt{IF}, 3)}$ IF cond_expr THEN x2 
  x2 $\to_{(\texttt{IF}, 5)}$ IF cond_expr THEN x2 ELSE x2 
  x2 $\to_{(\texttt{INT}, 0)}$ $\epsilon$ 
  x2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN

  _cond\_expr $\to_{(\texttt{TRUE}, 0)}$ $\epsilon$_ 
  _cond\_expr $\to_{(\texttt{FALSE}, 0)}$ $\epsilon$_

  }

After writing the above TA as a correctly formatted CFG on the parser file, running Greta on the grammar results in following.

### UI 2 - Step 1: Extract CFG $G$

* V = { cond_expr, e1, x1, x2, x3 }
* $\Sigma$ = { INT, TRUE, FALSE, IF, THEN, ELSE, PLUS, MUL, LPAREN, RPAREN }
* S = { e1 }
* P = {
  e1 $\to$ x1 PLUS x1 
  e1 $\to$ x3

  x1 $\to$ x1 PLUS x1 
  x1 $\to$ x2

  x3 $\to$ x2 MUL x3 
  x3 $\to$ x2

  x2 $\to$ IF cond_expr THEN x2 
  x2 $\to$ IF cond_expr THEN x2 ELSE x2 
  x2 $\to$ $\epsilon$ 
  x2 $\to$ LPAREN e1 RPAREN

  cond_expr $\to$ TRUE 
  cond_expr $\to$ FALSE

  }

### UI 2 - Step 2: Convert CFG $G$ to TA $A_{g}$
