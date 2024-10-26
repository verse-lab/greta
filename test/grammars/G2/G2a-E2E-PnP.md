## G2a E2E Illustration 

### Step 1: Extract CFG $G$

* V = { ident, decl, const, exp, stmt, stmts }
* $\Sigma$ = { INT, IDENT, STRING, ELSE, IF, TINT, RET, WHILE, SEMI, LBRACE, RBRACE, PLUS, DASH, STAR, EQ, LPAREN, RPAREN }
* S = { stmts }
* P = {\
    ident $\to$ IDENT 
    
    decl $\to$ TINT ident EQ exp
    
    const $\to$ INT

    exp $\to$ exp PLUS exp \
    exp $\to$ exp DASH exp \
    exp $\to$ exp STAR exp \
    exp $\to$ ident \
    exp $\to$ const \
    exp $\to$ LPAREN exp RPAREN 

    stmt $\to$ decl SEMI \
    stmt $\to$ ident EQ exp SEMI \
    stmt $\to$ IF LPAREN exp RPAREN stmt \
    stmt $\to$ IF LPAREN exp RPAREN stmt ELSE stmt \
    stmt $\to$ RET exp SEMI \
    stmt $\to$ WHILE LPAREN exp RPAREN stmt \
    stmt $\to$ LBRACE stmts RBRACE

    stmts $\to$ $\epsilon$ \
    stmts $\to$ stmt stmts

    }


### Step 2: Convert CFG to TA $A_{g}$

* $Q_{g}$ = { ident, decl, const, exp, stmt, stmts, $\epsilon$ }
* $F$ = { (INT, 0), (IDENT, 0), (IF, 4), (IF, 6), (TINT, 3), (RET, 2), (WHILE, 4), (SEMI, 1), (LBRACE, 2), (PLUS, 2), (DASH, 2), (STAR, 2), (EQ, 3), (LPARENRPAREN, 1) }
* $I_{g}$ = { stmts }
* $\Delta_{g}$ = {\
    stmts $\to_{(\varepsilon, 1)}$ $\epsilon$ \
    stmts $\to_{(\varepsilon, 2)}$ stmt stmts

    stmt $\to_{(\texttt{SEMI}, 1)}$ decl SEMI \
    stmt $\to_{(\texttt{EQ}, 3)}$ ident EQ exp SEMI 
    
    stmt $\to_{(\texttt{IF}, 4)}$ IF LPAREN exp RPAREN stmt \
    stmt $\to_{(\texttt{IF}, 6)}$ IF LPAREN exp RPAREN stmt ELSE stmt \
    stmt $\to_{(\texttt{RET}, 2)}$ RET exp SEMI \
    stmt $\to_{(\texttt{WHILE}, 4)}$ WHILE LPAREN exp RPAREN stmt \
    stmt $\to_{(\texttt{LBRACE}, 2)}$ LBRACE stmts RBRACE

    decl $\to_{(\texttt{TINT}, 3)}$ TINT ident EQ exp
    
    ident $\to_{(\texttt{IDENT}, 0)}$ $\epsilon$ 
    
    exp $\to_{(\texttt{PLUS}, 2)}$ exp PLUS exp \
    exp $\to_{(\texttt{DASH}, 2)}$ exp DASH exp \
    exp $\to_{(\texttt{STAR}, 2)}$ exp STAR exp \
    exp $\to_{(\varepsilon, 1)}$ ident \
    exp $\to_{(\varepsilon, 1)}$ const \
    exp $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN exp RPAREN 

    const $\to_{(\texttt{INT}, 0)}$ $\epsilon$
    
    }


Note that following terminals are _not_ included in function symbols: ELSE, STRING (unused)


### Step 3: Find $O_{bp}$

* Traverse $\Delta$ to get $L_{Q}$: 
  { (stmts, 0), (stmt, 1), (decl, 2), (ident, 2), (exp, 2), (const, 3) }

* Based on $\Delta$ and $L_{Q}$, get $O_{bp}$: 
  { <($\varepsilon$, 1), 0>,  <($\varepsilon$, 2), 0>,  <(SEMI, 1), 1>,  <(EQ, 3), 1>,  <(IF, 4), 1>,  <(IF, 6), 1>,  <(RET, 2), 1>,  <(WHILE, 4), 1>,  <(LBRACE, 2), 1>,  <(TINT, 3), 2>,  <(IDENT, 0), 2>,  <(PLUS, 2), 2>,  <(DASH, 2), 2>,  <(STAR, 2), 2>,  <($\varepsilon$, 1), 2>,  <(LPARENRPAREN, 1), 2>,  <(INT, 0), 3> }

That is, $O_{bp}$ is order $\to$ symbols mapping
- 0 $\to$ [ ($\varepsilon$, 1), ($\varepsilon$, 2) ]
- 1 $\to$ [ (SEMI, 1), (EQ, 3), (IF, 4), (IF, 6), (RET, 2), (WHILE, 4), (LBRACE, 2) ]
- 2 $\to$ [ (TINT, 3), (PLUS, 2), (DASH, 2), (STAR, 2), ($\varepsilon$, 1), (LPARENRPAREN, 1) ]
  

### Step 4: Interact with user to collect preferences 

* Based on the user preference, collect $O_{tmp}$:
  { <(IF, 4), -1>, <(IF, 6), 0> }


### Step 5: Based on Step 4 and $O_{bp}$, learn $O_{p}$

* Based on Step 4 and $O_{bp}$, compute $O_{p}$: 
- 0 $\to$ [ ($\varepsilon$, 1), ($\varepsilon$, 2), (IF, 4) ]
- 1 $\to$ [ (SEMI, 1), (EQ, 3), (IF, 6), (RET, 2), (WHILE, 4), (LBRACE, 2) ]
- 2 $\to$ [ (TINT, 3), (PLUS, 2), (DASH, 2), (STAR, 2), ($\varepsilon$, 1), (LPARENRPAREN, 1) ]


### Step 6: Learn $A_{r}$

* $Q_{r}$ = { e1, e2, e3, ident, const, $\epsilon$ }
* $F$ = { (INT, 0), (IDENT, 0), (IF, 4), (IF, 6), (TINT, 3), (RET, 2), (WHILE, 4), (SEMI, 1), (LBRACE, 2), (PLUS, 2), (DASH, 2), (STAR, 2), (EQ, 3), (LPARENRPAREN, 1) }
* $I_{r}$ = { stmts }
* $\Delta_{r}$ = {\
    e1 $\to_{(\varepsilon, 1)}$ $\epsilon$ \
    e1 $\to_{(\varepsilon, 2)}$ e1 e1 \
    e1 $\to_{(\texttt{IF}, 4)}$ IF LPAREN e1 RPAREN e1 \
    _e1 $\to_{(\varepsilon, 1)}$ e2_

    e2 $\to_{(\texttt{SEMI}, 1)}$ e2 SEMI \
    e2 $\to_{(\texttt{EQ}, 3)}$ ident EQ e2 SEMI \
    e2 $\to_{(\texttt{IF}, 6)}$ IF LPAREN e2 RPAREN e2 ELSE e2 \
    e2 $\to_{(\texttt{RET}, 2)}$ RET e2 SEMI \
    e2 $\to_{(\texttt{WHILE}, 4)}$ WHILE LPAREN e2 RPAREN e2 \
    e2 $\to_{(\texttt{LBRACE}, 2)}$ LBRACE e2 RBRACE
    _e2 $\to_{(\varepsilon, 1)}$ e3_

    e3 $\to_{(\texttt{TINT}, 3)}$ TINT ident EQ e3 \
    e3 $\to_{(\texttt{PLUS}, 2)}$ e3 PLUS e3 \
    e3 $\to_{(\texttt{DASH}, 2)}$ e3 DASH e3 \
    e3 $\to_{(\texttt{STAR}, 2)}$ e3 STAR e3 \
    e3 $\to_{(\varepsilon, 1)}$ ident \
    e3 $\to_{(\varepsilon, 1)}$ const \
    e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e3 RPAREN \
    _e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN e1 RPAREN_
    
    _ident $\to_{(\texttt{IDENT}, 0)}$ $\epsilon$_ \
    _const $\to_{(\texttt{INT}, 0)}$ $\epsilon$_
    
    }


### Step 7: Take intersection of the tree automata ($A_{g} \cap A_{r}$)

* $Q$ = { stmts_e1, stmt_e1, decl_e2, exp_e2, exp_e1, stmt_e2, stmts_e2, exp_e3, ident_ident, const_const }
* $I$ = { stmts_e1 }
* $\Delta$ = {\
    **stmts_e1** $\to_{(\varepsilon, 1)}$ $\epsilon$ \
    stmts_e1 $\to_{(\varepsilon, 2)}$ **stmt_e1** stmts_e1 

    **stmt_e1** $\to_{(\texttt{SEMI}, 1)}$ **decl_e2** SEMI \
    stmt_e1 $\to_{(\texttt{EQ}, 3)}$ ident_ident EQ **exp_e2** SEMI \
    stmt_e1 $\to_{(\texttt{IF}, 4)}$ IF LPAREN **exp_e1** RPAREN stmt_e1 \
    stmt_e1 $\to_{(\texttt{IF}, 6)}$ IF LPAREN exp_e2 RPAREN **stmt_e2** ELSE stmt_e2 \
    stmt_e1 $\to_{(\texttt{RET}, 2)}$ RET exp_e2 SEMI \
    stmt_e1 $\to_{(\texttt{WHILE}, 4)}$ WHILE LPAREN exp_e2 RPAREN stmt_e2 \
    stmt_e1 $\to_{(\texttt{LBRACE}, 2)}$ LBRACE **stmts_e2** RBRACE

    **decl_e2** $\to_{(\texttt{TINT}, 3)}$ TINT ident_ident EQ **exp_e3** 

    **exp_e2** $\to_{(\texttt{PLUS}, 2)}$ exp_e3 PLUS exp_e3 \
    exp_e2 $\to_{(\texttt{DASH}, 2)}$ exp_e3 DASH exp_e3 \
    exp_e2 $\to_{(\texttt{STAR}, 2)}$ exp_e3 STAR exp_e3 \
    exp_e2 $\to_{(\varepsilon, 1)}$ ident_ident \
    exp_e2 $\to_{(\varepsilon, 1)}$ const_const \
    exp_e2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN exp_e3 RPAREN \
    exp_e2 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN exp_e1 RPAREN

    **exp_e1** $\to_{(\texttt{PLUS}, 2)}$ exp_e3 PLUS exp_e3 \
    exp_e1 $\to_{(\texttt{DASH}, 2)}$ exp_e3 DASH exp_e3 \
    exp_e1 $\to_{(\texttt{STAR}, 2)}$ exp_e3 STAR exp_e3 \
    exp_e1 $\to_{(\varepsilon, 1)}$ ident_ident \
    exp_e1 $\to_{(\varepsilon, 1)}$ const_const \
    exp_e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN exp_e3 RPAREN \
    exp_e1 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN exp_e1 RPAREN

    **stmt_e2** $\to_{(\texttt{SEMI}, 1)}$ decl_e2 SEMI \
    stmt_e2 $\to_{(\texttt{EQ}, 3)}$ ident_ident EQ exp_e2 SEMI \
    stmt_e2 $\to_{(\texttt{IF}, 6)}$ IF LPAREN exp_e2 RPAREN stmt_e2 ELSE stmt_e2 \
    stmt_e2 $\to_{(\texttt{RET}, 2)}$ RET exp_e2 SEMI \
    stmt_e2 $\to_{(\texttt{WHILE}, 4)}$ WHILE LPAREN exp_e2 RPAREN stmt_e2 \
    stmt_e2 $\to_{(\texttt{LBRACE}, 2)}$ LBRACE stmts_e2 RBRACE

    **stmts_e2** $\to_{(\varepsilon, 1)}$ $\epsilon$

    **exp_e3** $\to_{(\texttt{PLUS}, 2)}$ exp_e3 PLUS exp_e3 \
    exp_e3 $\to_{(\texttt{DASH}, 2)}$ exp_e3 DASH exp_e3 \
    exp_e3 $\to_{(\texttt{STAR}, 2)}$ exp_e3 STAR exp_e3 \
    exp_e3 $\to_{(\varepsilon, 1)}$ ident_ident \
    exp_e3 $\to_{(\varepsilon, 1)}$ const_const \
    exp_e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN exp_e3 RPAREN \
    exp_e3 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN exp_e1 RPAREN
    
    _ident_ident $\to_{(\texttt{IDENT}, 0)}$ $\epsilon$_ \
    _const_const $\to_{(\texttt{INT}, 0)}$ $\epsilon$_
    
    }


### Step 8: Identify a list of duplicate state pairs

* { (exp_e2, exp_e1), (exp_e2, exp_e3) }

### Step 9: Remove duplicate states and rename states

* $Q$ = { stmts_e1, stmt_e1, decl_e2, exp_e2, stmts_e2, ident, const }
  - Rename maps:
    + stmts_e1 $\to$ e1
    + stmt_e1 $\to$ x1
    + stmt_e2 $\to$ x2
    + decl_e2 $\to$ x3
    + exp_e2 $\to$ x4
    + stmts_e2 $\to$ x5

* $I$ = { stmts_e1 }

* $\Delta$ = {\
    e1 $\to_{(\varepsilon, 1)}$ $\epsilon$ \
    e1 $\to_{(\varepsilon, 2)}$ x1 e1 

    x1 $\to_{(\texttt{SEMI}, 1)}$ x3 SEMI \
    x1 $\to_{(\texttt{EQ}, 3)}$ ident EQ x4 SEMI \
    x1 $\to_{(\texttt{IF}, 4)}$ IF LPAREN x4 RPAREN x1 \
    x1 $\to_{(\texttt{IF}, 6)}$ IF LPAREN x4 RPAREN x2 ELSE x2 \
    x1 $\to_{(\texttt{RET}, 2)}$ RET x4 SEMI \
    x1 $\to_{(\texttt{WHILE}, 4)}$ WHILE LPAREN x4 RPAREN x2 \
    x1 $\to_{(\texttt{LBRACE}, 2)}$ LBRACE x5 RBRACE

    x3 $\to_{(\texttt{TINT}, 3)}$ TINT ident EQ x4

    x4 $\to_{(\texttt{PLUS}, 2)}$ x4 PLUS x4 \
    x4 $\to_{(\texttt{DASH}, 2)}$ x4 DASH x4 \
    x4 $\to_{(\texttt{STAR}, 2)}$ x4 STAR x4 \
    x4 $\to_{(\varepsilon, 1)}$ ident \
    x4 $\to_{(\varepsilon, 1)}$ const \
    x4 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x4 RPAREN \
    x4 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x4 RPAREN

    x2 $\to_{(\texttt{SEMI}, 1)}$ x3 SEMI \
    x2 $\to_{(\texttt{EQ}, 3)}$ ident EQ x4 SEMI \
    x2 $\to_{(\texttt{IF}, 6)}$ IF LPAREN x4 RPAREN x2 ELSE x2 \
    x2 $\to_{(\texttt{RET}, 2)}$ RET x4 SEMI \
    x2 $\to_{(\texttt{WHILE}, 4)}$ WHILE LPAREN x4 RPAREN x2 \
    x2 $\to_{(\texttt{LBRACE}, 2)}$ LBRACE stmts_e2 RBRACE

    x5 $\to_{(\varepsilon, 1)}$ $\epsilon$
    
    _ident $\to_{(\texttt{IDENT}, 0)}$ $\epsilon$_ \
    _const $\to_{(\texttt{INT}, 0)}$ $\epsilon$_
    
    }


### Step 10: Introduce epsilon transitions to simplify the transitions

* $Q$ = { e1, x1, x2, x3, x4, x5, ident, const, $\epsilon$ }

* $I$ = { e1 }

 $\Delta$ = {\
    e1 $\to_{(\varepsilon, 1)}$ x5 \
    e1 $\to_{(\varepsilon, 2)}$ x1 e1 

    x1 $\to_{(\texttt{IF}, 4)}$ IF LPAREN x4 RPAREN x1 \
    x1 $\to_{(\varepsilon, 1)}$ x2

    x3 $\to_{(\texttt{TINT}, 3)}$ TINT ident EQ x4

    x4 $\to_{(\texttt{PLUS}, 2)}$ x4 PLUS x4 \
    x4 $\to_{(\texttt{DASH}, 2)}$ x4 DASH x4 \
    x4 $\to_{(\texttt{STAR}, 2)}$ x4 STAR x4 \
    x4 $\to_{(\varepsilon, 1)}$ ident \
    x4 $\to_{(\varepsilon, 1)}$ const \
    x4 $\to_{(\texttt{LPARENRPAREN}, 1)}$ LPAREN x4 RPAREN 

    x2 $\to_{(\texttt{SEMI}, 1)}$ x3 SEMI \
    x2 $\to_{(\texttt{EQ}, 3)}$ ident EQ x4 SEMI \
    x2 $\to_{(\texttt{IF}, 6)}$ IF LPAREN x4 RPAREN x2 ELSE x2 \
    x2 $\to_{(\texttt{RET}, 2)}$ RET x4 SEMI \
    x2 $\to_{(\texttt{WHILE}, 4)}$ WHILE LPAREN x4 RPAREN x2 \
    x2 $\to_{(\texttt{LBRACE}, 2)}$ LBRACE stmts_e2 RBRACE

    x5 $\to_{(\varepsilon, 1)}$ $\epsilon$
    
    _ident $\to_{(\texttt{IDENT}, 0)}$ $\epsilon$_ \
    _const $\to_{(\texttt{INT}, 0)}$ $\epsilon$_
    
    }



