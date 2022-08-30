## Examples

In this demo project, TAs are used to remove precedence ambiguities from CFG of a small toy language (only containing `+` and `*` operations).

### Step 1. CFG $G$ for a toy language

We construct a CFG $G$ for a toy language. $G$ is defined as a tuple $(V, \Sigma, S, P)$ where
- $V$ is a set of non-terminals (_aka_ variables),
- $\Sigma$ is a set of terminals,
- $S$ is a set of start symbols, and
- $P$ is a set of productions.

For our small toy language, initial CFG $G$ is defined with the following tuple:
```
G  := (V, Σ, S, P)

V   = { E, +, *, () }
Σ   = { N }
S   = { E }
P   = { E -> N      ;
        E -> E + E  ;
        E -> E * E  ;
        E -> ( E )  }
```

The $G$ contains an ambiguity regarding shift/reduce conflicts caused by precedence involving `+` and `*`. That is, when there is $n_1 * n_2 + n_3$, it is not sure whether to do perform $n_1 * n_2$ first or $n_2 + n_3$ first. Without specifying precedence, Menhir _arbitrarily_ resolves this conflict, resulting in an incorrect computation of $n_1 * (n_2 + n_3)$.

### Step 2. Mapping CFG $G$ to TA $A$

Once we have a CFG $G$, we convert it to a TA $A$ by labeling productions. $A$ is defined as a tuple $(Q, F, S, \Delta)$ where
- $Q$ is a set of states,
- $F$ is a set of (ranked) constructor labels (_aka_ alphabet),
- $S$ is a set of root states, and
- $\Delta$ is a set of productions.

Note that this is a top-down automata where the $A$ accepts a tree $t$ if it starts from a root state in $S$ and expanding downward can construct the tree $t$.

For our small toy language, initial TA $A$ is defined with the following tuple:

```
A  := (Q, F, S, Δ)

Q   = { E, ϵ }
F   = { <+, 2>, <*, 2>, <(), 1>, <N, 1>, <ϵ, 1> }
S   = { E }
Δ   = { E ->_N ϵ    ;
        E ->_+ E E  ;
        E ->_* E E  ;
        E ->_() E   }
```

TRE that corresponds to the above TA is written as follows:

```
TRE : 
    ( +([],[]) | *([],[]) | ()([]) )* . N()
```

_(TODO: We need to specify intermediate steps here that generate examples based on the shift-reduce conflicts.)_

### Step 3. TA $A'$ encoding restrictions

_(TODO: Here, we need to further clarify an algorithm involved in generating $A'$ based on the example selected by the user.)_

Now we specify a TA $A'$ encoding restrictions as follows:

```
A' := (Q', F, S', Δ')

Q'  = { X, Y, ϵ }
F   = { <+, 2>, <*, 2>, <(), 1>, <N, 1>, <ϵ, 1> }
S'  = { X }
Δ'  = { X ->_+ X X  ;
        X ->_ϵ Y    ;

        Y ->_N ϵ    ;
        Y ->_* Y Y  ;
        Y ->_() X   }
```

TRE that corresponds to the above TA is written as follows:

```
TRE : 
    ( ( +([],[]) )* . ( *([],[]) )* . ( N() | ()([]) ) )*
```

### Step 4. TA $A''$ resulted from intersection of $A$ and $A'$

TA $A''$ resulted from taking intersection of $A$ and $A'$ (_i.e._, $A \cap A'$ where $A = (Q, F, S, \Delta)$ and $A' = (Q', F, S', \Delta')$) is defined as a tuple $(Q'', F, S'', \Delta'')$ where:
- $Q''$ is a cross product of $Q$ and $Q'$, _i.e._, $Q X Q'$,
- $S''$ is a cross product of $S$ and $S'$, _i.e._, $S X S'$, and
- $\Delta''$ is a cross product of $\Delta$ and $\Delta'$, _i.e._, $\Delta X \Delta'$.

_(TODO: Need to double-check whether I can simply define $\Delta''$ as a cross product or not.)_

### Step 5. Convert TA $A''$ to CFG $G'$

The resulted $A''$ is converted back to CFG $G'$ by unlabeling the productions.



