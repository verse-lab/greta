## Examples

In this document, we explain how TAs are used to remove precedence ambiguities from CFG of a small toy language (only containing `+` and `*` operations).

<!-- TODO: mention how .mly file is convereted to CFG -->

### Step 1. CFG $G$ for a toy language

Given that a language has ambiguities in the form of shift/reduce conflicts, the first step involves constructing a CFG $G$ for the language. $G$ is defined as a tuple $(V, \Sigma, S, P)$ where
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

### Step 3. Generate examples based on conflict(s) in the language

We make the [Menhir parser generator](http://gallium.inria.fr/~fpottier/menhir/manual.html) provide explanations on conflicts via `-- inspection --dump --explain` flags. This generates `parser.conflicts` file in `_build/default/` directory.

For the above language, following line in `parser.conflicts` indicate that there are two types of examples that cause shift-reduce conflicts:
```
...
expr MUL expr
         expr . PLUS expr
...
expr PLUS expr
          expr . PLUS expr
...
```

Based on the above lines, we can create following options for the user:
```
     (Option 1)         |      (Option 2)       
                        |                       
        MUL             |         PLUS          
       /   \            |         /  \          
     expr PLUS          |       MUL  expr       
          /  \          |      /  \             
       expr  expr       |    expr expr          
```

### Step 4. The user selects an example

In this step, we let the user --- _aka_ the language designer --- select one example based on their preference. Note that there are two different formats to present examples to the user: _trees_ and _code snippets_. This format will be finalized based on the user study in the future. For simplicity, we assume that we're using the tree-structured examples as shown above.

### Step 5. TA $A'$ encoding restrictions

This example is subsequently fed to the following algorithm to automatically generate a TA $TA'$ encodinig restrictions.

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

### Step 6. TA $A''$ resulted from intersection of $A$ and $A'$

TA $A''$ resulted from taking intersection of $A$ and $A'$ (_i.e._, $A \cap A'$ where $A = (Q, F, S, \Delta)$ and $A' = (Q', F, S', \Delta')$) is defined as a tuple $(Q'', F, S'', \Delta'')$ where:
- $Q''$ is a cross product of $Q$ and $Q'$, _i.e._, $Q X Q'$,
- $S''$ is a cross product of $S$ and $S'$, _i.e._, $S X S'$, and
- $\Delta''$ is a cross product of $\Delta$ and $\Delta'$, _i.e._, $\Delta X \Delta'$.

Based on the above definition of intersection, we can define $A''$ as follows:

```
A'':= (Q'', F, S'', Δ'')

Q'' = { EX, EY, ϵ }
F   = { <+, 2>, <*, 2>, <(), 1>, <N, 1>, <ϵ, 1> }
S'' = { EX }
Δ'' = { EX ->_N ϵ       ;
        EX ->_+ EX EX   ;
        EX ->_* EY EY   ;
        EX ->_() EX     ;
        EY ->_N ϵ       ;
        EY ->_* EY EY   ;
        EY ->_() EX     }
```

<!-- (me) how do you algorithmically encode the epsilon introductions or removal of duplicate rules? -->

By introducing epsilon introductions, the productions $\Delta''$ can be simplified further, as shown below:

```
Δ'' = { EX ->_+ EX EX   ;
        EX ->_ϵ EY      ;
        EY ->_* EY EY   ;
        EY ->_N ϵ       ;
        EY ->_() EX     }
```

### Step 7. Convert TA $A''$ to CFG $G'$

The resulted $A''$ is converted back to CFG $G'$ by unlabeling the productions:

```
G' := (V', Σ', S', P')

V'  = { X, Y, +, *, () }
Σ'  = { N }
S'  = { X }
P'  = { X -> X + X  ;
        X -> Y      ;
        Y -> Y * Y  ;
        Y -> N
        Y -> ( X )  }
```


_(TODO: mention how this CFG gets converted to a new .mly file)_



_(TODO: include further example to illustrate a situation where there are multiple conflicts - e.g., dangling else, etc.)_

