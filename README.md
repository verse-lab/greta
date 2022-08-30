# cfg-ta

This project demonstrates how tree automata (TA) are used to disambiguate context-free grammar (CFG) using Menhir grammar. 

The idea is explained in the paper *Restricting Grammars with Tree Automata* by Michael D. Adams and Matthew Might<sup>[1](#001)</sup>.

Details of the demo scenario is explained in the [examples document](examples.md).

<!-- We are interested in applying ideas from the paper and taking it further by combining it with programming-by-examples synthesis technique. -->

## Project structure

```
.
├── dune
├── dune-project
├── parser.mly
├── converter.ml
├── cfg.ml
├── ast.ml
├── lexer.ml
├── main.ml
└── utils.ml
```

- *parser.mly* - definition of the grammar in menhir

- *ast.ml* - definition of a simple ast

- *lexer.ml* - definition of a lexer

- *utils.ml* - some glue code to hook things together

- *converter.ml* - converters from mly to cfg and vice versa

- *main.ml* - main repl


### Prerequisites

1. You need to install `dune` using `ocaml-dune`:

```
apt install ocaml-dune
```

2. The following packages need to be installed by running `opam install <package>`:

- sedlex
- menhir
- ppx_deriving

3. In order to access the opam installation, run the following:

```
eval $(opam env)
```

4. (Optional) If needed, change the `dune-project` to the right version. The current project runs with the following versions:

```
(lang dune 2.1)
(using menhir 2.0)
```

### Running 

To run the project, do the following:

```
dune exec ./main.exe
```

### References

<a id="1">[1]</a>:
Adams, M. D., & Might, M. (2017). Restricting grammars with tree automata. Proceedings of the ACM on Programming Languages, 1(OOPSLA), 1-25. https://doi.org/10.1145/3133906.
