# cfg-ta

This project demonstrates how tree automata (TA) are used to disambiguate context-free grammar (CFG) using Menhir grammar. 

Building upon the idea from the paper *Restricting Grammars with Tree Automata* by Michael D. Adams and Matthew Might<sup>[1](#001)</sup>, we introduce Angluin's algorithm and programming-by-examples synthesis framework to formally and automatically generate tree automata -- encoding restrictions -- based on examples provided by language designer.

Details of the demo scenario is explained in the [Wiki page](https://github.com/yunjeong-lee/cfg-ta/wiki).


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

4. (Optional) Ensure that you are using the right `dune` and `menhir` versions in `dune-project`. The current project runs with the following versions:

```
(lang dune 2.1)
(using menhir 2.0)
```

5. (Optional) When you encounter a message "... seems to be compiled with a version of OCaml that is not supported by Merlin", then check the ocaml version via `opam switch list` and select `ocaml.4.14.0` compiler for this project. Make sure you run `eval $(opam env)` after switching to version `4.14.0`.

### Running 

To run the project, do the following:

```
dune exec ./main.exe
```

### References

<a id="1">[1]</a>:
Adams, M. D., & Might, M. (2017). Restricting grammars with tree automata. Proceedings of the ACM on Programming Languages, 1(OOPSLA), 1-25. https://doi.org/10.1145/3133906.
