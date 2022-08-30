# cfg-ta

This project demonstrates how tree automata are used to disambiguate context-free grammar for a toy language (only containing + and * operations) using Menhir grammar.

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


