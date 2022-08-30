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


### Running 

To run the project, do the following:

```
dune exec ./main.exe
```

You will need the following packages installed (`opam install <package>` to install):

- sedlex
- menhir



