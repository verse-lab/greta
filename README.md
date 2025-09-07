# Greta

This project demonstrates how context-free grammar is disambiguated using tree automata synthesized based on user-provided examples. 

Building upon the idea from the paper *Restricting Grammars with Tree Automata* by Michael D. Adams and Matthew Might<sup>[1](#001)</sup>, we introduce tree automata-learning GRETA (**G**rammar **RE**pair via **T**ree **A**utomata) algorithm and programming-by-examples synthesis framework to formally and automatically resolve CFG ambiguities based on examples provided by the user.



## Project structure

```
.
├── dune
├── dune-project
├── Makefile
├── bin
│   ├── dune
│   └── main.ml                 // main repl
├── lib
│   ├── dune
│   ├── converter.ml            // convertion between mly and cfg and between cfg and tree automata
│   ├── operation.ml            // tree automata intersection operation
│   ├── examples.ml             // conflicts-based example tree generator and random tree generator
│   ├── learner.ml              // greta tree automata-learner
│   ├── utils.ml                // some glue code to hook things together
│   ├── parser.mly              // definition of the grammar in menhir
│   ├── lexer.ml                // definition of a lexer
│   ├── ast.ml                  // definition of an ast
│   ├── cfg.ml                  // definition of cfg
│   ├── ta.ml                   // definition of tree automata and tree
│   ├── run.ml                  // runner of tree automata on tree
│   └── pp.ml                   // pretty printers
└── test
    ├── dune
    └── learner_test.ml
```


### Prerequisites

1. You need to install `dune` using `ocaml-dune`:

   * Linux OS
     ```
     apt install ocaml-dune
     ```

   * Mac OS (using Homebrew)
     - Install Opam:
        ```
        brew install opam
        ```
     - Initialize Opam:
        ```
        opam init
        ```
     - Activate opam environment:
        ```
        eval $(opam env)
        ```
     - Install Dune:
        ```
        opam install dune
        ```


2. The following packages need to be installed by running `opam install <package>`:

   * sedlex, ppx_deriving, num, core, core_unix, qcheck, ppx_deriving_yojson, fileutils

Note: Some of the packages above are needed for testing. 

3. Install [Menhir](https://github.com/verse-lab/menhir/tree/dump-cfg) from the separate attachment.

4. After installing the above packages, in order to access the opam installation, run the following:

```
eval $(opam env)
```

5. (Optional) Ensure that you are using the right `dune` and `menhir` versions in `dune-project`. The current project runs with the following versions:

```
(lang dune 2.1)
(using menhir 2.0)
```

6. (Optional) When you encounter a message "... seems to be compiled with a version of OCaml that is not supported by Merlin", then check the ocaml version via `opam switch list` and select `ocaml.4.14.0` compiler for this project. Make sure you run `eval $(opam env)` after switching to version `4.14.0`.


### Running 

To run the project, do the following and choose your selections:

```
make
```
