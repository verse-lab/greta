# Greta

GRETA (**G**rammar **RE**pair via **T**ree **A**utomata) demonstrates
how ambiguous context-free grammars (CFGs) can be disambiguated using
tree automata synthesized from user-provided examples. 

GRETA introduces tree automata-learning algorithm and
programming-by-examples synthesis framework to _formally and
automatically repair CFG ambiguities_ based on tree examples selected
by the user. The project was originally motivated by the idea from the
paper *Restricting Grammars with Tree Automata* by Michael D. Adams
and Matthew Might<sup>[1](#001)</sup>


## Project structure

```
.
├── dune
├── dune-project
├── Makefile
├── bin
│   ├── dune
│   ├── opt_type.ml             // types for optimization ablations
│   └── main.ml                 // main repl
├── lib
│   ├── dune
│   ├── converter.ml            // convertion between mly and CFG and between CFG and TA
│   ├── operation.ml            // TA intersection operation
│   ├── operation_alt.ml        // TA intersection operation w/o optimizations
│   ├── examples.ml             // tree examples generator
│   ├── learner.ml              // TA-learner
│   ├── utils.ml                // some glue code to hook things together
│   ├── cfg.ml                  // definition of CFG
│   ├── ta.ml                   // definition of TA and tree
│   ├── treeutils.ml            // utilities for TA
│   ├── pp.ml                   // pretty printers
│   ├── parser.mly              // definition of the grammar in menhir
│   ├── lexer.ml                // definition of a lexer
│   └── ast.ml                  // definition of an ast
└── test
    ├── grammars-revamp/        // grammars tested for OOPSLA 2026
    ├── :
    └── dune
```


### Prerequisites

GRETA requires:

- **OCaml** (via `opam`)
- **dune**
- **Menhir** (custom fork with CFG dumping support)
- Some Python tools (for testing)

The instructions below are **OS-specific**.

## Installation

### Linux (Ubuntu / Debian)

#### 1. Install system dependencies

```bash
sudo apt update
sudo apt install -y \
  opam \
  expect \
  python3-pandas \
  python3-matplotlib
```

#### 2. Initialize opam and select an OCaml compiler

```bash
opam init -y
opam switch create 5.1.1
eval $(opam env --switch=5.1.1)
```

#### 3. Install required opam packages

```bash
opam install -y \
  dune \
  sedlex \
  ppx_deriving \
  ppx_deriving_yojson \
  num \
  core \
  core_unix \
  qcheck \
  fileutils \
  stdint \
  graphics
```


### macOS (Homebrew)

#### 1. Install opam

```bash
brew install opam
opam init
eval "$(opam env)"
```

#### 2. Select an OCaml compiler

```bash
opam switch create 5.1.1
eval "$(opam env)"
```

#### 3. Install required opam packages

```bash
opam install -y \
  dune \
  sedlex \
  ppx_deriving \
  ppx_deriving_yojson \
  num \
  core \
  core_unix \
  qcheck \
  fileutils \
  stdint \
  graphics
```

### Menhir (Requred)

GRETA depends on a custom Menhir fork that supports CFG dumping.

1. Clone the [Menhir](https://github.com/verse-lab/menhir/tree/dump-cfg) repository.

2. Build and install it:

```bash
make install
```

Note! This [Menhir](https://github.com/verse-lab/menhir/tree/dump-cfg)
version must be installed before building GRETA.


### Version Notes (Optional)

* GRETA is configured to run with: 

```lisp
(lang dune 2.1)
(using menhir 2.0)
```

* If you encounter Merlin errors such as: 
> ... seems to be compiled with a version of OCaml that is not
> supported by Merlin

switch to OCaml 4.14.0:
```bash
opam switch create 4.14.0
eval $(opam env)
```
You can do the above by `opam switch list` and selecting
`ocaml.4.14.0` compiler for this project. Make sure you run `eval
$(opam env)` after switching to version `4.14.0`.



## Building and Running GRETA

From the project root directory:
```bash
make
```

This command builds the project and launches the GRETA interactive
REPL. Follow the on-screen prompts to select the tree example(s) representing 

You can choose optimization/ablation settings. By default, it runs
with all the optimizations. 

1. Run without reachability-based optimization: 
```bash
make run-wo-opt1
```

2. Run without duplicate removal optimization: 
```bash
make run-wo-opt2
```

3. Run without epsilon introduction optimization: 
```bash
make run-wo-opt3
```

4. Run without any of the 3 optimizations: 
```bash
make run-wo-opt123
```



## Testing

The test suite reproduces the experimental evaluation used in the
OOPSLA 2026 paper.

#### 1. Ensure Menhir is installed

```bash
cd menhir   # path to the custom Menhir fork (already in dump-cfg)
make install
```

#### 2. Run the test harness

```bash
cd ../greta/test
./harness.py
```

#### 3. Generated files

The harness produces the following outputs:

- **Per-grammar results** — For each grammar variant and mode, a results
  folder is created at
  `grammars-revamp/<group>/<variant>_<mode>_results_artifact/` containing
  individual CSV files, `.cfg`, `.conflicts`, `.trees`, and any
  intermediate `.mly` files.
- **Aggregated results** — `results.csv` inside each results folder,
  produced by the aggregator.
- **LaTeX table** — `artifact_table.tex` in the `test/` directory,
  containing the full evaluation table ready for inclusion in a paper.
- **Scatter plots** — `convert_time_vs_ambiguities.pdf`,
  `learn_time_vs_ambiguities.pdf`, and
  `intersect_time_vs_ambiguities.pdf` in the `test/` directory.

### Reference

[1] Michael D. Adams and Matthew Might.
Restricting Grammars with Tree Automata.
