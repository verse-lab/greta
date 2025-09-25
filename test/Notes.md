### Notes

Evaluation

#### Files associated with the Greta

* `learner.ml` - Tree automata learner algorithm
* `operation.ml` -  Tree automata intersection algorithm
* `ta.ml` - Data structure for tree automata
* `cfg.ml` - Data structure for context-free grammar   
* `treeutils.ml` - Utilities for tree automata  
* `converter.ml` - Convert between tree automata and context-free grammar
* `examples.ml` - Example data, example generator
* `runner.ml` - Tree automata run to determine accept/reject
* `pp.ml` - Pretty printers

#### Test Data

|        | Tokens | Varia-<br />tions | Data<br />Ready? | Init<br />Learn? | E2E<br />Tested? | Source |      Relevant Files         |
| :----: | :--: | :----: | :---: | :---: | :------: | :-------------------------------------: | --------------------------------------------------------- |
| **G0** |  11  | 5    |    Y    |   Y   |    Y     |            Paper             | `parser.mly`  `ast.ml`   `lexer.ml`   `range.ml`   `parseutils.ml`   |
| **G1** |  10  | 5    |    Y    |   Y   |    Y     |   Compilers<br />coursework  | `parser.mly`  `ast.ml`   `lexer.mll`   `range.ml`   `range.mli`      |
| **G2** |  18  | 5    |    Y    |   Y   |    Y     |   Compilers<br />coursework  | `parser.mly`  `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`      |
| **G3** |  25  | 5    |    Y    |   Y   |    N     |   Compilers<br />coursework  | `parser.mly`  `ast.ml`   `lexer.mll`   `range.ml`   `range.mli`      |
| **G4** |  31  | 5    |    Y    |   Y   |    N     | Minimal OCaml<br />(MinCaml) | `parser.mly`  `lexer.mll`   `id.ml`   `syntax.ml`   `type.ml`        |
| **G5** |  7   | 5    |    Y    |   Y   |    N     |        StackOverflow         | `parser.mly`  `ast.mly`                                              |
| **G6** |  21  | 5    |    Y    |   Y   |    N     |        StackOverflow         | `parser.mly`  `ast.mly`                                              |
| **G7** |  -  |   -   |    -    |   -   |    -     | Simplified SQL<br />Grammar  | `parser.mly`   `ast.mly`                                             |
| **G8** |  -  |   -   |    -    |   -   |    -     |        ANSI C Grammar        | `parser.mly`   `lexer.mll`   `ast.mly`                               |
| **G9** |  -  |   -   |    -    |   -   |    -     | TestMatch data<br />(Java 5 by T. Parr) | `parser.mly`   `lexer.mll`   `ast.mly`                    |
| **Ga** (Tezos - Michelson)  | - | - | - | - | - | Tezos | `parser.mly` `lexer.mll` `syntax.ml` `mySupport.ml` | 
| **Gb** (Tezos - Kaitai)  | - | - | - | - | - | Tezos | `parser.mly` `lexer.mll` `types.ml` | 
| **Gc** (Scilla) | - | - | Y | - | - | Zilliqa Research | `ScillaParser.mly` `ScillaLexer.mll` `ParserUtil.ml` `ErrorUtils.ml` `MonadUtil.ml` `ScillaUtil.ml` `Literal.ml` `Integer256.ml` `Identifier.ml` `GasCharge.ml` `Syntax.ml` `Type.ml` | 

Note: Check if `dune` is consistent

#### Test Results

The results of the test can be summarized as follows:
