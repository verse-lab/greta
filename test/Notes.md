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

|              | Varia-<br />tions | Data<br />Ready? | Init<br />Learn? | E2E<br />Tested? | Relevant Files                                                                        |
| :----------: | :---------------: | :--------------: | :--------------: | :--------------: | ------------------------------------------------------------------------------------- |
| **G0** |         5         |        Y        |        Y        |      Before      | `parser.mly`   `ast.ml`   `lexer.ml`   `range.ml`   `parseutils.ml` |
| **G1** |         5         |        Y        |        -        |        N        | `parser.mly`   `ast.ml`   `lexer.mll`   `range.ml`   `range.mli`    |
| **G2** |         5         |     *WIP*     |        -        |        N        | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`   |
| **G3** |         -         |        -        |        -        |        -        | `parser.mly`                                                                      |
| **G4** |         -         |        -        |        -        |        -        | `parser.mly`                                                                      |
| **G5** |         -         |        -        |        -        |        -        | `parser.mly`                                                                      |
| **G6** |         -         |        -        |        -        |        -        | `parser.mly`                                                                        |
| **G7** |         -         |        -        |        -        |        -        | `parser.mly`                                                                        |
| **G8** |         -         |        -        |        -        |        -        | `parser.mly`                                                                        |
| **G9** |         -         |        -        |        -        |        -        | `parser.mly`                                                                        |


#### Test Results

The results of the test can be summarized as follows:
