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

|              | Varia-<br />tions | Data<br />Ready? | Init<br />Learn? | E2E<br />Tested? |          Source          | Relevant Files                                                                                            |
| :----------: | :---------------: | :--------------: | :--------------: | :--------------: | :-----------------------: | --------------------------------------------------------------------------------------------------------- |
| **G0** |         5         |        Y        |        Y        |      Before      |           Paper           | `parser.mly`   `ast.ml`   `lexer.ml`   `range.ml`   `parseutils.ml`                     |
| **G1** |         5         |        Y        |        -        |        N        | Compilers<br />coursework | `parser.mly`   `ast.ml`   `lexer.mll`   `range.ml`   `range.mli`                        |
| **G2** |         5         |        Y        |        -        |        N        | Compilers<br />coursework | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`                       |
| **G3** |         5         |       wip       |        -        |        N        | Compilers<br />coursework | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`   `range.mli`    |
| **G4** |         -         |        -        |        -        |        -        |       Stackoverflow       | `parser.mly`                                                                                          |
| **G5** |         -         |        -        |        -        |        -        |       Stackoverflow       | `parser.mly`                                                                                          |
| **G6** |         -         |        -        |        -        |        -        |       Stackoverflow       | `parser.mly`                                                                                            |
| **G7** |         -         |        -        |        -        |        -        |          GitHub          | `parser.mly`                                                                                            |
| **G8** |         -         |        -        |        -        |        -        |          GitHub          | `parser.mly`                                                                                            |
| **G9** |         -         |        -        |        -        |        -        |          GitHub          | `parser.mly`                                                                                            |

#### Test Results

The results of the test can be summarized as follows:
