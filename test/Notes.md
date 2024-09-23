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

|              | Tokens | Varia-<br />tions | Data<br />Ready? | Init<br />Learn? | E2E<br />Tested? |           Source           | Tokens                                            | Relevant Files                                                                                            |
| :----------: | :----: | :---------------: | :--------------: | :--------------: | :--------------: | :-------------------------: | ------------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| **G0** |   11   |         5         |        Y        |        Y        |      Before      |            Paper            | +, *, if1, if2,<br />N, B, (, )                   | `parser.mly`   `ast.ml`   `lexer.ml`   `range.ml`   `parseutils.ml`                     |
| **G1** |   10   |         5         |        Y        |        -        |        N        |  Compilers<br />coursework  | and, or, ~, var,<br />->, B, (, )                 | `parser.mly`   `ast.ml`   `lexer.mll`   `range.ml`   `range.mli`                        |
| **G2** |   18   |         5         |        Y        |        -        |        N        |  Compilers<br />coursework  | Int, Str, +, *, -, =,<br />while, ;, (, ), {, } | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`                       |
| **G3** |   29   |         5         |        Y        |        -        |        N        |  Compilers<br />coursework  | G2 tokens, ==,<br />!, ~, if1, if2, etc.          | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`   `range.mli`    |
| **G4** |        |         5         |       wip       |        -        |        -        | GitHub<br />(Menhir tests) |                                                   | `parser.mly`                                                                                          |
| **G5** |        |         -         |        -        |        -        |        -        | GitHub<br />(Menhir tests) |                                                   | `parser.mly`                                                                                          |
| **G6** |        |         -         |        -        |        -        |        -        | GitHub<br />(Project name) |                                                   | `parser.mly`                                                                                            |
| **G7** |        |         -         |        -        |        -        |        -        | GitHub<br />(Project name) |                                                   | `parser.mly`                                                                                            |
| **G8** |        |         -         |        -        |        -        |        -        | Stackoverflow<br />question |                                                   | `parser.mly`                                                                                            |
| **G9** |        |         -         |        -        |        -        |        -        | Stackoverflow<br />question |                                                   | `parser.mly`                                                                                            |

#### Test Results

The results of the test can be summarized as follows:
