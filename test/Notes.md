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

|              | Tokens | Varia-<br />tions | Data<br />Ready? | Init<br />Learn? | E2E<br />Tested? |              Source              | Relevant Files                                                                                            |
| :----------: | :----: | :---------------: | :--------------: | :--------------: | :--------------: | :------------------------------: | --------------------------------------------------------------------------------------------------------- |
| **G0** |   11   |         5         |        Y        |        Y        |        Y        |              Paper              | `parser.mly`   `ast.ml`   `lexer.ml`   `range.ml`   `parseutils.ml`                     |
| **G1** |   10   |         5         |        Y        |        -        |        Y        |    Compilers<br />coursework    | `parser.mly`   `ast.ml`   `lexer.mll`   `range.ml`   `range.mli`                        |
| **G2** |   18   |         5         |        Y        |        -        |        Y        |    Compilers<br />coursework    | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`                       |
| **G3** |   29   |         5         |        Y        |        -        |        N        |    Compilers<br />coursework    | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`   `range.mli`    |
| **G4** |   31   |         5         |        Y        |        -        |        N        |  Minimal OCaml<br />(MinCaml)  | `parser.mly`   `lexer.mll`   `id.ml`   `syntax.ml`   `type.ml`                          |
| **G5** |   -   |         5         |        Y        |        -        |        N        |          StackOverflow          | `parser.mly`   `lexer.mll`   `ast.mly`   `range.ml`                                         |
| **G6** |   -   |         5         |       wip       |        -        |        N        |     ANSI C Yacc<br />Grammar     | `parser.mly`   `lexer.mll`   `ast.mly`                                                          |
| **G7** |   -   |         -         |        -        |        -        |        -        | YACC grammar<br />for C language | `parser.mly`   `lexer.mll`   `ast.mly`                                                          |
| **G8** |   -   |         -         |        -        |        -        |        -        |    GitHub<br />(Project name)    | `parser.mly`   `lexer.mll`   `ast.mly`                                                          |
| **G9** |   -   |         -         |        -        |        -        |        -        |    GitHub<br />(Project name)    | `parser.mly`   `lexer.mll`   `ast.mly`                                                          |

#### Test Results

The results of the test can be summarized as follows:
