### Notes

How we initially ran experiments

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

|                    | Variations | Dat ready? |       Tested?       | Relevant files                                                                        |
| :-----------------: | :--------: | :--------: | :------------------: | ------------------------------------------------------------------------------------- |
| **Grammar 0** |     5     |     Y     | Before Impl. changes | `parser.mly`   `ast.ml`   `lexer.ml`   `range.ml`   `parseutils.ml` |
| **Grammar 1** |     5     |     Y     |          N          | `parser.mly`   `ast.ml`   `lexer.mll`   `range.ml`   `range.mli`    |
| **Grammar 2** |     5     |    wip    |          N          | `parser.mly`   `ast.ml`   `astlib.ml`   `lexer.mll`   `range.ml`   |
| **Grammar 3** |    wip    |    wip    |          N          | `parser.mly`                                                                      |
| **Grammar 4** |     5     |    wip    |          N          | `parser.mly`                                                                        |
| **Grammar 5** |     -     |     -     |          -          | `parser.mly`                                                                        |
| **Grammar 6** |     -     |     -     |          -          | `parser.mly`                                                                        |
| **Grammar 7** |     -     |     -     |          -          | `parser.mly`                                                                        |
| **Grammar 8** |     -     |     -     |          -          | `parser.mly`                                                                        |
| **Grammar 9** |     -     |     -     |          -          | `parser.mly`                                                                        |
