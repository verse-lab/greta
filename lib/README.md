# Implementation and Testing

### Implementation

- [x] **Converter from CFG to TA** 
     - [x] Start from cleaner data structures (CFG, TA)
     - [x] Learning O_bp

- [x] **Tree example generation**
     - [x] Correctly populate symbols (ID) in trees

- [x] **TA Learner from tree examples** 
     - [x] Update O_a wrt. trees - _Index-based; collect both positive and negatives_
     - [x] Update O_p wrt. trees - _Logic changed from earlier version, updated O_p wrt. O_a_ 
     - [x] Update O_p wrt. O_a as needed - _Works for multiple orders per sym_
     - [x] Learn TA wrt. O_p and O_a - Learn wrt. O_a_negatives

- [ ] **Intersection** - Yunjeong WIP
     - [ ] Update based on revised data structures
     - [ ] Infer Paren transition back to `e_0` (or other state)

- [ ] **Converter from TA to CFG**

- [ ] **Formatter for CFG** - _To reuse as much as possible, may have to modify_
<!-- Test G_paper end-to-end -->

### Testing 

(6 existing + 2-3 Real Grammars)

- [ ] Grammar 0 (Running paper example) - for demo only (1 variation)
- [ ] Grammar 1 (Alternte version of Gp) 
- [ ] Grammar 2 (Compiler - Boolean language)
- [ ] Grammar 3 (Compiler)
- [ ] Grammar 4 (Compiler)
- [ ] Grammar 5 (StackOverflow)
- [ ] Grammar 6 (StackOVerflow)
- [ ] Grammar 7 (Michelson)
- [ ] Grammar 8 (KaiTai)
- [ ] Grammar 9 (Scilla? - likely to be nonaddressable or SQL)


### Room for improvement

* Logic for identifying nonaddreaable ambiguities in tree example
  generation is not exhaustive



