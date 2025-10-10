# Implementation and Testing

### Implementation

- [x] **Converter from CFG to TA** 
     - [x] Start from cleaner data structures (CFG, TA)
     - [x] Learning O_bp

- [x] **Tree example generation**
     - [x] Correctly populate symbols (ID) in trees

- [ ] **TA Learner from tree examples** 
     - [x] Update O_a wrt. trees - _Index-based; collect negative examples with index_
     - [x] Update O_p wrt. trees - _Logic changed from earlier version, updated O_p wrt. O_a_ 
     - [x] Update O_p wrt. O_a as needed - _Works for multiple orders per sym_
     - [x] Learn TA wrt. O_p and O_a - _Learn wrt. O_a_negatives, w/o triv opt_
     - [x] High to low production detection 
     - [ ] Update Learner by maintaining reg. high-to-low case in Op learning - Gokul WIP

- [x] **Intersection** 
     - [x] Update based on revised data structures
     - [x] Renaming to be indicative of original grammar nonterminals
     - [x] Simplify based on linkage between states - _Considered part of epsilon intro_ 
           - I.e., if e0 -> e1 and transitions repeat with only these differing, then simplify with e0

- [x] **Converter from TA to CFG** - (_Skip at Impl level_)

- [x] **Writing the resulted CFG (TA) back to .mly file**

### Manual Initial Testing 

(6 existing + 2-3 Real Grammars)
- [x] Initial manual testing on subset - _Yunjeong WIP_

- [x] Grammar 0 (Running paper example) - for demo only (1 variation)
- [x] Grammar 1 (Earlier G0 case) - constructed for testing (3 variations)
- [ ] Grammar 2 (Compiler - Boolean language) 
- [ ] Grammar 3 (Compiler)
- [ ] Grammar 4 (Compiler)
- [ ] Grammar 5 (StackOverflow)
- [ ] Grammar 6 (StackOVerflow)
- [ ] Grammar 7 (Michelson)
- [ ] Grammar 8 (KaiTai)
- [ ] Grammar 9 (Scilla? - likely to be nonaddressable or SQL)

### Automated Testing 

- [x] Grammar 0 (Running paper example) - for demo only (1 variation)
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



