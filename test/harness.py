#!/usr/bin/env python3

import os

grammars = [
    "grammars/G0/G0s",
    # "grammars/G1/G1s",
    # "grammars/G2/G2s",
    # "grammars/G3/G3s",
    # "grammars/G4/G4s",
    # "grammars/G5/G5s",
]

# for each grammar in grammars, run the harness script, 
# then for newly generated .mly files, run menhir again
# on all of them and repeat the process until no new .mly 
# files are generated

seen_files = []
exist_unseen = True

while exist_unseen:
    exist_unseen = False
    for folder in grammars:
        cwd = os.getcwd()
        # get all files in `folder`
        files = os.listdir(f"{cwd}/{folder}")
        # filter out .mly files
        files = [f for f in files if f.endswith('.mly') and "G0a" in f ]
        # get rid of .mly extension
        files = [f[:-4] for f in files]

        unseen_files = [f for f in files if f not in seen_files]

        if unseen_files == []:
            continue
        else:
            exist_unseen = True

        for f in unseen_files:
            os.system(f"menhir --greta --explain {folder}/{f}.mly")
            os.system(f"./harness.exp {folder}/{f}")
            os.system(f"mv test_results.csv {folder}/{f}.csv")
        
        seen_files += unseen_files
