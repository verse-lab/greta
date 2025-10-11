#!/usr/bin/env python3

import os
import aggregator
import table_generator

postfix = "251011"
grammars = {
#     "grammars-revamp/G0": ["G0a"],
#     "grammars-revamp/G1": ["G1a", "G1b", "G1c"],
#     "grammars-revamp/G2": ["G2a", "G2b", "G2c"],
#     "grammars-revamp/G3": ["G3a", "G3b", "G3c"],
    # "grammars-revamp/G4": ["G4a", "G4b", "G4c"],
    # "grammars-revamp/G5": ["G5a", "G5b", "G5c"],
    # "grammars-revamp/G6": ["G6a", "G6b", "G6c"],
    "grammars/tezos/kaitai": ["parser"]
}

# for each grammar in grammars, run the harness script, 
# then for newly generated .mly files, run menhir again
# on all of them and repeat the process until no new .mly 
# files are generated

seen_files = []
exist_unseen = True

for folder, variants in grammars.items():
    for variant in variants:
        while exist_unseen:
            exist_unseen = False
                
            cwd = os.getcwd()
            # get all files in `folder`
            files = os.listdir(f"{cwd}/{folder}")
            # filter out .mly files
            files = [f for f in files if f.endswith('.mly') and variant in f ]
            # get rid of .mly extension
            files = [f[:-4] for f in files]

            unseen_files = [f for f in files if f not in seen_files]

            if unseen_files == []:
                continue
            else:
                exist_unseen = True

            for f in unseen_files:
                # check that this file and the previous one (variant_000.mly and variant.mly) are not the same
                if f != variant:
                    previous_file = str.join('_', f.split('_')[:-1])
                    if os.path.exists(f"{folder}/{previous_file}.mly"):
                        print(f"Comparing {f}.mly with {previous_file}.mly")
                        with open(f"{folder}/{f}.mly", 'r') as new_file, open(f"{folder}/{previous_file}.mly", 'r') as old_file:
                            new_content = new_file.read()
                            old_content = old_file.read()
                            if new_content == old_content:
                                print(f"Skipping {f}.mly as it is identical to {previous_file}.mly")
                                os.system(f"echo 'Path,Result,Convert Time,Learn Time,Intersect Time' > {folder}/{f}.csv")
                                os.system(f"echo ',REPEATED_OUTPUT,,,' >> {folder}/{f}.csv")
                                continue

                os.system(f"menhir --greta --explain {folder}/{f}.mly")
                os.system(f"./harness.exp {folder}/{f}")
                os.system(f"mv test_results.csv {folder}/{f}.csv")
            
            seen_files += unseen_files
        
        # move seen files to a new folder {variant}_results
        results_folder = f"{folder}/{variant}_results_{postfix}"
        os.system(f"mkdir -p {results_folder}")
        os.system(f"rm -f {results_folder}/*")
        for f in seen_files:
            os.system(f"mv {folder}/{f}.csv {results_folder}/{f}.csv")
            os.system(f"mv {folder}/{f}.cfg {results_folder}/{f}.cfg")
            os.system(f"mv {folder}/{f}.conflicts {results_folder}/{f}.conflicts")
            os.system(f"mv {folder}/{f}.trees {results_folder}/{f}.trees")

            if (f != variant):
                os.system(f"mv {folder}/{f}.mly {results_folder}/{f}.mly")

        seen_files = []
        exist_unseen = True

# aggregate results
for folder, variants in grammars.items():
    for variant in variants:
        base_path = f"{folder}/{variant}_results_{postfix}"
        if os.path.exists(f"{base_path}/{variant}.csv"):
            aggregator.process_csv_tree(f"{base_path}", f"{variant}.csv")
        else:
            print(f"Base file {base_path}/{variant}.csv does not exist.")

# generate tables
grammars = { grammar: f"{folder}/{grammar}_results_{postfix}/results.csv" for folder, grammars in grammars.items() for grammar in grammars }
table_generator.generate_tables(grammars)

