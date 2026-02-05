#!/usr/bin/env python3

import os
import pandas as pd
import aggregator
import table_generator
import scatter

def check_for_timeouts(grammars, postfix, modes=["default"]):
    """Check all grammar result CSVs for timeout cases and print warnings."""
    print("\n=== Checking for timeouts ===")
    found_timeouts = False
    for folder, variants in grammars.items():
        for variant in variants:
            for mode in modes:
                results_path = f"{folder}/{variant}_{mode}_results_{postfix}/{variant}.csv"
                if os.path.exists(results_path):
                    df = pd.read_csv(results_path)
                    if 'Result' in df.columns:
                        timeout_rows = df[df['Result'].str.contains('TIMEOUT', case=False, na=False)]
                        if len(timeout_rows) > 0:
                            found_timeouts = True
                            print(f"⚠️  TIMEOUT found in {variant} ({mode}): {len(timeout_rows)} case(s)")
    if not found_timeouts:
        print("✓ No timeouts found")
    print("=== End timeout check ===\n")
    return found_timeouts

postfix = "2601261040"
grammars = {
    "grammars-revamp/G0": ["G0a"],
    "grammars-revamp/G1": ["G1a", "G1b", "G1c"],
    "grammars-revamp/G2": ["G2a", "G2b", "G2c"],
    "grammars-revamp/G3": ["G3a", "G3b"],
    "grammars-revamp/G4": ["G4a", "G4b", "G4c"],
    "grammars-revamp/G5": ["G5a", "G5b", "G5c"],
    "grammars-revamp/G6": ["G6a", "G6b", "G6c"],
    "grammars-revamp/G7": ["G7a"],
    "grammars-revamp/G8": ["G8a"],
    "grammars-revamp/G9": ["G9a"],
}

intersection_modes = ["default", "wo_opt1", "wo_opt2", "wo_opt3", "wo_opt123"]

# for each grammar in grammars, run the harness script, 
# then for newly generated .mly files, run menhir again
# on all of them and repeat the process until no new .mly 
# files are generated

seen_files = []
exist_unseen = True

for mode in intersection_modes:
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
                # filter to only files with < 2 rounds
                exceeded_rounds = [f for f in files if f.count('_') >= 2]
                for f in exceeded_rounds:
                    print(f"Skipping {f}.mly as it has exceeded the maximum number of rounds")
                    os.system(f"echo 'Path,Result,Convert Time,Learn Time,Intersect Time' > {folder}/{f}.csv")
                    os.system(f"echo ',EXCEEDED_ROUNDS,,,' >> {folder}/{f}.csv")
                    seen_files.append(f)

                files = [f for f in files if f.count('_') < 2]
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
                    os.system(f"./harness.exp {folder}/{f} {mode}")
                    os.system(f"mv test_results.csv {folder}/{f}.csv")
                
                seen_files += unseen_files
            
            # move seen files to a new folder {variant}_results
            results_folder = f"{folder}/{variant}_{mode}_results_{postfix}"
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
for mode in intersection_modes:
    for folder, variants in grammars.items():
        for variant in variants:
            base_path = f"{folder}/{variant}_{mode}_results_{postfix}"
            if os.path.exists(f"{base_path}/{variant}.csv"):
                aggregator.process_csv_tree(f"{base_path}", f"{variant}.csv")
            else:
                print(f"Base file {base_path}/{variant}.csv does not exist.")

# check for timeouts
check_for_timeouts(grammars, postfix, modes=intersection_modes)

# generate tables
default_grammars = { grammar: f"{folder}/{grammar}_default_results_{postfix}/results.csv" for folder, grammar_list in grammars.items() for grammar in grammar_list }
wo_opt_grammars = { (grammar, mode): f"{folder}/{grammar}_{mode}_results_{postfix}/results.csv"
                    for folder, grammar_list in grammars.items()
                    for grammar in grammar_list
                    for mode in ["wo_opt1", "wo_opt2", "wo_opt3", "wo_opt123"] }
table_generator.generate_tables(default_grammars, wo_opt_grammars)

ambiguity_map = {'G0a': 5,     # 5 shift/reduce conflicts
                'G1a': 4,      # 4 shift/reduce conflicts
                'G1b': 7,      # 7 shift/reduce conflicts
                'G1c': 9,      # 9 shift/reduce conflicts
                'G2a': 4,      # 4 shift/reduce conflicts
                'G2b': 6,      # 6 shift/reduce conflicts
                'G2c': 12,     # 12 shift/reduce conflicts
                'G3a': 2,      # 2 shift/reduce conflicts
                'G3b': 3,      # 3 shift/reduce conflicts
                'G3c': 9,      # 9 shift/reduce conflicts
                'G4a': 3,      # 3 shift/reduce conflicts
                'G4b': 12,     # 12 shift/reduce conflicts
                'G4c': 16,     # 16 shift/reduce conflicts
                'G5a': 2,      # 2 shift/reduce conflicts
                'G5b': 2,      # 2 shift/reduce conflicts
                'G5c': 6,      # 6 shift/reduce conflicts
                'G6a': 2,      # 2 shift/reduce conflicts
                'G6b': 14,     # 14 shift/reduce conflicts
                'G6c': 18,     # 18 shift/reduce conflicts
                'G7a': 3,      # 3 shift/reduce conflicts
                'G8a': 9,      # 9 shift/reduce conflicts
                'G9a': 23}     # 23 shift/reduce conflicts

scatter.create_plots(postfix, grammars, ambiguity_map)

