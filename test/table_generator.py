import pandas as pd
import glob
import os
import re
from typing import Dict, List, Tuple

def count_rounds(grammar_file: str) -> int:
    """Count number of underscores in grammar_file column."""
    return grammar_file.count('_')

def count_prompts(grammar_file: str) -> int:
    """Count number of 1s and 0s after first underscore in grammar_file column."""
    parts = grammar_file.split('_')
    if len(parts) <= 1:
        return 0
    return sum(len(part) for part in parts[1:] if set(part).issubset({'0', '1'}))

def process_grammar_files(grammar_id: str, files_data: pd.DataFrame) -> Dict:
    """Process all files for a given grammar ID."""
    success_files = files_data[files_data['status'] == 'SUCCESS']
    
    if len(success_files) == 0:
        return {
            'rounds_range': 'N/A',
            'avg_rounds': 'N/A',
            'prompts_range': 'N/A',
            'avg_prompts': 'N/A',
            'fixed_count': 0,
            'unfixed_count': len(files_data),
            'avg_convert': 'N/A',
            'avg_learn': 'N/A',
            'avg_intersect': 'N/A'
        }
    
    # Calculate rounds statistics
    success_rounds = success_files['grammar_file'].apply(count_rounds)
    rounds_range = f"{success_rounds.min()}-{success_rounds.max()}"
    avg_rounds = success_rounds.mean()
    
    # Calculate prompts statistics
    success_prompts = success_files['grammar_file'].apply(count_prompts)
    prompts_range = f"{success_prompts.min()}-{success_prompts.max()}"
    avg_prompts = success_prompts.mean()
    
    return {
        'rounds_range': rounds_range,
        'avg_rounds': f"{int(avg_rounds)}",
        'prompts_range': prompts_range,
        'avg_prompts': f"{int(avg_prompts)}",
        'fixed_count': len(success_files),
        'unfixed_count': len(files_data) - len(success_files),
        'avg_convert': f"{int(round(success_files['convert_time'].mean() * 1000000)) / 1000:.3g}",
        'avg_learn': f"{int(round(success_files['learn_time'].mean() * 1000000)) / 1000:.3g}",
        'avg_intersect': f"{int(round(success_files['intersect_time'].mean() * 1000000)) / 1000:.3g}"
    }

def create_latex_table(filepaths: Dict, terms_map: Dict, 
                      nonterms_map: Dict, prods_map: Dict, prec_map: Dict, assoc_map: Dict, ambiguities_map: Dict) -> str:
    """Generate LaTeX table with grammar statistics."""
    results = []
    
    # Process each grammar variant
    for g in range(10):
        for variant in ['a', 'b', 'c', 'd', 'e']:
            grammar_id = f"G{g}{variant}"
            row_grammar = f"$G{g}${variant}"
            
            # Construct full file path
            if grammar_id not in filepaths:
                continue
            
            filename = filepaths[grammar_id]
            # filename = os.path.join(data_dir, f"{grammar_id}_aggregate.csv")
            
            # Check if file existsf"{grammar_id}_aggregate.csv
            if not os.path.exists(filename):
                continue
            
            # Read file
            files_data = pd.read_csv(filename)
            
            stats = process_grammar_files(grammar_id, files_data)
            
            results.append({
                'Grammar': row_grammar,
                'Terms': terms_map.get(grammar_id, 'N/A'),
                'NonTerms': nonterms_map.get(grammar_id, 'N/A'),
                'Prods': prods_map.get(grammar_id, 'N/A'),
                'Precedence': prec_map.get(grammar_id, 'N/A'),
                'Associativity': assoc_map.get(grammar_id, 'N/A'),
                'Ambiguities': ambiguities_map.get(grammar_id, 'N/A'),
                **stats
            })
    
    # Generate LaTeX table
    latex = r"""\begin{table}[htbp]
\centering
\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}
\hline
Grammar & $|Σ|$ & |V| & $|P|$ & Prec. & Assoc. & Amb. & Avg. Rounds & Avg Prompts & % Fixed & Conv. & Learn & Inter. \\
\hline
"""
    for row in results:
        latex += f"{row['Grammar']} & {row['Terms']} & {row['NonTerms']} & "
        latex += f"{row['Prods']} & {row['Precedence']} & {row['Associativity']} & {row['Ambiguities']} & {row['avg_rounds']} & "
        latex += f"{row['avg_prompts']} & {round(row['fixed_count'] * 100 / (row['fixed_count'] + row['unfixed_count']))} & "
        latex += f"{row['avg_convert']} & {row['avg_learn']} & {row['avg_intersect']} \\\\\n"
    
    latex += r"""\hline
\end{tabular}
\caption{Grammar Statistics}
\label{tab:grammar-stats}
\end{table}"""
    return latex

# Example usage:
terms_map = {'G0a': 13,
              'G1a': 11,
              'G1b': 11,
              'G1c': 11,
              'G2a': 10,
              'G2b': 10,
              'G2c': 10,
              'G3a': 17,
              'G3b': 18,
              'G3c': 18,
              'G4a': 25,
              'G4b': 25,
              'G4c': 25,
              'G5a': 8,
              'G5b': 8,
              'G5c': 8,
              'G6a': 21,
              'G6b': 21,
              'G6c': 21,
              'G7a': 56,
              'G8a': 37,
              'G9a': 29}

nonterms_map = {'G0a': 5,
              'G1a': 4,
              'G1b': 4,
              'G1c': 3,
              'G2a': 3,
              'G2b': 3,
              'G2c': 2,
              'G3a': 8,
              'G3b': 9,
              'G3c': 8,
              'G4a': 10,
              'G4b': 8,
              'G4c': 8,
              'G5a': 4,
              'G5b': 4,
              'G5c': 2,
              'G6a': 5,
              'G6b': 5,
              'G6c': 5,
              'G7a': 12,
              'G8a': 32,
              'G9a': 18}

prods_map = {'G0a': 10,
              'G1a': 10,
              'G1b': 10,
              'G1c': 9,
              'G2a': 10,
              'G2b': 10,
              'G2c': 9,
              'G3a': 20,
              'G3b': 21,
              'G3c': 20,
              'G4a': 26,
              'G4b': 24,
              'G4c': 24,
              'G5a': 9,
              'G5b': 9,
              'G5c': 7,
              'G6a': 23,
              'G6b': 23,
              'G6c': 23,
              'G7a': 77,
              'G8a': 72,
              'G9a': 42}

ambiguities_map = {'G0a': 5,      # 5 shift/reduce conflicts
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

prec_map = {'G0a': 2,
              'G1a': 3,
              'G1b': 6,
              'G1c': 6,
              'G2a': 2,
              'G2b': 3,
              'G2c': 6,
              'G3a': 1,
              'G3b': 2,
              'G3c': 4,
              'G4a': 1,
              'G4b': 6,
              'G4c': 6,
              'G5a': 1,
              'G5b': 1,
              'G5c': 3,
              'G6a': 1,
              'G6b': 7,
              'G6c': 8,
              'G7a': 2,
              'G8a': 3,
              'G9a': 7}

assoc_map = {'G0a': 2,
              'G1a': 1,
              'G1b': 1,
              'G1c': 2,
              'G2a': 2,
              'G2b': 2,
              'G2c': 3,
              'G3a': 1,
              'G3b': 1,
              'G3c': 2,
              'G4a': 2,
              'G4b': 3,
              'G4c': 4,
              'G5a': 1,
              'G5b': 1,
              'G5c': 2,
              'G6a': 1,
              'G6b': 4,
              'G6c': 6,
              'G7a': 1,
              'G8a': 3,
              'G9a': 9}


def generate_tables(grammars):
    latex_table = create_latex_table(grammars, 
                                terms_map, nonterms_map, 
                                prods_map, prec_map, assoc_map, ambiguities_map)
    print(latex_table)

