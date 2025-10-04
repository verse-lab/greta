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
        'avg_rounds': f"{avg_rounds:.2f}",
        'prompts_range': prompts_range,
        'avg_prompts': f"{avg_prompts:.2f}",
        'fixed_count': len(success_files),
        'unfixed_count': len(files_data) - len(success_files),
        'avg_convert': f"{int(round(success_files['convert_time'].mean() * 1000000))}",
        'avg_learn': f"{int(round(success_files['learn_time'].mean() * 1000000))}",
        'avg_intersect': f"{int(round(success_files['intersect_time'].mean() * 1000000))}"
    }

def create_latex_table(filepaths: Dict, tokens_map: Dict, terms_map: Dict, 
                      nonterms_map: Dict, prods_map: Dict, ambiguities_map: Dict) -> str:
    """Generate LaTeX table with grammar statistics."""
    results = []
    
    # Process each grammar variant (excluding G4)
    for g in range(7):
        if g == 4:  # Skip G4
            continue
        for variant in ['a', 'b', 'c', 'd', 'e']:
            grammar_id = f"G{g}{variant}"
            
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
                'Grammar': grammar_id,
                'Tokens': tokens_map.get(grammar_id, 'N/A'),
                'Terms': terms_map.get(grammar_id, 'N/A'),
                'NonTerms': nonterms_map.get(grammar_id, 'N/A'),
                'Prods': prods_map.get(grammar_id, 'N/A'),
                'Ambiguities': ambiguities_map.get(grammar_id, 'N/A'),
                **stats
            })
    
    # Generate LaTeX table
    latex = r"""\begin{table}[htbp]
\centering
\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}
\hline
Grammar & $|T|$ & $|P|$ & Amb. & Avg. Rounds & Avg Prompts & % Fixed & Conv. & Learn & Inter. \\
\hline
"""
    for row in results:
        latex += f"{row['Grammar']} & {row['Tokens']} & "
        latex += f"{row['Prods']} & {row['Ambiguities']} & {row['avg_rounds']} & "
        latex += f"{row['avg_prompts']} & {round(row['fixed_count'] * 100 / (row['fixed_count'] + row['unfixed_count']))} & "
        latex += f"{row['avg_convert']} & {row['avg_learn']} & {row['avg_intersect']} \\\\\n"
    
    latex += r"""\hline
\end{tabular}
\caption{Grammar Statistics}
\label{tab:grammar-stats}
\end{table}"""
    return latex

# Example usage:
tokens_map = {'G0a': 11, 'G0b': 11, 'G0c': 11, 'G0d': 11, 'G0e': 11,
            'G1a': 10, 'G1b': 10, 'G1c': 10, 'G1d': 10, 'G1e': 10,
            'G2a': 18, 'G2b': 18, 'G2c': 18, 'G2d': 18, 'G2e': 18,
            'G3a': 25, 'G3b': 25, 'G3c': 25, 'G3d': 25, 'G3e': 25,
            'G5a': 7, 'G5b': 7, 'G5c': 7, 'G5d': 7, 'G5e': 7,
            'G6a': 21, 'G6b': 21, 'G6c': 21, 'G6d': 21, 'G6e': 21}
terms_map = {'G0a': 5, 'G0b': 6, 'G0c': 6, 'G0d': 7, 'G0e': 8,
              'G1a': 5, 'G1b': 6, 'G1c': 6, 'G1d': 7, 'G1e': 8,
              'G2a': 5, 'G2b': 6, 'G2c': 6, 'G2d': 7, 'G2e': 8,
              'G3a': 5, 'G3b': 6, 'G3c': 6, 'G3d': 7, 'G3e': 8,
              'G5a': 5, 'G5b': 6, 'G5c': 6, 'G5d': 7, 'G5e': 8,
              'G6a': 5, 'G6b': 6, 'G6c': 6, 'G6d': 7, 'G6e': 8}
nonterms_map = {'G0a': 3, 'G0b': 4, 'G0c': 4, 'G0d': 5, 'G0e': 6,
                    'G1a': 3, 'G1b': 4, 'G1c': 4, 'G1d': 5, 'G1e': 6,
                    'G2a': 3, 'G2b': 4, 'G2c': 4, 'G2d': 5, 'G2e': 6,
                    'G3a': 3, 'G3b': 4, 'G3c': 4, 'G3d': 5, 'G3e': 6,
                    'G5a': 3, 'G5b': 4, 'G5c': 4, 'G5d': 5, 'G5e': 6,
                    'G6a': 3, 'G6b': 4, 'G6c': 4, 'G6d': 5, 'G6e': 6}
prods_map = {
    'G0a': 11, 'G0b': 11, 'G0c': 11, 'G0d': 11, 'G0e': 10,
    'G1a': 11, 'G1b': 11, 'G1c': 11, 'G1d': 11, 'G1e': 10,
    'G2a': 22, 'G2b': 21, 'G2c': 21, 'G2d': 21, 'G2e': 21,
    'G3a': 25, 'G3b': 25, 'G3c': 25, 'G3d': 25, 'G3e': 25,
    'G5a': 9, 'G5b': 10, 'G5c': 10, 'G5d': 9, 'G5e': 8,
    'G6a': 2, 'G6b': 3, 'G6c': 6, 'G6d': 8, 'G6e': 13
}
ambiguities_map = {
    'G0a': 4, 'G0b': 5, 'G0c': 5, 'G0d': 7, 'G0e': 9,
    'G1a': 2, 'G1b': 4, 'G1c': 6, 'G1d': 9, 'G1e': 12,
    'G2a': 1, 'G2b': 3, 'G2c': 4, 'G2d': 5, 'G2e': 10,
    'G3a': 3, 'G3b': 4, 'G3c': 6, 'G3d': 8, 'G3e': 10,
    'G5a': 1, 'G5b': 2, 'G5c': 3, 'G5d': 4, 'G5e': 6,
    'G6a': 2, 'G6b': 3, 'G6c': 6, 'G6d': 8, 'G6e': 13
}

def generate_tables(grammars):
    latex_table = create_latex_table(grammars, 
                                tokens_map, terms_map, nonterms_map, 
                                prods_map, ambiguities_map)
    print(latex_table)

