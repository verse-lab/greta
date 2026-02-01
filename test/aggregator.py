import os
import pandas as pd
from pathlib import Path

# ERROR: Failure to read mly & conflicts
# ERROR2: Failure to generate mly
# ERROR3: Empty CSV (might be failure to generate conflicts file / cfg file)

def process_csv_tree(base_dir, base_file):
    results = []
    base_path = Path(base_dir) / base_file
    
    def traverse_path(current_file, path_so_far, times_so_far):
        df = pd.read_csv(current_file)
        
        # Check if contains csv data
        if df.shape[0] == 0:
            results.append({
                'grammar_file': current_file.stem,
                'status': 'ERROR3',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return
        
        # Check if this is a leaf node (contains NO_CONFLICTS)
        if df.shape[0] == 1 and str(df.iloc[0]['Result']).strip() == 'NO_CONFLICTS':
            results.append({
                'grammar_file': current_file.stem,
                'status': 'SUCCESS',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return
        
        # Check if this is a leaf node (contains ERROR)
        if df.shape[0] == 1 and str(df.iloc[0]['Result']).strip() == 'ERROR':
            results.append({
                'grammar_file': current_file.stem,
                'status': 'ERROR',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return

        # Check if this is a timeout
        if df.shape[0] == 1 and str(df.iloc[0]['Result']).strip() == 'TIMEOUT':
            results.append({
                'grammar_file': current_file.stem,
                'status': 'TIMEOUT',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return
        
        # Check if this is a menhir limitation
        if df.shape[0] == 1 and str(df.iloc[0]['Result']).strip() == 'MENHIR_LIMITATION':
            results.append({
                'grammar_file': current_file.stem,
                'status': 'MENHIR_LIMITATION',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return
    
        # Check if this is a greta unaddressable error
        if df.shape[0] == 1 and str(df.iloc[0]['Result']).strip() == 'GRETA_UNADDRESSABLE':
            results.append({
                'grammar_file': current_file.stem,
                'status': 'GRETA_UNADDRESSABLE',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return
        
        # Check if this was a repeated output
        if df.shape[0] == 1 and str(df.iloc[0]['Result']).strip() == 'REPEATED_OUTPUT':
            results.append({
                'grammar_file': current_file.stem,
                'status': 'REPEATED_OUTPUT',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return

        if df.shape[0] == 1 and str(df.iloc[0]['Result']).strip() == 'EXCEEDED_ROUNDS':
            results.append({
                'grammar_file': current_file.stem,
                'status': 'REPEATED_OUTPUT',
                'convert_time': times_so_far.get('convert', 0),
                'learn_time': times_so_far.get('learn', 0),
                'intersect_time': times_so_far.get('intersect', 0),
            })
            return

        # Process each row in the current file
        for _, row in df.iterrows():
            path = str(row['Path']).strip()
            if not path:  # Skip empty paths
                continue

            result = str(row['Result']).strip()

            # If ERROR found, record it and stop this branch
            if result == 'ERROR':
                results.append({
                    'grammar_file': current_file.stem + '_' + path.replace(' ', ''),
                    'status': 'ERROR2',
                    'convert_time': times_so_far.get('convert', 0),
                    'learn_time': times_so_far.get('learn', 0),
                    'intersect_time': times_so_far.get('intersect', 0),
                })
                continue

            # If TIMEOUT found, record it and stop this branch
            if result == 'TIMEOUT':
                results.append({
                    'grammar_file': current_file.stem + '_' + path.replace(' ', ''),
                    'status': 'TIMEOUT',
                    'convert_time': times_so_far.get('convert', 0),
                    'learn_time': times_so_far.get('learn', 0),
                    'intersect_time': times_so_far.get('intersect', 0),
                })
                continue

            # Calculate accumulated times
            new_times = times_so_far.copy()
            new_times['convert'] = new_times.get('convert', 0) + float(row['Convert Time'] if pd.notna(row['Convert Time']) else 0)
            new_times['learn'] = new_times.get('learn', 0) + float(row['Learn Time'] if pd.notna(row['Learn Time']) else 0)
            new_times['intersect'] = new_times.get('intersect', 0) + float(row['Intersect Time'] if pd.notna(row['Intersect Time']) else 0)
            
            # Generate next file name
            path_without_spaces = path.replace(' ', '')
            next_file = current_file.parent / f"{current_file.stem}_{path_without_spaces}.csv"
            
            # If next file exists, traverse it
            if next_file.exists():
                traverse_path(next_file, path_so_far + path_without_spaces, new_times)

    # Start traversal from base file
    traverse_path(base_path, '', {})
    
    # Create and save results DataFrame
    results_df = pd.DataFrame(results)
    output_file = base_path.parent / 'results.csv'
    results_df.to_csv(output_file, index=False)
    print(f"Results saved to {output_file}")

# Example usage:
# process_csv_tree("grammars/G0/G0b_25_09_29", "G0b.csv")
