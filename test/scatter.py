import matplotlib.pyplot as plt
import pandas as pd
import glob
import os
import numpy as np

def process_grammar_file(filename):
    """Process a single grammar result file and return average times for SUCCESS cases only."""
    df = pd.read_csv(filename)
    # Filter for SUCCESS cases only
    success_df = df[df['status'] == 'SUCCESS']
    
    # If there are no SUCCESS cases, return None
    if len(success_df) == 0:
        return None
        
    # Convert seconds to milliseconds
    return {
        'convert_time': success_df['convert_time'].mean() * 1000,  # Convert to ms
        'learn_time': success_df['learn_time'].mean() * 1000,      # Convert to ms
        'intersect_time': success_df['intersect_time'].mean() * 1000  # Convert to ms
    }

def create_plots(variant, data_dirs, ambiguity_map, output_dir='plots'):
    """Create separate scatter plots for each time metric vs ambiguities and save as SVG files."""
    # Create output directory if it doesn't exist
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Process all grammar files
    results = []
    for data_dir, grammar_ids in data_dirs.items():
        for grammar_id in grammar_ids:
            filename = os.path.join('.', f"{data_dir}/{grammar_id}_results_{variant}/results.csv")
            print(f"Processing file: {filename}")
            if os.path.exists(filename):
                averages = process_grammar_file(filename)
                print(averages)
                if averages is not None:  # Only add if there were SUCCESS cases
                    results.append({
                        'grammar': grammar_id,
                        'ambiguities': ambiguity_map[grammar_id],
                        **averages
                    })
            else:
                print(f"File {filename} does not exist.")

    # Define markers and colors
    markers = ['o', 's', '^', 'D', 'v', '<', '>', 'p', '*', 'h']  # Different marker for each G0-G9
    colors = plt.cm.tab10(np.linspace(0, 1, 10))  # Different color for each G0-G9

    # Function to create a single plot and save as SVG
    def create_single_plot(metric_name, title, filename):
        plt.figure(figsize=(8, 6))
        plt.rcParams.update({'font.size': 16})  # Increases all fonts

        for i, grammar_base in enumerate(['G0', 'G1', 'G2', 'G3', 'G4', 'G5', 'G6', 'G7', 'G9']):
            # Filter results for this grammar base
            grammar_results = [r for r in results if r['grammar'].startswith(grammar_base)]
            if grammar_results:                
                # Sort by ambiguities to ensure proper line connection
                grammar_results_sorted = sorted(grammar_results, key=lambda r: r['ambiguities'])
                
                x = [r['ambiguities'] for r in grammar_results_sorted]
                y = [r[metric_name] for r in grammar_results_sorted]
                
                # Plot both line and scatter
                plt.plot(x, y, color=colors[i], linestyle='-', linewidth=1.5, alpha=0.7)
                plt.scatter(x, y, label=grammar_base, marker=markers[i], color=colors[i], s=80, zorder=5)
        
        plt.xlabel('Number of Ambiguities')
        plt.ylabel('Average Time (ms)')
        plt.title(f'Average {title} vs Ambiguities')
        plt.grid(True, linestyle='--', alpha=0.7)
        plt.legend()
        plt.tight_layout()
        
        # Save as SVG
        pdf_path = os.path.join(output_dir, filename)
        plt.savefig(pdf_path, format='pdf', dpi=300, bbox_inches='tight')
        plt.close()  # Close the figure to free memory
        print(f"Saved plot to {pdf_path}")
    
    # Create separate plots for each metric and save as SVG
    create_single_plot('convert_time', 'Convert Time', 'convert_time_vs_ambiguities.pdf')
    create_single_plot('learn_time', 'Learn Time', 'learn_time_vs_ambiguities.pdf')
    create_single_plot('intersect_time', 'Intersect Time', 'intersect_time_vs_ambiguities.pdf')

# Example usage:
# ambiguity_map = {'G0a': 5,      # 5 shift/reduce conflicts
#                 'G1a': 4,      # 4 shift/reduce conflicts  
#                 'G1b': 7,      # 7 shift/reduce conflicts
#                 'G1c': 9,      # 9 shift/reduce conflicts
#                 'G2a': 4,      # 4 shift/reduce conflicts
#                 'G2b': 6,      # 6 shift/reduce conflicts  
#                 'G2c': 12,     # 12 shift/reduce conflicts
#                 'G3a': 2,      # 2 shift/reduce conflicts
#                 'G3b': 3,      # 3 shift/reduce conflicts
#                 'G3c': 9,      # 9 shift/reduce conflicts
#                 'G4a': 3,      # 3 shift/reduce conflicts
#                 'G4b': 12,     # 12 shift/reduce conflicts
#                 'G4c': 16,     # 16 shift/reduce conflicts
#                 'G5a': 2,      # 2 shift/reduce conflicts
#                 'G5b': 2,      # 2 shift/reduce conflicts
#                 'G5c': 6,      # 6 shift/reduce conflicts
#                 'G6a': 2,      # 2 shift/reduce conflicts
#                 'G6b': 14,     # 14 shift/reduce conflicts
#                 'G6c': 18,     # 18 shift/reduce conflicts
#                 'G7a': 3,      # 3 shift/reduce conflicts
#                 'G8a': 9,      # 9 shift/reduce conflicts
#                 'G9a': 23}     # 23 shift/reduce conflicts

# create_plots('.', ambiguity_map)
