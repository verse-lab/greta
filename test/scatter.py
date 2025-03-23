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

def create_plots(data_dir, ambiguity_map, output_dir='plots'):
    """Create separate scatter plots for each time metric vs ambiguities and save as SVG files."""
    # Create output directory if it doesn't exist
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Process all grammar files
    results = []
    for grammar in ['G0', 'G1', 'G2', 'G3', 'G5', 'G6']:
        for variant in ['a', 'b', 'c', 'd', 'e']:
            grammar_id = f"{grammar}{variant}"
            filename = os.path.join(data_dir, f"{grammar_id}_aggregate.csv")
            if os.path.exists(filename):
                averages = process_grammar_file(filename)
                if averages is not None:  # Only add if there were SUCCESS cases
                    results.append({
                        'grammar': grammar_id,
                        'ambiguities': ambiguity_map[grammar_id],
                        **averages
                    })
    
    # Define markers and colors
    markers = ['o', 's', '^', 'D', 'v', '<', '>', 'p', '*']
    colors = plt.cm.tab10(np.linspace(0, 1, 6))  # Different color for each G0-G6
    
    # Function to create a single plot and save as SVG
    def create_single_plot(metric_name, title, filename):
        plt.figure(figsize=(8, 6))
        for i, grammar_base in enumerate(['G0', 'G1', 'G2', 'G3', 'G5', 'G6']):
            # Filter results for this grammar base
            grammar_results = [r for r in results if r['grammar'].startswith(grammar_base)]
            if grammar_results:
                # Adjust labels for G5 and G6 as per original code
                display_name = grammar_base
                if grammar_base == 'G5':
                    display_name = 'G4'
                if grammar_base == 'G6':
                    display_name = 'G5'
                
                x = [r['ambiguities'] for r in grammar_results]
                y = [r[metric_name] for r in grammar_results]
                plt.scatter(x, y, label=display_name, marker=markers[i], color=colors[i])
        
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
ambiguity_map = {
    'G0a': 4, 'G0b': 5, 'G0c': 5, 'G0d': 7, 'G0e': 9,
    'G1a': 2, 'G1b': 4, 'G1c': 6, 'G1d': 9, 'G1e': 12,
    'G2a': 1, 'G2b': 3, 'G2c': 4, 'G2d': 5, 'G2e': 10,
    'G3a': 3, 'G3b': 4, 'G3c': 6, 'G3d': 8, 'G3e': 10,
    'G5a': 1, 'G5b': 2, 'G5c': 3, 'G5d': 4, 'G5e': 6,
    'G6a': 2, 'G6b': 3, 'G6c': 6, 'G6d': 8, 'G6e': 13
}

create_plots('.', ambiguity_map)
