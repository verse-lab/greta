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
        
    return {
        'convert_time': success_df['convert_time'].mean(),
        'learn_time': success_df['learn_time'].mean(),
        'intersect_time': success_df['intersect_time'].mean()
    }

def create_plots(data_dir, ambiguity_map):
    """Create scatter plots for each time metric vs ambiguities."""
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

    # Create the plots
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(15, 5))
    markers = ['o', 's', '^', 'D', 'v', '<', '>', 'p', '*']
    colors = plt.cm.tab10(np.linspace(0, 1, 6))  # Different color for each G0-G6

    for i, grammar_base in enumerate(['G0', 'G1', 'G2', 'G3', 'G5', 'G6']):
        # Filter results for this grammar base
        grammar_results = [r for r in results if r['grammar'].startswith(grammar_base)]
        if grammar_results:
            if grammar_base == 'G5':
                grammar_base = 'G4'
            if grammar_base == 'G6':
                grammar_base = 'G5'
            x = [r['ambiguities'] for r in grammar_results]
            
            # Convert time plot
            y1 = [r['convert_time'] for r in grammar_results]
            ax1.scatter(x, y1, label=grammar_base, marker=markers[i], color=colors[i])
            
            # Learn time plot
            y2 = [r['learn_time'] for r in grammar_results]
            ax2.scatter(x, y2, label=grammar_base, marker=markers[i], color=colors[i])
            
            # Intersect time plot
            y3 = [r['intersect_time'] for r in grammar_results]
            ax3.scatter(x, y3, label=grammar_base, marker=markers[i], color=colors[i])

    # Customize plots
    for ax, title in zip([ax1, ax2, ax3], ['Convert Time', 'Learn Time', 'Intersect Time']):
        ax.set_xlabel('Number of Ambiguities')
        ax.set_ylabel('Average Time (s)')
        ax.set_title(f'Average {title} vs Ambiguities')
        ax.grid(True, linestyle='--', alpha=0.7)
        ax.legend()

    plt.tight_layout()
    plt.show()

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
