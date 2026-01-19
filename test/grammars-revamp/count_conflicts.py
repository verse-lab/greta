#!/usr/bin/env python3
"""
Run menhir on grammar files and count conflicts
"""

import subprocess
import re
import os

def count_conflicts(grammar_file):
    """Run menhir on a grammar file and count conflicts"""
    try:
        # Run menhir command
        result = subprocess.run(['menhir', '--greta', '--explain', grammar_file], 
                              capture_output=True, text=True)
        
        # Parse output for conflicts
        output = result.stderr + result.stdout
        
        # Count shift/reduce conflicts
        sr_states_match = re.search(r'(\d+) states have shift/reduce conflicts', output)
        sr_conflicts_match = re.search(r'(\d+) shift/reduce conflicts were arbitrarily resolved', output)
        
        # Count reduce/reduce conflicts  
        rr_states_match = re.search(r'(\d+) states have reduce/reduce conflicts', output)
        rr_conflicts_match = re.search(r'(\d+) reduce/reduce conflicts were arbitrarily resolved', output)
        
        sr_conflicts = int(sr_conflicts_match.group(1)) if sr_conflicts_match else 0
        rr_conflicts = int(rr_conflicts_match.group(1)) if rr_conflicts_match else 0
        
        total_conflicts = sr_conflicts + rr_conflicts
        
        return total_conflicts, sr_conflicts, rr_conflicts
        
    except Exception as e:
        print(f"Error processing {grammar_file}: {e}")
        return 0, 0, 0

def main():
    # Grammar files to analyze (G0 to G9)
    grammar_files = [
        'G0/G0a.mly',
        'G1/G1a.mly',
        'G1/G1b.mly', 
        'G1/G1c.mly',
        'G2/G2a.mly',
        'G2/G2b.mly',
        'G2/G2c.mly',
        'G3/G3a.mly',
        'G3/G3b.mly',
        'G3/G3c.mly',
        'G4/G4a.mly',
        'G4/G4b.mly',
        'G4/G4c.mly',
        'G5/G5a.mly',
        'G5/G5b.mly',
        'G5/G5c.mly',
        'G6/G6a.mly',
        'G6/G6b.mly',
        'G6/G6c.mly',
        'G7/G7a.mly',
        'G8/G8a.mly',
        'G9/G9a.mly'
    ]
    
    ambiguities_map = {}
    
    for grammar_file in grammar_files:
        if os.path.exists(grammar_file):
            total, sr, rr = count_conflicts(grammar_file)
            
            # Extract grammar name (e.g., G0a from G0/G0a.mly)
            from pathlib import Path
            grammar_name = Path(grammar_file).stem
            
            ambiguities_map[grammar_name] = total
            
            print(f"{grammar_name}: {total} total conflicts ({sr} shift/reduce, {rr} reduce/reduce)")
        else:
            print(f"File not found: {grammar_file}")
    
    print(f"\nambiguities_map = {ambiguities_map}")

if __name__ == "__main__":
    main()