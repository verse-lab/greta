#!/usr/bin/env python3
"""
Count terminals, nonterminals, and productions in Menhir grammar files
"""

import re
import os
from pathlib import Path

def count_grammar_elements(file_path):
    """Count terminals, nonterminals, and productions in a Menhir grammar file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Count terminals (token declarations)
    terminals = set()
    
    # Find all %token declarations
    for line in content.split('\n'):
        line = line.strip()
        if line.startswith('%token'):
            # Extract token names from the line
            # Remove %token and any type annotations
            tokens_line = re.sub(r'%token\s*(?:<[^>]+>)?', '', line)
            # Split by whitespace and filter out empty strings
            token_names = [t.strip() for t in tokens_line.split() if t.strip()]
            for token in token_names:
                if token and (token[0].isupper() or token == 'EOF'):
                    terminals.add(token)
    
    # Count nonterminals and productions
    # Find the %% section that contains grammar rules
    grammar_section = ""
    lines = content.split('\n')
    in_grammar = False
    
    for line in lines:
        if line.strip() == '%%' and not in_grammar:
            in_grammar = True
            continue
        elif line.strip() == '%%' and in_grammar:
            break
        elif in_grammar:
            grammar_section += line + '\n'
    
    # Parse nonterminals and productions more carefully
    nonterminals = set()
    productions_count = 0
    
    # Remove comments and clean up the grammar section
    grammar_lines = []
    for line in grammar_section.split('\n'):
        # Remove comments
        if '//' in line:
            line = line[:line.index('//')]
        if '/*' in line:
            # Simple comment removal - doesn't handle multi-line
            line = re.sub(r'/\*.*?\*/', '', line)
        grammar_lines.append(line)
    
    grammar_text = '\n'.join(grammar_lines)
    
    # Find nonterminal definitions using regex
    # Look for patterns like "nonterminal:" at the beginning of lines
    nonterminal_pattern = r'^\s*([a-z_][a-z0-9_]*)\s*:'
    
    current_nonterminal = None
    production_count = 0
    
    for line in grammar_lines:
        line = line.strip()
        if not line:
            continue
            
        # Check if this line starts a new nonterminal definition
        match = re.match(nonterminal_pattern, line)
        if match:
            if current_nonterminal:
                # Add the production count for the previous nonterminal
                productions_count += max(1, production_count)  # At least 1 production per nonterminal
            
            current_nonterminal = match.group(1)
            nonterminals.add(current_nonterminal)
            production_count = 0
            
            # Count productions in the rest of this line
            rest_of_line = line[match.end():].strip()
            if rest_of_line:
                production_count = 1
        elif current_nonterminal and line.startswith('|'):
            # This is an alternative production
            production_count += 1
        elif current_nonterminal and line and not line.startswith(';') and not line.endswith(';'):
            # Check if this line has productions (not just semicolons)
            if production_count == 0:
                production_count = 1
    
    # Don't forget the last nonterminal
    if current_nonterminal and production_count == 0:
        production_count = 1
    if current_nonterminal:
        productions_count += production_count
    
    return len(terminals), len(nonterminals), productions_count

def main():
    # Grammar files to analyze
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
        'G9/G9a.mly',
        'G10/G10a.mly',
        'G11/G11a.mly',
        'Gp/Gp-alt.mly',
        'Gp/parser.mly'
    ]
    
    terms_map = {}
    nonterms_map = {}
    prods_map = {}
    
    for grammar_file in grammar_files:
        if os.path.exists(grammar_file):
            terminals, nonterminals, productions = count_grammar_elements(grammar_file)
            
            # Extract grammar name (e.g., G0a from G0/G0a.mly)
            grammar_name = Path(grammar_file).stem
            
            terms_map[grammar_name] = terminals
            nonterms_map[grammar_name] = nonterminals  
            prods_map[grammar_name] = productions
            
            print(f"{grammar_name}: {terminals} terminals, {nonterminals} nonterminals, {productions} productions")
    
    print("\nPython dictionaries:")
    print(f"terms_map = {terms_map}")
    print(f"nonterms_map = {nonterms_map}")
    print(f"prods_map = {prods_map}")

if __name__ == "__main__":
    main()