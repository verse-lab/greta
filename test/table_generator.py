import pandas as pd
import glob
import os
import re
from typing import Dict, List, Tuple

# Statuses to include when calculating intersection time for wo_optx columns
INCLUDED_CASES = ["SUCCESS", "ERROR", "ERROR2", "MENHIR_LIMITATION", "GRETA_UNADDRESSABLE"]

# wo_opt modes to include in the table
WO_OPT_MODES = ["wo_opt1", "wo_opt2", "wo_opt3", "wo_opt123"]

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
        'avg_convert': f"{success_files['convert_time'].mean() * 1000:.2f}",
        'avg_learn': f"{success_files['learn_time'].mean() * 1000:.2f}",
        'avg_intersect': f"{success_files['intersect_time'].mean() * 1000:.2f}"
    }

def get_wo_opt_stats(filepath: str) -> Tuple[float, int]:
    """Calculate average intersection time and % fixed for wo_optx modes.

    Returns:
        Tuple of (avg_intersect_time_ms, percent_fixed) or (None, None) if no data.
    """
    if not os.path.exists(filepath):
        return None, None

    files_data = pd.read_csv(filepath)
    filtered_files = files_data[files_data['status'].isin(INCLUDED_CASES)]

    if len(filtered_files) == 0:
        return None, None

    avg_time = filtered_files['intersect_time'].mean() * 1000  # Convert to ms

    # Calculate % fixed (SUCCESS status only)
    success_count = len(files_data[files_data['status'] == 'SUCCESS'])
    total_count = len(files_data)
    percent_fixed = round(success_count * 100 / total_count) if total_count > 0 else 0

    return avg_time, percent_fixed

def format_time(val) -> str:
    """Format time value for display (2 decimal places)."""
    if val is None or val == 'N/A':
        return 'N/A'
    return f"{val:.2f}"

# Grammar groupings: defines which variants exist for each grammar number
# and whether it's a single-grammar group (no variant letter shown)
GRAMMAR_GROUPS = [
    (0, ['a'], True),   # G0 - single, shown as $G0$
    (1, ['a', 'b', 'c'], False),  # G1a, G1b, G1c with average
    (2, ['a', 'b', 'c'], False),  # G2a, G2b, G2c with average
    (3, ['a', 'b'], False),       # G3a, G3b with average
    (4, ['a', 'b', 'c'], False),  # G4a, G4b, G4c with average
    (5, ['a', 'b', 'c'], False),  # G5a, G5b, G5c with average
    (6, ['a', 'b', 'c'], False),  # G6a, G6b, G6c with average
    (7, ['a'], True),   # G7 - single
    (8, ['a'], True),   # G8 - single
    (9, ['a'], True),   # G9 - single
]

def create_latex_table(filepaths: Dict, wo_opt_filepaths: Dict, terms_map: Dict,
                      nonterms_map: Dict, prods_map: Dict, prec_map: Dict, assoc_map: Dict, ambiguities_map: Dict,
                      manual_time_map: Dict, manual_prod_edits_map: Dict) -> str:
    """Generate LaTeX table with grammar statistics matching paper format.

    Args:
        filepaths: Dict mapping grammar_id to default results.csv path
        wo_opt_filepaths: Dict mapping (grammar_id, mode) to wo_optx results.csv path
                          e.g., {('G1a', 'wo_opt1'): 'path/to/results.csv', ...}
    """
    # Build wo_opt column headers with subscript format for % fixed (gray color)
    def make_wo_opt_header(mode):
        num = mode.replace('wo_opt', '')
        return r"{$I^{\text{" + num + r"}}_{\textcolor{gray}{\%}}$}"
    wo_opt_headers = " & ".join([make_wo_opt_header(mode) for mode in WO_OPT_MODES])
    num_base_cols = 13  # Grammar + 12 data columns (|Σ|, |V|, |P|, Prec, Assoc, Amb, Prompts, ManTime, ManEdits, Conv, Learn, Inter)
    num_wo_opt_cols = len(WO_OPT_MODES)

    latex = r"""\begin{table}[!t]
\scriptsize
\centering
\begin{tabular}{c@{\ \ \ \ \ }""" + "c" * (num_base_cols - 1 + num_wo_opt_cols) + r"""}
\toprule
 & {$|\Sigma|$} & {$|V|$} & {$|P|$} & {Pre} & {Ass} & {Amb} & {$\Delta$} & {Conv} & {Learn} & {$I^{\text{def}}_{\textcolor{gray}{\%}}$} & """ + wo_opt_headers + r""" & {\textcolor{gray}{$T_{\text{man}}$}} & {\textcolor{gray}{$\Delta_{\text{man}}$}} \\
\midrule
"""

    # Track all rows for summary calculation
    all_rows = []

    for g_num, variants, is_single in GRAMMAR_GROUPS:
        group_rows = []

        for variant in variants:
            grammar_id = f"G{g_num}{variant}"

            if grammar_id not in filepaths:
                continue

            filename = filepaths[grammar_id]
            if not os.path.exists(filename):
                continue

            files_data = pd.read_csv(filename)
            stats = process_grammar_files(grammar_id, files_data)

            # Calculate wo_optx intersection times and % fixed (raw values for averaging)
            wo_opt_stats = {}
            for mode in WO_OPT_MODES:
                key = (grammar_id, mode)
                if key in wo_opt_filepaths:
                    time_val, pct_fixed = get_wo_opt_stats(wo_opt_filepaths[key])
                    wo_opt_stats[mode] = {'time': time_val, 'pct_fixed': pct_fixed}
                else:
                    wo_opt_stats[mode] = {'time': None, 'pct_fixed': None}

            row = {
                'grammar_id': grammar_id,
                'g_num': g_num,
                'variant': variant,
                'is_single': is_single,
                'Terms': terms_map.get(grammar_id, 'N/A'),
                'NonTerms': nonterms_map.get(grammar_id, 'N/A'),
                'Prods': prods_map.get(grammar_id, 'N/A'),
                'Precedence': prec_map.get(grammar_id, 'N/A'),
                'Associativity': assoc_map.get(grammar_id, 'N/A'),
                'Ambiguities': ambiguities_map.get(grammar_id, 'N/A'),
                'avg_prompts': stats['avg_prompts'],
                'fixed_count': stats['fixed_count'],
                'unfixed_count': stats['unfixed_count'],
                'avg_convert': stats['avg_convert'],
                'avg_learn': stats['avg_learn'],
                'avg_intersect': stats['avg_intersect'],
                'wo_opt_stats': wo_opt_stats,
                'manual_time': manual_time_map.get(grammar_id, 'N/A'),
                'manual_prod_edits': manual_prod_edits_map.get(grammar_id, 'N/A'),
                # Raw values for summary
                'raw_convert': None if stats['avg_convert'] == 'N/A' else float(stats['avg_convert']),
                'raw_learn': None if stats['avg_learn'] == 'N/A' else float(stats['avg_learn']),
                'raw_intersect': None if stats['avg_intersect'] == 'N/A' else float(stats['avg_intersect']),
                'raw_prompts': None if stats['avg_prompts'] == 'N/A' else float(stats['avg_prompts']),
                'raw_prec': prec_map.get(grammar_id),
                'raw_assoc': assoc_map.get(grammar_id),
                'raw_amb': ambiguities_map.get(grammar_id),
                'raw_manual_time': manual_time_map.get(grammar_id),
                'raw_manual_prod_edits': manual_prod_edits_map.get(grammar_id),
            }
            group_rows.append(row)
            all_rows.append(row)

        if not group_rows:
            continue

        # Output rows for this group
        for i, row in enumerate(group_rows):
            is_last_in_group = (i == len(group_rows) - 1)
            bstrut = r" \Bstrut" if is_last_in_group and not is_single else ""

            if is_single:
                grammar_label = f"$G{row['g_num']}$"
            else:
                grammar_label = f"$G{row['g_num']}${row['variant']}"

            percent_fixed = round(row['fixed_count'] * 100 / (row['fixed_count'] + row['unfixed_count'])) if (row['fixed_count'] + row['unfixed_count']) > 0 else 0

            line = f"{grammar_label} & {row['Terms']} & {row['NonTerms']} & "
            line += f"{row['Prods']} & {row['Precedence']} & {row['Associativity']} & {row['Ambiguities']} & {row['avg_prompts']} & "
            line += f"{row['avg_convert']} & {row['avg_learn']} & ${row['avg_intersect']}_{{\\textcolor{{gray}}{{{percent_fixed}}}}}$"

            # Add wo_optx intersection times with % fixed (gray subscript)
            for mode in WO_OPT_MODES:
                wo_stat = row['wo_opt_stats'].get(mode, {})
                time_val = format_time(wo_stat.get('time'))
                pct_val = wo_stat.get('pct_fixed')
                if time_val == 'N/A' or pct_val is None:
                    line += f" & N/A"
                else:
                    line += f" & ${time_val}_{{\\textcolor{{gray}}{{{pct_val}}}}}$"

            # Add manual columns (gray) at the end
            line += f" & \\textcolor{{gray}}{{{row['manual_time']}}} & \\textcolor{{gray}}{{{row['manual_prod_edits']}}}"

            line += f"{bstrut} \\\\\n"
            latex += line

        # Add average row for multi-variant groups
        if not is_single and len(group_rows) > 1:
            avg_prec = sum(r['raw_prec'] for r in group_rows if r['raw_prec']) / len(group_rows)
            avg_assoc = sum(r['raw_assoc'] for r in group_rows if r['raw_assoc']) / len(group_rows)
            avg_amb = sum(r['raw_amb'] for r in group_rows if r['raw_amb']) / len(group_rows)
            avg_prompts = sum(float(r['avg_prompts']) for r in group_rows if r['avg_prompts'] != 'N/A') / len([r for r in group_rows if r['avg_prompts'] != 'N/A']) if any(r['avg_prompts'] != 'N/A' for r in group_rows) else 'N/A'
            avg_fixed = sum(r['fixed_count'] for r in group_rows) * 100 / sum(r['fixed_count'] + r['unfixed_count'] for r in group_rows) if sum(r['fixed_count'] + r['unfixed_count'] for r in group_rows) > 0 else 0

            valid_converts = [r['raw_convert'] for r in group_rows if r['raw_convert'] is not None]
            valid_learns = [r['raw_learn'] for r in group_rows if r['raw_learn'] is not None]
            valid_intersects = [r['raw_intersect'] for r in group_rows if r['raw_intersect'] is not None]

            avg_convert = format_time(sum(valid_converts) / len(valid_converts)) if valid_converts else 'N/A'
            avg_learn = format_time(sum(valid_learns) / len(valid_learns)) if valid_learns else 'N/A'
            avg_intersect = format_time(sum(valid_intersects) / len(valid_intersects)) if valid_intersects else 'N/A'

            # Average wo_opt times and % fixed
            wo_opt_avgs = {}
            for mode in WO_OPT_MODES:
                valid_times = [r['wo_opt_stats'].get(mode, {}).get('time') for r in group_rows if r['wo_opt_stats'].get(mode, {}).get('time') is not None]
                valid_pcts = [r['wo_opt_stats'].get(mode, {}).get('pct_fixed') for r in group_rows if r['wo_opt_stats'].get(mode, {}).get('pct_fixed') is not None]
                wo_opt_avgs[mode] = {
                    'time': sum(valid_times) / len(valid_times) if valid_times else None,
                    'pct_fixed': int(round(sum(valid_pcts) / len(valid_pcts))) if valid_pcts else None
                }

            # Average manual times and prod edits
            valid_manual_times = [r['raw_manual_time'] for r in group_rows if r['raw_manual_time'] is not None]
            valid_manual_edits = [r['raw_manual_prod_edits'] for r in group_rows if r['raw_manual_prod_edits'] is not None]
            avg_manual_time = int(round(sum(valid_manual_times) / len(valid_manual_times))) if valid_manual_times else 'N/A'
            avg_manual_edits = sum(valid_manual_edits) / len(valid_manual_edits) if valid_manual_edits else 'N/A'

            latex += r"\hdashline\noalign{\vskip 0.5ex}" + "\n"
            line = r"\multicolumn{3}{c}{Average} & & "
            line += f"{int(round(avg_prec))} & {int(round(avg_assoc))} & {int(round(avg_amb))} & "
            line += f"{avg_prompts if avg_prompts == 'N/A' else f'{avg_prompts:.1f}'} & "
            line += f"{avg_convert} & {avg_learn} & ${avg_intersect}_{{\\textcolor{{gray}}{{{int(round(avg_fixed))}}}}}$"

            for mode in WO_OPT_MODES:
                wo_avg = wo_opt_avgs[mode]
                time_val = format_time(wo_avg['time'])
                pct_val = wo_avg['pct_fixed']
                if time_val == 'N/A' or pct_val is None:
                    line += f" & N/A"
                else:
                    line += f" & ${time_val}_{{\\textcolor{{gray}}{{{pct_val}}}}}$"

            # Add manual columns (gray) at the end
            avg_manual_time_str = str(avg_manual_time) if avg_manual_time != 'N/A' else 'N/A'
            avg_manual_edits_str = f'{avg_manual_edits:.1f}' if avg_manual_edits != 'N/A' else 'N/A'
            line += f" & \\textcolor{{gray}}{{{avg_manual_time_str}}} & \\textcolor{{gray}}{{{avg_manual_edits_str}}}"

            line += " \\\\\n"
            latex += line

        latex += r"\midrule" + "\n"

    # Remove the last midrule (replace with nothing before bottomrule)
    latex = latex.rstrip()
    if latex.endswith(r"\midrule"):
        latex = latex[:-len(r"\midrule")]

    latex += r"""\bottomrule
\end{tabular}
\caption{Aggregate results from \tool runs. The table presents the
    number of terminals ($|\Sigma|$), the number of nonterminals ($|V|$),
    the number of productions ($|P|$), the number of example trees \wrt
    precedence order (Pre), the number of example trees \wrt associativity
    (Ass), the total number of ambiguities in each grammar reported by
    \menhir (Amb), the average number of prompts ($\Delta$),
    time spent in converting the grammar to TA in \si{\ms} (Conv),
    time spent for TA learning in \si{\ms} (Learn), TA intersection
    time in \si{\ms} with percentage of scenarios successfully disambiguated
    as subscript ($I^{\text{def}}_{\%}$), TA intersection time in \si{\ms}
    without optimization 1 ($I^{1}_{\%}$), without optimization 2 ($I^{2}_{\%}$),
    without optimization 3 ($I^{3}_{\%}$), and without all three optimizations
    ($I^{123}_{\%}$), each with their respective percentage fixed as subscript,
    total time for manual disambiguation in \si{\s} ($T_{\text{man}}$), and
    number of production edits for manual disambiguation ($\Delta_{\text{man}}$).}
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

# Manual disambiguation
# Grammar	# Ambigs	Total time (sec)	# Rounds	# Prod edits
# G0	5	265	2	9.5
# G1a	4	163	1	6
# G1b	7	202	1.5	7
# G1c	9	169	1	9.5
# G2a	4	294	2	12
# G2b	6	123	2	8
# G2c	12	178	1.5	12
# G3a	3	244	2	14
# G3b	9	209	1	12.5
# G4a	3	123	1.5	3
# G4b	12	176	1.5	10.5
# G4c	16	154	1	10
# G5a	2	80	1	4
# G5b	3	56	1	5
# G5c	6	86	1	8
# G6a	2	84	1	5
# G6b	14	220	1	14.5
# G6c	16	266	1	17
# G7	3	337	1	8.5
# G8	9	305	1.5	6.5
# G9	23	549	1.5	25

manual_time_map = {
    'G0a': 265,
    'G1a': 163,
    'G1b': 202,
    'G1c': 169,
    'G2a': 294,
    'G2b': 123,
    'G2c': 178,
    'G3a': 244,
    'G3b': 209,
    'G4a': 123,
    'G4b': 176,
    'G4c': 154,
    'G5a': 80,
    'G5b': 56,
    'G5c': 86,
    'G6a': 84,
    'G6b': 220,
    'G6c': 266,
    'G7a': 337,
    'G8a': 305,
    'G9a': 549,
}

manual_prod_edits_map = {
    'G0a': 9.5,
    'G1a': 6,
    'G1b': 7,
    'G1c': 9.5,
    'G2a': 12,
    'G2b': 8,
    'G2c': 12,
    'G3a': 14,
    'G3b': 12.5,
    'G4a': 3,
    'G4b': 10.5,
    'G4c': 10,
    'G5a': 4,
    'G5b': 5,
    'G5c': 8,
    'G6a': 5,
    'G6b': 14.5,
    'G6c': 17,
    'G7a': 8.5,
    'G8a': 6.5,
    'G9a': 25,
}

def generate_tables(grammars, wo_opt_grammars=None):
    """Generate LaTeX tables for grammar statistics.

    Args:
        grammars: Dict mapping grammar_id to default results.csv path
        wo_opt_grammars: Dict mapping (grammar_id, mode) to wo_optx results.csv path
                         e.g., {('G1a', 'wo_opt1'): 'path/to/results.csv', ...}
    """
    if wo_opt_grammars is None:
        wo_opt_grammars = {}
    latex_table = create_latex_table(grammars, wo_opt_grammars,
                                terms_map, nonterms_map,
                                prods_map, prec_map, assoc_map, ambiguities_map,
                                manual_time_map, manual_prod_edits_map)
    print(latex_table)
    return latex_table

