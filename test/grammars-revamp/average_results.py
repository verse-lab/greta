#!/usr/bin/env python3
"""
Script to calculate column averages from G0 to G9 grammar results.
"""

# Data from the table
data = [
    # Grammar, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13
    ["G0", 13, 5, 10, 2, 2, 5, 4, 50, 0.057, 0.559, 0.527],
    ["G1a", 11, 4, 10, 3, 1, 4, 4, 14, 0.058, 0.728, 2.31],
    ["G1b", 11, 4, 10, 6, 1, 7, 7, 17, 0.061, 0.842, 0.535],
    ["G1c", 11, 3, 9, 6, 2, 9, 8, 17, 0.055, 1.01, 0.493],
    ["G2a", 10, 3, 10, 2, 2, 4, 4, 100, 0.055, 0.615, 1.84],
    ["G2b", 10, 3, 10, 3, 2, 6, 5, 100, 0.054, 0.587, 0.399],
    ["G2c", 10, 2, 9, 6, 3, 12, 9, 100, 0.053, 0.933, 0.466],
    ["G3a", 18, 9, 21, 2, 1, 3, 3, 50, 0.132, 0.632, 1.61],
    ["G3b", 18, 8, 20, 4, 2, 9, 6, 50, 0.101, 0.903, 1.15],
    ["G4a", 25, 10, 26, 1, 2, 3, 3, 100, 0.179, 0.787, 1.35],
    ["G4b", 25, 8, 24, 6, 3, 12, 9, 100, 0.123, 1.06, 0.944],
    ["G4c", 25, 8, 24, 6, 4, 16, 10, 100, 0.118, 1.14, 0.934],
    ["G5a", 8, 4, 9, 1, 1, 2, 2, 100, 0.056, 0.334, 0.369],
    ["G5b", 8, 4, 9, 1, 1, 2, 2, 100, 0.06, 0.339, 0.361],
    ["G5c", 8, 2, 7, 3, 2, 6, 5, 100, 0.047, 0.547, 0.334],
    ["G6a", 21, 5, 23, 1, 1, 2, 2, 100, 0.24, 0.726, 1.08],
    ["G6b", 21, 5, 23, 7, 4, 14, 11, 100, 0.161, 1.25, 0.958],
    ["G6c", 21, 5, 23, 8, 6, 18, 14, 100, 0.122, 1.55, 1.21],
    ["G7", 56, 12, 77, 2, 1, 3, 3, 100, 0.47, 3.91, 19.2],
    ["G8", 37, 32, 72, 3, 3, 9, 6, 100, 138, 178, 12.4],
    ["G9", 29, 18, 42, 7, 9, 23, 16, 100, 0.242, 2.13, 2.71],
]

# Calculate averages for each column (excluding grammar name)
num_cols = len(data[0]) - 1  # Exclude first column (grammar name)
num_rows = len(data)

averages = []
for col_idx in range(1, num_cols + 1):
    col_sum = sum(row[col_idx] for row in data)
    col_avg = col_sum / num_rows
    averages.append(col_avg)

# Print results
print("Column averages across all grammars (G0-G9):")
print("=" * 60)
print(f"Column  3: {averages[0]:.2f}")
print(f"Column  4: {averages[1]:.2f}")
print(f"Column  5: {averages[2]:.2f}")
print(f"Column  6: {averages[3]:.2f}")
print(f"Column  7: {averages[4]:.2f}")
print(f"Column  8: {averages[5]:.2f}")
print(f"Column  9: {averages[6]:.2f}")
print(f"Column 10: {averages[7]:.2f}")
print(f"Column 11: {averages[8]:.3f}")
print(f"Column 12: {averages[9]:.3f}")
print(f"Column 13: {averages[10]:.3f}")

print("\n" + "=" * 60)
print("\nFormatted for LaTeX table:")
print("\\hdashline\\noalign{\\vskip 0.5ex}")
print(f"\\multicolumn{{3}}{{c}}{{Overall Average}} & & "
      f"{averages[3]:.0f} & {averages[4]:.0f} & {averages[5]:.0f} & "
      f"{averages[6]:.1f} & {averages[7]:.0f} & {averages[8]:.3f} & "
      f"{averages[9]:.3f} & {averages[10]:.3f} \\\\")
