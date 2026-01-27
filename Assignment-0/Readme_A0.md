# Sudoku Solver using SAT (CNF + Z3)

## Overview

This project solves Sudoku puzzles by reducing them to a Boolean Satisfiability (SAT) problem.  
The Sudoku is first encoded into **CNF (Conjunctive Normal Form)**, solved using the **Z3 SAT solver**, and then decoded back into a Sudoku grid.

The solver supports:
- **9×9 Sudoku**
- **16×16 Sudoku (0–F)**

It also includes functionality to check whether a puzzle has a **unique solution** or **multiple solutions**.

---

## Files

- `A0_cnf_generator.ml`  
  Converts the input Sudoku puzzle into CNF (DIMACS format).

- `A0_sol2grid.ml`  
  Converts Z3’s SAT output back into a Sudoku grid.

- `check_multiple.ml`  
  Adds a blocking clause to detect multiple solutions.

- `check_sudoku.ml`  
  Validates the correctness of the solution (provided by TA).

- `Makefile`  
  Automates compilation, solving, validation, and uniqueness checking.

- `input.txt`  
  Input Sudoku puzzle.

- `output.txt`  
  Solved Sudoku grid (generated).

---

## Input Format

### 9×9 Sudoku
- Digits: `1–9`
- Empty cells: `.`

Example:
```
53..7....
6..195...
.98....6.
8...6...3
4..8.3..1
7...2...6
.6....28.
...419..5
....8..79
```

### 16×16 Sudoku
- Digits: `0–9`, `A–F`
- Empty cells: `.`

Example:
```
0.23.56789A.C.EF
4..789.B...F0..3
...
```

---

## Variable Encoding

Each possible assignment is encoded using a direct encoding.

A variable represents:
> “Cell `(i, j)` contains value `v`”

Encoding:
```
var_id(i, j, v) = n² × i + n × j + v
```

Where:
- `n = 9` or `16`
- `i, j` are 1-based indices
- `v ∈ {1..9}` for 9×9
- `v ∈ {0..15}` for 16×16

---

## Constraints Encoded

The CNF generator encodes the following constraints:

1. **Input Constraints**  
   Pre-filled cells are encoded as unit clauses.

2. **Cell Constraints**  
   - Each cell has at least one value  
   - Each cell has at most one value

3. **Row Constraints**  
   - Each value appears at least once per row  
   - No value appears more than once per row

4. **Column Constraints**  
   - Each value appears at least once per column  
   - No value appears more than once per column

5. **Block Constraints**  
   - Each value appears at least once per block  
   - No value appears more than once per block  
   - Block size is `√n` (3×3 for 9×9, 4×4 for 16×16)

---

## Output Format

- **9×9**: digits `1–9`
- **16×16**: `0–9` and `A–F`
- Empty cells (if any) are printed as `.`

If the puzzle has no solution, the output is:
```
No solution
```

---

## Usage

### Compile

```bash
make all
```

---

### Solve the Puzzle

```bash
make run
```

This:
1. Reads `input.txt`
2. Generates `problem.cnf`
3. Runs Z3 on the CNF
4. Produces `output.txt` with the solved grid

---

### Validate the Solution

```bash
make check
```

---

### Check for Unique / Multiple Solutions

```bash
make unique
```

Outputs one of:
- `NO SOLUTION`
- `UNIQUE SOLUTION`
- `MULTIPLE SOLUTIONS`

---

## Multiple Solution Detection

1. Solve the Sudoku once using Z3.
2. Extract all positive variables from the satisfying assignment.
3. Construct a blocking clause by negating these variables.
4. Add the blocking clause to the original CNF.
5. Solve again:
   - **SAT** → Multiple solutions
   - **UNSAT** → Unique solution

This logic is implemented in `check_multiple.ml` and automated via the Makefile.

---

## Implementation Notes

- Z3 outputs models using `v` lines (DIMACS format); these are parsed to extract assignments.
- Grid size (9 or 16) is inferred from the maximum variable ID in the SAT output.
- Arrays are used for grid construction for simplicity and clarity.
- Output redirection to CNF files is handled by the Makefile.

---

## Dependencies

- **OCaml** (`ocamlc`)
- **Z3 SAT Solver**

### Installing Z3

- Ubuntu:
  ```bash
  sudo apt-get install z3
  ```
- macOS:
  ```bash
  brew install z3
  ```

---

## Cleaning

```bash
make clean
```

Removes all generated executables, CNF files, SAT outputs, and solution files.
