# Assignment 2a: BIGNUM — An Arbitrary-Precision Integer Package in OCaml

## Overview

This module implements a **BIGNUM** package in OCaml that supports arithmetic operations on arbitrarily large integers. Numbers are represented using lists of digits, allowing computations far beyond the limits of OCaml's native `int` type.

---

## Data Type Definition

```ocaml
type sign = Neg | NonNeg
type bigint = sign * int list
type myBool = T | F
exception Division_by_zero of string
```

### Representational Invariant

1. **Digit range:** Each element in the `int list` is a single digit between `0` and `9` (inclusive).
2. **Most significant digit first:** Digits are stored with the most significant digit at the head of the list.  
   - Example: `123` is represented as `(NonNeg, [1; 2; 3])`
   - Example: `-456` is represented as `(Neg, [4; 5; 6])`
3. **No unnecessary leading zeros:** The first element of the list is always non-zero, except for the number zero itself.
   - Zero is uniquely represented as `(NonNeg, [0])`.
4. **No negative zero:** The value zero always has the sign `NonNeg`. The representation `(Neg, [0])` is never produced by any operation in this module.

---

## Module Functions

### Conversion Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `int_to_bigint` | `int -> bigint` | Converts a native OCaml `int` to a `bigint` |
| `print_answer` | `bigint -> string` | Pretty-prints a `bigint` as a human-readable string (e.g., `"-123"`) |

### Arithmetic Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `add` | `bigint -> bigint -> bigint` | Addition of two bigints |
| `sub` | `bigint -> bigint -> bigint` | Subtraction of two bigints |
| `mult` | `bigint -> bigint -> bigint` | Multiplication of two bigints |
| `div` | `bigint -> bigint -> bigint` | Integer division (truncated toward zero, same as OCaml's native `/`). e.g., `-3/4 = 0`, not `-1` |
| `modulo` | `bigint -> bigint -> bigint` | Remainder after division (same sign as dividend) |

### Unary Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `negate` | `bigint -> bigint` | Negates the sign of a bigint; `negate(0) = 0` |
| `absolute` | `bigint -> bigint` | Returns the absolute value |

### Comparison Operations

All comparisons return `myBool` (`T` or `F`):

| Function | Signature | Description |
|----------|-----------|-------------|
| `equal` | `bigint -> bigint -> myBool` | Equality check |
| `greater_than` | `bigint -> bigint -> myBool` | Greater than |
| `less_than` | `bigint -> bigint -> myBool` | Less than |
| `greater_or_equal` | `bigint -> bigint -> myBool` | Greater than or equal |
| `less_or_equal` | `bigint -> bigint -> myBool` | Less than or equal |

---

## Exceptions

| Exception | Description |
|-----------|-------------|
| `Division_by_zero of string` | Raised when dividing or taking modulo by zero |

---

## Internal Design & Helper Functions

### Digit List Reversal

Since the `bigint` type stores digits most-significant-digit first, operations like addition and subtraction (which process digits from the least significant end) require **reversing** the digit lists before processing, and reversing the result back.

- `reverse_list` — tail-recursive list reversal with an accumulator
- `reverse_digits` — extracts and reverses the digit list from a `bigint`

### Leading Zero Removal

- `strip_trailing_zeroes` — removes leading zeros from MSD-first digit lists while preserving `[0]` for the number zero.

### Addition & Subtraction

- `add_helper` — adds two reversed digit lists with a carry accumulator
- `sub_helper` — subtracts two reversed digit lists with a borrow accumulator (assumes first operand ≥ second operand in magnitude)
- `adder` — wraps `add_helper` for two non-negative bigints
- `subtractor` — compares magnitudes, subtracts smaller from larger, and assigns the correct sign

The `add` function dispatches based on the signs of its two operands:

| Signs | Operation |
|-------|-----------|
| `(+, +)` | Direct addition via `adder` |
| `(-, -)` | Add magnitudes, negate result |
| `(+, -)` | Subtract via `subtractor` |
| `(-, +)` | Subtract via `subtractor` |

`sub` is implemented as `add a (negate b)`.

### Multiplication

Uses the **grade-school long multiplication** algorithm:

1. `mult_single` — multiplies a reversed digit list by a single digit (0–9) with carry propagation
2. `padding_zeroes` — appends trailing zeros to a digit list (equivalent to multiplying by a power of 10)
3. `mult_helper` — iterates over each digit of the second operand, computes shifted partial products, and sums them using `List.fold_left add`

The sign of the result is determined by the signs of the operands (same sign → positive, different signs → negative).

### Division & Remainder

Uses the **long division** algorithm processing digits left to right (MSD-first):

1. `find_q` — for a given current value and divisor, finds the largest quotient digit `q` (0–9) such that `divisor × q ≤ current`. Uses `subtractor` to check if the difference is negative.
2. `div_helper` — folds over the dividend's digits, maintaining a running remainder and building quotient digits one at a time. Returns both the quotient and the final remainder.
3. `div` — extracts the quotient and assigns the correct sign
4. `modulo` — extracts the remainder with the same sign as the dividend

### Comparison

- `gt_helper` / `equal_helper` — lexicographic comparison of same-length digit lists
- `greater_than` / `equal` — full comparison handling signs, then lengths, then digit-by-digit
- `less_than`, `greater_or_equal`, `less_or_equal` — built on top of `greater_than` and `equal`

---

## How to Run

### Interpret directly

```bash
ocaml A2_a.ml
```

### Interactive toplevel

```bash
ocaml
# #use "A2_a.ml";;
# BIGNUM.print_answer (BIGNUM.add (BIGNUM.int_to_bigint 123) (BIGNUM.int_to_bigint 456));;
```

---

## Example Usage

```ocaml
let a = BIGNUM.int_to_bigint 123 in
let b = BIGNUM.int_to_bigint 456 in
let c = BIGNUM.int_to_bigint (-50) in

BIGNUM.print_answer (BIGNUM.add a b)         (* "579"    *)
BIGNUM.print_answer (BIGNUM.sub a b)         (* "-333"   *)
BIGNUM.print_answer (BIGNUM.mult a b)        (* "56088"  *)
BIGNUM.print_answer (BIGNUM.div b a)         (* "3"      *)
BIGNUM.print_answer (BIGNUM.modulo b a)      (* "87"     *)
BIGNUM.print_answer (BIGNUM.add a c)         (* "73"     *)
BIGNUM.print_answer (BIGNUM.mult c c)        (* "2500"   *)
BIGNUM.greater_than a b                      (* F        *)
BIGNUM.equal a a                             (* T        *)
```

---

## File Structure

```
Assignment-2/
├── A2_a.ml          # Main source file containing the BIGNUM module
└── README.md        # This file
```