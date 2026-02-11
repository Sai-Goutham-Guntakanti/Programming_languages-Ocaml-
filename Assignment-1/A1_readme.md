# Assignment 1: Expressions, Substitution, Predicates

## How to Run

```
ocaml a1.ml
```

Or in the toplevel:
```
ocaml
#use "a1.ml";;
```

Then call any function directly, e.g.:
```
check_sig [("f", 2); ("g", 1); ("c", 0)];;
ht (Expression.Node (("f", 2), [| Expression.V "x"; Expression.V "y" |]));;
```

## Type Representations

- `symbol = string * int` — a pair of symbol name and its arity
- `signature = symbol list` — a list of symbols
- `variable = string`
- `exp = V of variable | Node of symbol * (exp array)` — variables are leaves, nodes hold a symbol and an array of child expressions
- `substitution = (variable * exp) array` — an array of (variable, expression) pairs
- `position = int list` — a path from root, where each int is a child index
- `pred_symbol = string * int` — same representation as symbol, but from a disjoint signature
- `pred = T | F | Pred of pred_symbol * (exp array) | Not of pred | And of pred * pred | Or of pred * pred`

## Design Decisions

- Same string with different arities is treated as different symbols, as permitted by the specification.
- Substitution is represented as an array of (variable, expression) pairs. Lookup scans the array linearly. This was chosen to stay consistent with the Array module usage requirement.
- Composition of substitutions is done by constructing a new substitution explicitly rather than composing two traversals. This avoids walking the tree twice when the composed substitution is applied.
- Position is represented as an `int list` where each element is a child index. For example, `[1; 0]` means "go to second child, then first child."
- `edit` returns a new expression using `Array.copy` to avoid mutating the original tree.
- `inplace_subst` mutates the tree directly using array assignment. It cannot handle the case where the root itself is a standalone variable (no parent array to mutate), so it returns `()` in that case.
- `vars` returns a deduplicated array using a custom `dedup` function since OCaml's Array module does not have a built-in deduplication function.

## Module Structure

- `Signature` — types and validation for signatures
- `Expression` — expression type, tree functions (wfexp, ht, size, vars), substitution functions (subst, compose), position functions (edit, inplace_subst)
- `Predicate` — predicate type, well-formedness check (wff), predicate substitution (psubst), and weakest precondition substitution (wp)

All functions are also available at the top level without module prefixes.
