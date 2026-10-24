# `02-nested`

A small nested layout illustrating how percentage sizes propagate through
multiple levels of the tree. Two equal-width children each contain a fixed-size
leaf, all sharing the same outer container space.

```bash
dune exec ./examples/02-nested/main.exe
```

## Highlights

- Nested containers with percentage sizing.
- Demonstrates how constraints from the root propagate to descendants.
- Prints layouts for all nodes to inspect computed sizes and positions.

