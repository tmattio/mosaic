# `05-measure`

Demonstrates custom measure functions for leaf nodes with attached context. A
flex column contains a text node and an image node; both are measured via
user-provided functions.

```bash
dune exec ./examples/05-measure/main.exe
```

## Highlights

- Uses `new_leaf_with_context` to attach per-node data.
- Implements a custom `measure_function` that inspects node context.
- Shows how `Available_space` drives text wrapping and intrinsic sizing.

