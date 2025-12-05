# `01-basic`

Minimal Toffee “hello, world”. Builds a tiny tree with a fixed-size container
and a single percentage-based child, then computes layout for both finite and
unbounded available space.

```bash
dune exec ./examples/01-basic/main.exe
```

## Highlights

- Basic usage of `Toffee.new_tree`, `new_leaf`, and `new_with_children`.
- Demonstrates percentage sizing and `justify_content`.
- Shows how `Available_space.Definite` vs. max-content constraints affect layout.

