# `11-counter`

A minimal inline TUI demonstrating `Primary_inline` mode. The counter and item
list render below the prompt line without taking over the full screen, making
it suitable for CLI tools that print incremental output.

```bash
dune exec ./matrix/examples/11-counter/main.exe
```

## Controls

- `+` / `-` &mdash; increment / decrement the counter.
- `R` &mdash; reset to initial state.
- `A` &mdash; add a random item to the list.
- `Q` or `Esc` &mdash; quit.

## Highlights

- Uses `mode:`Primary_inline` so the TUI coexists with shell history.
- Dynamic grid height that grows as items are added.
- Box-drawing helpers for bordered content panels.
