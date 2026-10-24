# `03-mandelbrot`

Explore the Mandelbrot set with braille subcell rendering, smooth palette
interpolation, and a pointer-aware zoom that keeps the cursor anchored during
scrolling. A help overlay explains the controls in-app.

```bash
dune exec ./matrix/examples/03-mandelbrot/main.exe
```

## Controls

- Arrow keys &mdash; pan the viewport.
- `+` / `-` or mouse wheel &mdash; zoom in/out. Wheel zooms around the cursor.
- Horizontal wheel (or Shift + wheel) &mdash; pan sideways precisely.
- `[` / `]` &mdash; tweak the max iteration count.
- `C` &mdash; cycle palettes; `R` &mdash; reset view; `H` &mdash; hide/show help.
- `Esc` or `Ctrl+C` &mdash; quit.

## Highlights

- Braille-based supersampling (2×4 subcells) for smooth gradients.
- Adaptive layout that keeps the overlay readable on short terminals.
- Demonstrates pointer tracking and fine-grained resize handling.
