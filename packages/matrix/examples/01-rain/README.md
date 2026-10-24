# `01-rain`

A neon downpour of randomly cycling Katakana and digits. The demo rebuilds the
grid every frame, caches styles per streak, and shows how to mix `Matrix.run`
with a lightweight state machine.

```bash
dune exec ./packages/matrix/examples/01-rain/main.exe
```

## Controls

- `Space` &mdash; reseed all columns and request an immediate redraw.
- `Esc` or `Ctrl+C` &mdash; quit.
- Resize the terminal to spawn as many streams as fit on screen.

## Highlights

- Immediate-mode drawing with `Grid.draw_text`.
- Palette blends and memoized `Ansi.Style` values.
- `Matrix.Debug_overlay` over each frame, with frame times and the
  collector's statistics.
