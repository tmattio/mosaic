# `02-life`

Conway's Game of Life unfolds on a toroidal grid with a gently animated
background and a text status bar rendered via `Matrix.Image`. Mouse or keyboard
input edits the world while the simulation keeps ticking.

```bash
dune exec ./matrix/examples/02-life/main.exe
```

## Controls

- `Space` &mdash; pause/resume evolution.
- `C` &mdash; clear all live cells.
- `Esc` or `Ctrl+C` &mdash; quit.
- Left mouse button (click or drag) &mdash; paint live cells under the cursor.
- Resize &mdash; the world stretches while existing cells wrap to the new size.

## Highlights

- Demonstrates hit testing (`ctx.hits`) and SGR mouse tracking.
- Shows how to keep a footer line in sync with the grid height.
- Uses `Matrix.Image` composition helpers for fast background animation.
