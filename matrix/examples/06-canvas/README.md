# `06-canvas`

A mouse-driven paint program with adjustable brush size, color palette, and
alpha blending. Click and drag to draw on the canvas, right-click to erase.

```bash
dune exec ./matrix/examples/06-canvas/main.exe
```

## Controls

- Left click / drag &mdash; paint with the current brush.
- Right click / drag &mdash; erase (paint black).
- `1`&ndash;`0` &mdash; select a color from the palette.
- `A` / `Z` &mdash; increase / decrease brush alpha.
- `+` / `-` &mdash; increase / decrease brush size.
- `C` &mdash; clear the canvas.
- `P` &mdash; toggle the palette panel.
- `H` &mdash; toggle the help overlay.
- `Esc` &mdash; quit.

## Highlights

- Demonstrates `respect_alpha` mode for alpha-blended cell rendering.
- Persistent canvas grid that survives resizes via `Grid.blit_region`.
- Overlay panels drawn on top of the canvas each frame.
