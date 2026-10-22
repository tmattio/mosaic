# Mosaic renderer benchmarks

Run the renderer performance contracts with:

```sh
dune build --profile release @mosaic/bench/bench
```

The transcript workload keeps 1,000 laid-out text rows behind a 24-row scroll
viewport. It guards the frame renderer against invoking text surfaces whose
output cannot reach the grid. Setup constructs and warms the tree outside the
measured region; the benchmark measures a steady-state frame and screen diff.

The reconciler workload re-renders a 500-row keyed list through
`Reconciler.render`, both unchanged (the per-redraw steady state) and rotated
(keyed matching plus child moves). Vnode construction happens inside the
measured region because a TEA view rebuilds its vnodes every frame.

`dune runtest --profile release mosaic/bench` also checks allocation budgets
for editing, selection, text layout, highlighting, and the application loop.
Highlighting covers 100 KB and 1 MB sources with 1,000 captures: alternating
themes over a retained capture batch, and installing fresh capture batches over
already indexed text. Both cases resolve paint styles inside the measured
region; source and viewport setup are excluded.

Text layout's reference workloads cover short labels, styled long lines, tall
clipped text, dense overlapping highlights, highlights outside the viewport,
horizontal scroll, five widths cycled through the four-entry width cache, and
an edit in the middle of 1 MB. Its allocation baseline holds each within 5%.
Three checks run beside it in `dune runtest`:

- `paint` repaints unchanged 80x20 views and fails if a repaint allocates more
  when the text hides more rows, columns, overlays or wrapped rows. Paint
  allocates per visited row and per entered style run, so equal counts show
  that repeated paint follows the visible area. It prints the cost of one
  visible row and one visible run.
- `memory` lays out 1 MB at width 1, a million rows, and fails above 128 MiB
  retained or a 256 MiB heap peak while a second layout is built.
- `memory-widths` lays out 1 MB of prose at the four widths a text surface
  caches, prints the index, each width's rows and a count-only probe, and fails
  above 32 MiB retained or a 64 MiB heap peak while the text is replaced. The
  budget is the measured 28.1 MiB rounded up to the next power of two: one more
  array per grapheme (8 MiB) exceeds it. The peak allows the old and the new
  text's geometry together.
