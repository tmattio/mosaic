# Mosaic renderer benchmarks

Run the renderer performance contracts with:

```sh
dune build --profile release @packages/mosaic/bench/bench
```

The transcript workload keeps 1,000 laid-out text rows behind a 24-row scroll
viewport. It guards the frame renderer against invoking text surfaces whose
output cannot reach the grid. Setup constructs and warms the tree outside the
measured region; the benchmark measures a steady-state frame and screen diff.

The reconciler workload re-renders a 500-row keyed list through
`Reconciler.render`, both unchanged (the per-redraw steady state) and rotated
(keyed matching plus child moves). Vnode construction happens inside the
measured region because a TEA view rebuilds its vnodes every frame.

The same alias checks time and allocation for editing, selection, text
layout, highlighting, and the application loop. Promotions and minor
collections, which the frame suites also record, depend on where the minor
heap stands when a sample starts, so their budget is 50%. Each suite checks
against `<suite>.thumper`, which holds one section per machine, and a machine
without a section proposes one through `dune promote`. The alias refuses to
measure outside release, where allocation counts would differ, and a machine
keys its section with `THUMPER_MACHINE` (see
`packages/matrix/bench/README.md`, which also describes the `exact` section
of allocation counts that CI checks). These suites therefore stay out of
`runtest`, where a new machine (a CI runner, a contributor's laptop) would
fail on the proposal.

Highlighting covers 100 KB and 1 MB sources with 1,000 captures: alternating
themes over a retained capture batch, and installing fresh capture batches over
already indexed text. Both cases resolve paint styles inside the measured
region; source and viewport setup are excluded.

Text layout's reference workloads cover short labels, styled long lines, tall
clipped text, dense overlapping highlights, highlights outside the viewport,
horizontal scroll, five widths cycled through the four-entry width cache, and
an edit in the middle of 1 MB. Its allocation budget, like every suite's, is 0.
Three checks that compare with fixed budgets, the same on every machine, run in
`dune runtest`, as do the undo-history checks of text layout and the editor:

- `paint` repaints unchanged 80x20 views and fails if a repaint allocates more
  when the text hides more rows, columns, overlays or wrapped rows. Paint
  allocates per visited row and per entered style run, so equal counts show
  that repeated paint follows the visible area. It prints the cost of one
  visible row and one visible run.
- `memory` lays out 1 MB at width 1, a million rows, and fails above 128 MiB
  retained or a 256 MiB heap peak while a second layout is built.
- `memory-widths` lays out 1 MB of prose at the four widths a text surface
  caches, prints the index, each width's rows and a count-only probe, and fails
  above 24 MiB retained or a 48 MiB heap peak while the text is replaced. The
  measured 20.4 MiB retained and 42.0 MiB peak leave less room than one more
  array per grapheme (7.6 MiB): such an array exceeds both budgets. The peak
  allows the old and the new text's geometry together.
