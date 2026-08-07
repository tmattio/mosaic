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
