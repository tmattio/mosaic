# Matrix Benchmarks

Performance benchmarks for the Matrix terminal rendering library components.

This benchmark suite measures the performance of core Matrix library operations, including:
- **Grid operations**: Cell manipulation, text rendering, and grid transformations
- **Screen operations**: Frame lifecycle, rendering, diffing, and viewport management

## `mosaic.grid`

```bash
dune exec matrix/bench/bench_grid.exe
```

```
┌─────────────────────────────┬──────────┬─────────┬─────────┬────────────┐
│ Name                        │ Time/Run │ mWd/Run │ Speedup │ vs Fastest │
├─────────────────────────────┼──────────┼─────────┼─────────┼────────────┤
│ setting/set_grapheme_wide   │   5.29μs │  4.27kw │   1.00x │       100% │
│ clearing/clear_rect         │   5.38μs │  4.27kw │   0.98x │       102% │
│ setting/set_grapheme_narrow │   5.42μs │  4.27kw │   0.98x │       102% │
│ clearing/clear              │   5.46μs │  4.27kw │   0.97x │       103% │
│ composite/scroll            │   5.52μs │  4.27kw │   0.96x │       104% │
│ setting/set_text_long       │   5.58μs │  4.27kw │   0.95x │       105% │
│ setting/set_text_short      │   5.64μs │  4.27kw │   0.94x │       106% │
│ clearing/clear_line         │   5.64μs │  4.27kw │   0.94x │       107% │
│ resizing/resize_larger      │   5.68μs │  4.27kw │   0.93x │       107% │
│ composite/typing            │   5.74μs │  4.27kw │   0.92x │       108% │
│ composite/insert_line       │   6.12μs │  4.27kw │   0.87x │       116% │
│ setting/set_cell            │   7.08μs │  4.27kw │   0.75x │       134% │
│ creation/create_small       │  10.11μs │  4.25kw │   0.52x │       191% │
│ resizing/resize_smaller     │  15.20μs │ 16.10kw │   0.35x │       287% │
│ blitting/blit_small         │  15.91μs │  9.15kw │   0.33x │       301% │
│ blitting/blit_large         │  47.14μs │ 16.58kw │   0.11x │       890% │
│ diffing/diff_single_change  │  47.99μs │ 12.38kw │   0.11x │       906% │
│ diffing/diff_no_change      │  48.62μs │ 12.34kw │   0.11x │       918% │
│ diffing/diff_row_change     │  55.34μs │ 14.44kw │   0.10x │      1045% │
│ diffing/diff_full_change    │ 185.79μs │ 62.27kw │   0.03x │      3509% │
│ creation/create_large       │   3.72ms │ 17.12kw │   0.00x │     70230% │
└─────────────────────────────┴──────────┴─────────┴─────────┴────────────┘
```

## `mosaic.screen`

```bash
dune exec matrix/bench/bench_screen.exe
```

```
┌────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                               │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ Creation/Resizing/create           │  22.32μs │   8.52kw │   1.00x │       100% │
│ Drawing/clear                      │ 360.34μs │  98.08kw │   0.06x │      1615% │
│ Viewport/render_viewport           │ 418.75μs │ 137.17kw │   0.05x │      1876% │
│ Cloning/Copying/clone              │ 556.49μs │ 176.90kw │   0.04x │      2494% │
│ Drawing/set_text                   │ 584.23μs │ 185.13kw │   0.04x │      2618% │
│ Viewport/with_viewport             │ 687.97μs │ 227.74kw │   0.03x │      3083% │
│ Frame Lifecycle/begin_present      │ 733.66μs │ 216.69kw │   0.03x │      3288% │
│ Viewport/copy_viewport             │ 745.43μs │ 199.09kw │   0.03x │      3340% │
│ Cloning/Copying/copy_to            │ 764.04μs │ 199.09kw │   0.03x │      3424% │
│ Drawing/set_multiline_text         │ 769.73μs │ 227.74kw │   0.03x │      3449% │
│ Frame Lifecycle/batch              │ 826.30μs │ 259.30kw │   0.03x │      3703% │
│ Diffing/Rendering/render_to_string │ 897.36μs │ 335.10kw │   0.02x │      4021% │
│ Diffing/Rendering/render           │   1.10ms │ 525.10kw │   0.02x │      4942% │
│ Creation/Resizing/resize           │   1.12ms │ 216.14kw │   0.02x │      5004% │
│ Diffing/Rendering/patches_to_sgr   │   1.12ms │ 533.05kw │   0.02x │      5014% │
│ Drawing/set_grapheme               │   4.77ms │ 942.23kw │   0.00x │     21388% │
│ Diffing/Rendering/diff_cells       │   4.94ms │   1.07Mw │   0.00x │     22131% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
