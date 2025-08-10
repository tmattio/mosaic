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
│ setting/set_text_long       │   5.46μs │  4.27kw │   1.00x │       100% │
│ setting/set_grapheme_narrow │   5.48μs │  4.27kw │   1.00x │       100% │
│ setting/set_text_short      │   5.61μs │  4.27kw │   0.97x │       103% │
│ clearing/clear_rect         │   5.61μs │  4.27kw │   0.97x │       103% │
│ clearing/clear_line         │   5.72μs │  4.27kw │   0.95x │       105% │
│ composite/insert_line       │   5.73μs │  4.27kw │   0.95x │       105% │
│ setting/set_grapheme_wide   │   5.80μs │  4.27kw │   0.94x │       106% │
│ resizing/resize_larger      │   5.89μs │  4.27kw │   0.93x │       108% │
│ composite/scroll            │   5.96μs │  4.27kw │   0.92x │       109% │
│ clearing/clear              │   6.05μs │  4.27kw │   0.90x │       111% │
│ setting/set_cell            │   6.20μs │  4.27kw │   0.88x │       114% │
│ composite/typing            │   6.38μs │  4.27kw │   0.85x │       117% │
│ resizing/resize_smaller     │  13.47μs │ 16.10kw │   0.41x │       247% │
│ creation/create_small       │  14.28μs │  4.25kw │   0.38x │       262% │
│ blitting/blit_small         │  17.46μs │  9.15kw │   0.31x │       320% │
│ diffing/diff_single_change  │  50.15μs │ 12.38kw │   0.11x │       919% │
│ diffing/diff_no_change      │  51.31μs │ 12.34kw │   0.11x │       940% │
│ blitting/blit_large         │  54.92μs │ 16.58kw │   0.10x │      1006% │
│ diffing/diff_row_change     │  74.60μs │ 14.44kw │   0.07x │      1367% │
│ diffing/diff_full_change    │ 661.39μs │ 62.27kw │   0.01x │     12120% │
│ creation/create_large       │   3.33ms │ 17.12kw │   0.00x │     60973% │
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
│ Creation/Resizing/create           │  20.39μs │   8.52kw │   1.00x │       100% │
│ Drawing/clear                      │ 374.62μs │  98.08kw │   0.05x │      1837% │
│ Viewport/render_viewport           │ 423.67μs │ 137.17kw │   0.05x │      2077% │
│ Cloning/Copying/clone              │ 568.93μs │ 176.90kw │   0.04x │      2790% │
│ Drawing/set_text                   │ 600.44μs │ 185.13kw │   0.03x │      2944% │
│ Drawing/set_multiline_text         │ 695.92μs │ 227.74kw │   0.03x │      3412% │
│ Viewport/with_viewport             │ 700.16μs │ 227.74kw │   0.03x │      3433% │
│ Frame Lifecycle/begin_present      │ 769.86μs │ 216.69kw │   0.03x │      3775% │
│ Viewport/copy_viewport             │ 789.85μs │ 199.09kw │   0.03x │      3873% │
│ Cloning/Copying/copy_to            │ 790.20μs │ 199.09kw │   0.03x │      3875% │
│ Frame Lifecycle/batch              │ 867.38μs │ 259.30kw │   0.02x │      4253% │
│ Diffing/Rendering/render_to_string │ 919.59μs │ 335.10kw │   0.02x │      4509% │
│ Diffing/Rendering/patches_to_sgr   │   1.11ms │ 533.05kw │   0.02x │      5428% │
│ Diffing/Rendering/render           │   1.11ms │ 525.10kw │   0.02x │      5444% │
│ Creation/Resizing/resize           │   1.14ms │ 216.14kw │   0.02x │      5572% │
│ Drawing/set_grapheme               │   4.96ms │ 942.23kw │   0.00x │     24320% │
│ Diffing/Rendering/diff_cells       │   5.18ms │   1.07Mw │   0.00x │     25399% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
