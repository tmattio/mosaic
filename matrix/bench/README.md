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
│ clearing/clear              │   5.26μs │  4.27kw │   1.00x │       100% │
│ setting/set_text_long       │   5.26μs │  4.27kw │   1.00x │       100% │
│ clearing/clear_line         │   5.31μs │  4.27kw │   0.99x │       101% │
│ setting/set_text_short      │   5.35μs │  4.27kw │   0.98x │       102% │
│ setting/set_grapheme_narrow │   5.44μs │  4.27kw │   0.97x │       103% │
│ composite/insert_line       │   5.53μs │  4.27kw │   0.95x │       105% │
│ setting/set_grapheme_wide   │   5.59μs │  4.27kw │   0.94x │       106% │
│ composite/scroll            │   5.60μs │  4.27kw │   0.94x │       106% │
│ resizing/resize_larger      │   5.68μs │  4.27kw │   0.93x │       108% │
│ setting/set_cell            │   5.77μs │  4.27kw │   0.91x │       110% │
│ clearing/clear_rect         │   5.87μs │  4.27kw │   0.90x │       112% │
│ composite/typing            │   5.87μs │  4.27kw │   0.90x │       112% │
│ creation/create_small       │   8.36μs │  4.25kw │   0.63x │       159% │
│ resizing/resize_smaller     │  14.96μs │ 16.10kw │   0.35x │       284% │
│ blitting/blit_small         │  15.52μs │  9.15kw │   0.34x │       295% │
│ diffing/diff_no_change      │  46.39μs │ 12.34kw │   0.11x │       882% │
│ diffing/diff_single_change  │  47.65μs │ 12.38kw │   0.11x │       906% │
│ blitting/blit_large         │  48.29μs │ 16.58kw │   0.11x │       918% │
│ diffing/diff_row_change     │  53.56μs │ 14.44kw │   0.10x │      1018% │
│ diffing/diff_full_change    │ 206.85μs │ 62.27kw │   0.03x │      3932% │
│ creation/create_large       │   3.07ms │ 17.12kw │   0.00x │     58413% │
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
│ Creation/Resizing/create           │  34.71μs │   8.52kw │   1.00x │       100% │
│ Drawing/clear                      │ 402.49μs │  98.08kw │   0.09x │      1160% │
│ Viewport/render_viewport           │ 462.07μs │ 137.17kw │   0.08x │      1331% │
│ Cloning/Copying/clone              │ 617.37μs │ 176.90kw │   0.06x │      1779% │
│ Drawing/set_text                   │ 671.14μs │ 185.13kw │   0.05x │      1934% │
│ Viewport/with_viewport             │ 811.62μs │ 227.74kw │   0.04x │      2338% │
│ Drawing/set_multiline_text         │ 821.56μs │ 227.74kw │   0.04x │      2367% │
│ Viewport/copy_viewport             │ 852.79μs │ 199.09kw │   0.04x │      2457% │
│ Cloning/Copying/copy_to            │ 860.56μs │ 199.09kw │   0.04x │      2479% │
│ Frame Lifecycle/begin_present      │ 925.72μs │ 216.69kw │   0.04x │      2667% │
│ Frame Lifecycle/batch              │   1.00ms │ 259.30kw │   0.03x │      2883% │
│ Diffing/Rendering/render_to_string │   1.08ms │ 335.10kw │   0.03x │      3122% │
│ Creation/Resizing/resize           │   1.24ms │ 216.14kw │   0.03x │      3576% │
│ Diffing/Rendering/render           │   1.24ms │ 525.10kw │   0.03x │      3581% │
│ Diffing/Rendering/patches_to_sgr   │   1.30ms │ 533.05kw │   0.03x │      3740% │
│ Drawing/set_grapheme               │   6.19ms │ 942.23kw │   0.01x │     17842% │
│ Diffing/Rendering/diff_cells       │   6.33ms │   1.07Mw │   0.01x │     18228% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
