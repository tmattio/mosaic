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
│ setting/set_text_long       │  19.71μs │  4.27kw │   1.00x │       100% │
│ resizing/resize_larger      │  19.76μs │  4.27kw │   1.00x │       100% │
│ clearing/clear              │  19.77μs │  4.27kw │   1.00x │       100% │
│ setting/set_grapheme_narrow │  19.85μs │  4.27kw │   0.99x │       101% │
│ clearing/clear_line         │  20.05μs │  4.27kw │   0.98x │       102% │
│ setting/set_grapheme_wide   │  20.12μs │  4.27kw │   0.98x │       102% │
│ composite/typing            │  20.20μs │  4.27kw │   0.98x │       102% │
│ setting/set_text_short      │  20.21μs │  4.27kw │   0.98x │       103% │
│ clearing/clear_rect         │  20.64μs │  4.27kw │   0.95x │       105% │
│ composite/scroll            │  20.66μs │  4.27kw │   0.95x │       105% │
│ composite/insert_line       │  22.46μs │  4.27kw │   0.88x │       114% │
│ setting/set_cell            │  23.35μs │  4.27kw │   0.84x │       118% │
│ creation/create_small       │  41.45μs │  4.25kw │   0.48x │       210% │
│ blitting/blit_small         │  44.85μs │  9.15kw │   0.44x │       228% │
│ diffing/diff_no_change      │  64.64μs │ 12.34kw │   0.30x │       328% │
│ diffing/diff_single_change  │  66.57μs │ 12.38kw │   0.30x │       338% │
│ diffing/diff_row_change     │  68.01μs │ 14.42kw │   0.29x │       345% │
│ blitting/blit_large         │  76.26μs │ 16.58kw │   0.26x │       387% │
│ resizing/resize_smaller     │  80.82μs │ 16.10kw │   0.24x │       410% │
│ diffing/diff_full_change    │ 194.49μs │ 62.26kw │   0.10x │       987% │
│ creation/create_large       │  10.99ms │ 17.12kw │   0.00x │     55741% │
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
│ Creation/Resizing/create           │  83.04μs │   8.52kw │   1.00x │       100% │
│ Drawing/clear                      │   2.68ms │ 493.21kw │   0.03x │      3223% │
│ Viewport/render_viewport           │   2.75ms │ 532.30kw │   0.03x │      3317% │
│ Cloning/Copying/clone              │   2.86ms │ 572.03kw │   0.03x │      3447% │
│ Creation/Resizing/resize           │   3.43ms │ 611.27kw │   0.02x │      4128% │
│ Viewport/copy_viewport             │   5.39ms │ 989.35kw │   0.02x │      6485% │
│ Cloning/Copying/copy_to            │   5.39ms │ 989.34kw │   0.02x │      6496% │
│ Drawing/set_multiline_text         │   6.69ms │   1.29Mw │   0.01x │      8060% │
│ Viewport/with_viewport             │   6.70ms │   1.29Mw │   0.01x │      8072% │
│ Drawing/set_text                   │   6.71ms │   1.29Mw │   0.01x │      8077% │
│ Frame Lifecycle/batch              │   6.85ms │   1.32Mw │   0.01x │      8252% │
│ Frame Lifecycle/begin_present      │   6.87ms │   1.32Mw │   0.01x │      8270% │
│ Drawing/set_grapheme               │   6.88ms │   1.34Mw │   0.01x │      8287% │
│ Diffing/Rendering/render_to_string │   6.90ms │   1.39Mw │   0.01x │      8312% │
│ Diffing/Rendering/diff_cells       │   7.14ms │   1.47Mw │   0.01x │      8595% │
│ Diffing/Rendering/render           │   7.17ms │   1.63Mw │   0.01x │      8634% │
│ Diffing/Rendering/patches_to_sgr   │   7.27ms │   1.63Mw │   0.01x │      8749% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
