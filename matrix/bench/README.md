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
┌─────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                        │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├─────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ clearing/clear_line         │  19.32μs │   4.17kw │   1.00x │       100% │
│ clearing/clear_rect         │  19.42μs │   4.17kw │   1.00x │       100% │
│ clearing/clear              │  19.47μs │   4.17kw │   0.99x │       101% │
│ setting/set_text_short      │  19.54μs │   4.17kw │   0.99x │       101% │
│ setting/set_grapheme_wide   │  19.63μs │   4.17kw │   0.98x │       102% │
│ composite/insert_line       │  19.80μs │   4.17kw │   0.98x │       102% │
│ setting/set_text_long       │  19.84μs │   4.17kw │   0.97x │       103% │
│ resizing/resize_larger      │  19.92μs │   4.17kw │   0.97x │       103% │
│ setting/set_cell            │  20.12μs │   4.17kw │   0.96x │       104% │
│ setting/set_grapheme_narrow │  20.18μs │   4.17kw │   0.96x │       104% │
│ composite/typing            │  20.77μs │   4.17kw │   0.93x │       108% │
│ composite/scroll            │  20.81μs │   4.17kw │   0.93x │       108% │
│ creation/create_small       │  34.95μs │   4.15kw │   0.55x │       181% │
│ blitting/blit_small         │  44.14μs │   8.79kw │   0.44x │       228% │
│ diffing/diff_no_change      │  63.42μs │  12.14kw │   0.30x │       328% │
│ diffing/diff_single_change  │  63.88μs │  12.18kw │   0.30x │       331% │
│ blitting/blit_large         │  64.49μs │  10.62kw │   0.30x │       334% │
│ resizing/resize_smaller     │  73.44μs │  15.86kw │   0.26x │       380% │
│ diffing/diff_row_change     │ 159.76μs │  26.71kw │   0.12x │       827% │
│ diffing/diff_full_change    │   2.32ms │ 361.59kw │   0.01x │     11998% │
│ creation/create_large       │  10.73ms │  112.00w │   0.00x │     55547% │
└─────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```

## `mosaic.screen`

```bash
dune exec matrix/bench/bench_screen.exe
```

```
┌────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                               │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ Creation/Resizing/create           │  69.98μs │   8.31kw │   1.00x │       100% │
│ Viewport/render_viewport           │   4.89ms │ 925.11kw │   0.01x │      6993% │
│ Drawing/clear                      │   4.97ms │ 929.88kw │   0.01x │      7106% │
│ Cloning/Copying/clone              │   5.09ms │ 964.64kw │   0.01x │      7279% │
│ Creation/Resizing/resize           │   5.56ms │   1.00Mw │   0.01x │      7938% │
│ Cloning/Copying/copy_to            │   9.79ms │   1.78Mw │   0.01x │     13990% │
│ Viewport/copy_viewport             │   9.88ms │   1.78Mw │   0.01x │     14113% │
│ Drawing/set_text                   │  12.46ms │   2.41Mw │   0.01x │     17802% │
│ Drawing/set_multiline_text         │  12.48ms │   2.41Mw │   0.01x │     17828% │
│ Viewport/with_viewport             │  12.50ms │   2.41Mw │   0.01x │     17857% │
│ Drawing/set_grapheme               │  12.63ms │   2.43Mw │   0.01x │     18046% │
│ Frame Lifecycle/batch              │  12.64ms │   2.45Mw │   0.01x │     18065% │
│ Frame Lifecycle/begin_present      │  12.74ms │   2.45Mw │   0.01x │     18201% │
│ Diffing/Rendering/render_to_string │  12.81ms │   2.52Mw │   0.01x │     18300% │
│ Diffing/Rendering/diff_cells       │  12.87ms │   2.57Mw │   0.01x │     18397% │
│ Diffing/Rendering/patches_to_sgr   │  13.05ms │   2.76Mw │   0.01x │     18645% │
│ Diffing/Rendering/render           │  13.13ms │   2.75Mw │   0.01x │     18766% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
