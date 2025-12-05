# Matrix Benchmarks

The `matrix/bench` directory contains microbenchmarks that stress specific subsystems. Run them with:

```bash
dune exec matrix/bench/<bench>.exe
```

## Results - Ansi

```
┌────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                               │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ styled/inline_styled               │ 497.30ns │  151.00w │   1.00x │       100% │
│ styled/render_log_segments         │   1.81μs │  416.00w │   0.27x │       364% │
│ strip_parse/strip_plain_block      │  12.39μs │    0.00w │   0.04x │      2491% │
│ tui_output/segment_emit_dense_line │  14.18μs │   2.66kw │   0.04x │      2851% │
│ strip_parse/parse_tui_frame        │  54.79μs │  17.29kw │   0.01x │     11017% │
│ strip_parse/strip_ansi_block       │ 103.34μs │    9.00w │   0.00x │     20781% │
│ control/cursor_script_80x24        │ 258.84μs │  33.71kw │   0.00x │     52049% │
│ strip_parse/parse_ansi_block       │ 455.88μs │ 136.91kw │   0.00x │     91671% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```

## Results - Glyph

```
┌──────────────────────────────┬──────────┬─────────┬─────────┬────────────┐
│ Name                         │ Time/Run │ mWd/Run │ Speedup │ vs Fastest │
├──────────────────────────────┼──────────┼─────────┼─────────┼────────────┤
│ width/width/ascii/unicode    │   3.13μs │  14.00w │   1.00x │       100% │
│ width/width/complex/wcwidth  │   6.95μs │  14.00w │   0.45x │       222% │
│ segment/segment/complex_line │   7.37μs │  2.79kw │   0.42x │       236% │
│ encode/encode/ascii_line     │   7.89μs │  3.58kw │   0.40x │       252% │
│ segment/segment/ascii_line   │  10.55μs │   7.00w │   0.30x │       337% │
│ width/width/complex/unicode  │  14.38μs │  7.12kw │   0.22x │       460% │
│ width/width/complex/no_zwj   │  14.75μs │  8.27kw │   0.21x │       472% │
│ pool/pool/get_existing       │  26.95μs │ 434.00w │   0.12x │       861% │
│ encode/encode/complex_line   │  28.70μs │ 11.94kw │   0.11x │       917% │
│ pool/pool/intern_hotset      │  32.70μs │ 392.00w │   0.10x │      1045% │
│ pool/pool/intern_unique_256  │  62.67μs │ 12.80kw │   0.05x │      2003% │
└──────────────────────────────┴──────────┴─────────┴─────────┴────────────┘
```

## Results - Input

```
┌────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                       │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ input/mouse/mixed          │  28.45μs │  34.32kw │   1.00x │       100% │
│ input/typing/ascii-burst   │  33.43μs │  34.29kw │   0.85x │       118% │
│ input/typing/unicode-burst │  55.36μs │  44.37kw │   0.51x │       195% │
│ input/paste/ci-log         │ 162.68μs │  522.00w │   0.17x │       572% │
│ input/keyboard/legacy-hot  │ 248.82μs │ 232.18kw │   0.11x │       875% │
│ input/keyboard/kitty-hot   │ 371.78μs │ 353.78kw │   0.08x │      1307% │
└────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```

## Results - Grid

```
┌─────────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                                    │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├─────────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ grid/grid.partial_update/sparse-cells   │ 626.15ns │  246.00w │   1.00x │       100% │
│ grid/grid.partial_update/status-line    │   4.41μs │  483.00w │   0.14x │       704% │
│ grid/grid.fill_rect/opaque-full         │ 357.84μs │   1.36kw │   0.00x │     57149% │
│ grid/grid.draw_text/ascii-full-screen   │ 787.72μs │  49.31kw │   0.00x │    125804% │
│ grid/grid.draw_text/emoji-full-screen   │ 815.87μs │  99.06kw │   0.00x │    130300% │
│ grid/grid.fill_rect/translucent-overlay │ 922.37μs │ 130.94kw │   0.00x │    147309% │
│ grid/grid.scroll/terminal-region        │   1.64ms │ 136.52kw │   0.00x │    261802% │
└─────────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```

## Results - Screen

```
┌────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                               │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ render/render.diff/single-line     │   4.97ms │ 765.38kw │   1.00x │       100% │
│ render/render.diff/sparse-cells    │   5.00ms │ 767.89kw │   1.00x │       100% │
│ render/render.scenario/text-editor │   5.05ms │ 768.32kw │   0.98x │       102% │
│ render/render.diff/no-changes      │   5.06ms │ 767.36kw │   0.98x │       102% │
│ render/render.diff/full-screen     │   5.14ms │ 772.63kw │   0.97x │       103% │
│ render/render.emoji/full-screen    │   5.29ms │ 875.28kw │   0.94x │       106% │
│ render/render.scenario/ci-log-view │   5.31ms │ 960.14kw │   0.94x │       107% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
