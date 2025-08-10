# Mosaic Benchmarks

Performance benchmarks for the Mosaic terminal UI framework components.

This benchmark suite measures the performance of core Mosaic library operations, including:
- **UI Components**: Text rendering, box layouts, tables, trees, and canvas drawing
- **Engine Operations**: Commands, subscriptions, focus management, and input routing
- **Interaction**: Hit testing and event handling

## `mosaic.ui`

Benchmarks for UI component rendering and layout operations.

```bash
dune exec mosaic/bench/bench_ui.exe
```

```
┌──────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                 │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├──────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ Table/with_borders   │  20.91μs │   7.43kw │   1.00x │       100% │
│ Table/wide_columns   │  24.78μs │   8.67kw │   0.84x │       119% │
│ Table/large_rows     │  34.88μs │  12.79kw │   0.60x │       167% │
│ Box/zbox_stack       │ 332.43μs │ 113.81kw │   0.06x │      1590% │
│ Text/long_truncate   │ 369.57μs │  85.11kw │   0.06x │      1767% │
│ Text/long_clip       │ 459.02μs │  84.85kw │   0.05x │      2195% │
│ Canvas/dense_lines   │ 885.68μs │ 186.31kw │   0.02x │      4235% │
│ Box/wide_hbox        │   1.53ms │ 478.34kw │   0.01x │      7316% │
│ Complex/dashboard    │   2.27ms │ 326.22kw │   0.01x │     10854% │
│ Canvas/filled_boxes  │   2.91ms │ 497.97kw │   0.01x │     13929% │
│ Box/tall_vbox        │   3.16ms │   1.13Mw │   0.01x │     15106% │
│ Text/long_wrap       │   3.25ms │ 666.92kw │   0.01x │     15563% │
│ Interaction/hit_test │   4.85ms │   1.15Mw │   0.00x │     23175% │
│ Tree/deep            │   7.32ms │   3.00Mw │   0.00x │     35025% │
│ Box/deep_nest        │   9.32ms │   1.56Mw │   0.00x │     44570% │
│ Text/multi_line      │   9.52ms │   3.91Mw │   0.00x │     45534% │
│ Tree/wide            │  15.82ms │   5.75Mw │   0.00x │     75629% │
│ Tree/nested          │  36.73ms │  13.24Mw │   0.00x │    175654% │
└──────────────────────┴──────────┴──────────┴─────────┴────────────┘
```

## `mosaic.engine`

Benchmarks for the TEA (The Elm Architecture) engine operations.

```bash
dune exec mosaic/bench/bench_engine.exe
```

```
┌──────────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                                     │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├──────────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ Subscriptions/keyboard_simple            │  13.32ns │    7.00w │   1.00x │       100% │
│ Commands/simple_msg                      │  43.19ns │   28.00w │   0.31x │       324% │
│ Commands/tick                            │  59.61ns │   52.00w │   0.22x │       447% │
│ Commands/perform_simple                  │  60.14ns │   50.00w │   0.22x │       451% │
│ Commands/map_chain_10                    │ 114.09ns │  148.00w │   0.12x │       856% │
│ Subscriptions/timer_accumulate_100_ticks │ 399.60ns │   11.00w │   0.03x │      2999% │
│ Commands/batch_100_msgs                  │ 689.33ns │  932.00w │   0.02x │      5173% │
│ Subscriptions/batch_100_keyboard         │ 735.86ns │   1.01kw │   0.02x │      5523% │
│ Commands/seq_100_msgs                    │ 769.42ns │  932.00w │   0.02x │      5775% │
│ Focus Manager/register_1000              │ 159.44μs │  64.31kw │   0.00x │   1196598% │
│ Input Router/on_keyboard_1000_focused    │ 234.17μs │  28.19kw │   0.00x │   1757425% │
│ Focus Manager/unregister_1000            │ 258.78μs │ 125.61kw │   0.00x │   1942161% │
│ Input Router/subscribe_1000_mixed        │ 531.05μs │  60.88kw │   0.00x │   3985539% │
│ Focus Manager/focus_next_loop_1000       │ 606.93μs │ 449.05kw │   0.00x │   4555062% │
│ Input Router/on_mouse_drag_1000          │ 633.19μs │  77.17kw │   0.00x │   4752095% │
│ Input Router/on_mouse_motion_1000_hits   │   7.35ms │   3.91Mw │   0.00x │  55177587% │
└──────────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
