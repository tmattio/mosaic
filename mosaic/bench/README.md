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
│ Subscriptions/keyboard_simple            │  19.81ns │    7.00w │   1.00x │       100% │
│ Commands/perform_simple                  │  43.44ns │   50.00w │   0.46x │       219% │
│ Commands/tick                            │  72.32ns │   52.00w │   0.27x │       365% │
│ Commands/map_chain_10                    │  79.21ns │  148.00w │   0.25x │       400% │
│ Commands/simple_msg                      │  83.97ns │   28.00w │   0.24x │       424% │
│ Subscriptions/timer_accumulate_100_ticks │ 402.78ns │   11.00w │   0.05x │      2033% │
│ Commands/seq_100_msgs                    │ 648.12ns │  932.00w │   0.03x │      3271% │
│ Commands/batch_100_msgs                  │ 659.55ns │  932.00w │   0.03x │      3329% │
│ Subscriptions/batch_100_keyboard         │ 897.92ns │   1.01kw │   0.02x │      4532% │
│ Focus Manager/register_1000              │  26.10μs │   3.55kw │   0.00x │    131724% │
│ Focus Manager/focus_next_loop_1000       │  37.64μs │   8.90kw │   0.00x │    189975% │
│ Focus Manager/unregister_1000            │  53.27μs │   4.35kw │   0.00x │    268902% │
│ Input Router/on_keyboard_1000_focused    │ 271.28μs │  28.19kw │   0.00x │   1369300% │
│ Input Router/subscribe_1000_mixed        │ 497.15μs │  60.88kw │   0.00x │   2509391% │
│ Input Router/on_mouse_motion_1000_hits   │ 545.57μs │ 103.80kw │   0.00x │   2753776% │
│ Input Router/on_mouse_drag_1000          │ 577.04μs │ 102.98kw │   0.00x │   2912637% │
└──────────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
