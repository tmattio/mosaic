# Toffee Benchmarks

The `toffee/bench/` directory contains ubench suites for realistic layout workloads. Run them with:

```bash
dune exec toffee/bench/bench_toffee.exe
```

Current groups:
- `flex/deep-hierarchy` — deeply nested flex stacks alternating row/column.
- `flex/wide-dashboard` — many flex rows with wrapping cards.
- `grid/auto-placement-gallery` — dense grid auto-placement with fixed rows and fr columns.
- `mixed/dashboard` — mixed flex and grid sections (header, toolbar, card grid, activity feed).

Use `--list` to see names, `--quota 5s` to cap runtime, and `--format text|json|csv` for output. Example:

```
dune exec toffee/bench/bench_toffee.exe -- --quota 5s --format text
```

## Results

```
┌────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                               │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ toffee/grid/auto-placement-gallery │ 549.14μs │ 301.62kw │   1.00x │       100% │
│ toffee/mixed/dashboard             │ 639.80μs │ 369.44kw │   0.86x │       117% │
│ toffee/flex/wide-dashboard         │   1.03ms │ 502.33kw │   0.53x │       188% │
│ toffee/flex/deep-hierarchy         │  33.33ms │  17.35Mw │   0.02x │      6069% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
