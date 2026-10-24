# Toffee Benchmarks

The `toffee/bench/` directory contains thumper suites for realistic layout
workloads. Run the performance gate with:

```bash
dune build @toffee/bench
```

Current groups:
- `flex/deep-hierarchy` — deeply nested flex stacks alternating row/column.
- `flex/wide-dashboard` — many flex rows with wrapping cards.
- `grid/auto-placement-gallery` — dense grid auto-placement with fixed rows and fr columns.
- `mixed/dashboard` — mixed flex and grid sections (header, toolbar, card grid, activity feed).

The suite checks against `toffee.thumper` independently of functional
`runtest`, and exposes improvements or a new machine section through
`dune promote`. Use `--bless` to replace the current machine baseline,
`--explore` to print results without baseline interaction, `-l` / `--list` to
list cases, `-f PATTERN` to filter, or `--csv FILE` to write CSV. Example:

```
dune exec toffee/bench/bench_toffee.exe -- --explore
```
