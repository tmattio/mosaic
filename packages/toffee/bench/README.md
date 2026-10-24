# Toffee Benchmarks

The `packages/toffee/bench/` directory contains thumper suites for realistic layout
workloads. Run the performance gate with:

```bash
dune build --profile release @packages/toffee/bench
```

Current groups:
- `flex/deep-hierarchy` — deeply nested flex stacks alternating row/column.
- `flex/wide-dashboard` — many flex rows with wrapping cards.
- `grid/auto-placement-gallery` — dense grid auto-placement with fixed rows and fr columns.
- `mixed/dashboard` — mixed flex and grid sections (header, toolbar, card grid, activity feed).

The suite checks against `toffee.thumper` independently of functional
`runtest`, one section per machine, and exposes improvements or a new machine
section through `dune promote`. The alias refuses to measure outside release,
and machines key their section with `THUMPER_MACHINE` (see
`packages/matrix/bench/README.md`). Pass `bless` to re-record the current
machine's section, `list` to list cases, `-f PATTERN` to filter, or `--help`
for the rest. Example:

```
dune exec --profile release packages/toffee/bench/bench_toffee.exe -- list
```
