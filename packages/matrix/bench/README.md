# Matrix Benchmarks

The `matrix/bench` directory contains thumper microbenchmarks that stress
specific subsystems. Run the complete performance gate with:

```bash
dune build @matrix/bench
```

The gate runs the six suites sequentially so their samples never compete. Each
suite checks against `<suite>.thumper` (for example `ansi.thumper` or
`grid.thumper`) and exposes improvements or a new machine section through
`dune promote`. Functional `runtest` does not run performance measurements.

For one suite, use `dune exec matrix/bench/<bench>.exe`. Pass `--bless` to
replace the current machine baseline, `--explore` to print results without
baseline interaction, `-l` / `--list` to list cases, `-f PATTERN` to filter,
or `--csv FILE` to write CSV.
