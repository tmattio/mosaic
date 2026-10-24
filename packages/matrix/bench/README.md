# Matrix Benchmarks

The `packages/matrix/bench` directory contains thumper microbenchmarks that stress
specific subsystems. Run the complete performance gate with:

```bash
dune build --profile release @packages/matrix/bench
```

Allocation counts follow the profile (dev compiles with `-opaque`), so the
alias refuses to measure outside release. The gate runs the seven suites
sequentially so their samples never compete. Each suite checks against
`<suite>.thumper` (for example `ansi.thumper` or `grid.thumper`), which holds
one section per machine, and exposes improvements or a new machine section
through `dune promote`. Functional `runtest` does not run performance
measurements.

A section is keyed by the machine's hostname, which can change with the
network. Set a stable key in your shell profile, the one your section in the
baseline files already uses (`fudge`, `m1max`):

```bash
export THUMPER_MACHINE=fudge
```

For one suite, use `dune exec --profile release packages/matrix/bench/<bench>.exe`.
Pass `bless` to re-record the current machine's section, `list` to list cases,
`-f PATTERN` to filter, `--quick` for a shorter protocol while iterating, or
`--help` for the rest.
