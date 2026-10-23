# `27-threads`

A program fed by a thread it did not start, and stopped by another.

```bash
dune exec ./mosaic/examples/27-threads/main.exe
dune exec ./mosaic/examples/27-threads/main.exe -- --for 5
```

## Controls

- `p` &mdash; pause or resume the feed.
- `q` or `Esc` &mdash; quit.

## Highlights

- `Mosaic.start` gives the program's handle to the code that runs it, and a
  thread gives the program messages with `Mosaic.dispatch`.
- The loop waits with `Unix.select` on `Mosaic.fds` until
  `Mosaic.deadline`, as `Mosaic.run` does.
- With `--for SECONDS`, a watchdog thread requests a `Matrix.Stop` made
  before the program started.
