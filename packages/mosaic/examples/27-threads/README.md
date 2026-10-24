# `27-threads`

A program fed by a worker thread, and stopped by a thread it did not start.

```bash
dune exec ./packages/mosaic/examples/27-threads/main.exe
dune exec ./packages/mosaic/examples/27-threads/main.exe -- --for 5
```

## Controls

- `p` &mdash; pause or resume the feed.
- `q` or `Esc` &mdash; quit.

## Highlights

- A `Sub.worker` gives the program lines from a thread of its own, with
  its mailbox's `dispatch`, until its stop is requested when the program
  ends.
- With `--for SECONDS`, a watchdog thread requests a `Matrix.Stop` made
  before the program started, and `Mosaic.run` ends as at the end of its
  input.
- A thread the program did not start, such as a library's callback thread,
  gives it messages with the `dispatch` of a `Cmd.perform`, which it may
  keep after the perform returns: `Cmd.perform (fun dispatch ->
  Watcher.on_change (fun path -> dispatch (Changed path)))`.
