# `26-lwt`

Timers that are Lwt promises, in a program that is a promise itself.

```bash
dune exec ./mosaic/examples/26-lwt/main.exe
dune exec ./mosaic/examples/26-lwt/main.exe -- --for 5
```

It is built only when `lwt` is installed. With `--for SECONDS`, `Lwt.pick`
cancels the program after that long.

## Controls

- `a` &mdash; add a timer.
- `q` or `Esc` &mdash; quit.

## Highlights

- `Mosaic_lwt.run app` is the whole glue: the program runs on a domain of its
  own, so it never holds up Lwt's thread, and the work of `Cmd.perform` runs
  on Lwt's thread, where it starts its promises.
- Cancelling the promise requests the program's stop, even before the
  program has begun. The promise is rejected at once, and the program gives
  the terminal back on its own domain; code that must wait for the terminal
  requests the stop given to `Mosaic_lwt.run ~stop` and waits for the
  promise instead.
