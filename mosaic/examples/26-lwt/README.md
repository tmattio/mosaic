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

- The program runs on a domain of its own, as `Mosaic.run` runs it, so it
  never holds up Lwt's thread; the work of `Cmd.perform` runs on Lwt's
  thread through `Lwt_preemptive.run_in_main_dont_wait`.
- A `Matrix.Stop` request is made before the program starts: cancelling the
  promise requests the stop and resolves at once, even before the program
  has begun.
- The program gives the terminal back on its own domain; the domain closes a
  pipe then, and code that must wait for the terminal awaits the pipe with
  `Lwt_unix.wait_read`.
