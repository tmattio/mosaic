# `24-eio`

Async task runner using Eio fibers: the program runs on a domain of its own
and waits for its terminal with Eio, while the main domain runs its jobs as
fibers.

```bash
dune exec ./mosaic/examples/24-eio/main.exe
```

It is built only when `eio_main` is installed.

## Controls

- `a` &mdash; launch a new async job.
- `q` or `Esc` &mdash; quit.

## Features

- Simulated background jobs running as Eio daemon fibers.
- Live progress percentage updated via tick subscriptions.
- Multiple concurrent jobs without blocking the UI loop.

## Highlights

- `Mosaic.start` leaves the waiting to the caller: the loop takes the
  program's steps with `Mosaic.step`, and between them waits with Eio until
  one of `Mosaic.fds` is readable or `Mosaic.deadline` comes.
- `Eio.Domain_manager.run` gives the program its domain, so the main
  domain's other fibers go on while a step runs, and cancelling the caller
  cancels the wait.
- `process_perform` hands the work of `Cmd.perform` to fibers of the main
  domain through an `Eio.Stream`.
