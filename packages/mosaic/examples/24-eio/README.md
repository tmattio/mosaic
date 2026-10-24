# `24-eio`

Async task runner using Eio fibers: the program runs from the main fiber with
`Mosaic_eio.run`, while the main domain runs its jobs as fibers.

```bash
dune exec ./packages/mosaic/examples/24-eio/main.exe
dune exec ./packages/mosaic/examples/24-eio/main.exe -- --for 5
```

It is built only when `eio_main` is installed. With `--for SECONDS`,
`Fiber.first` cancels the program after that long.

## Controls

- `a` &mdash; launch a new async job.
- `q` or `Esc` &mdash; quit.

## Features

- Simulated background jobs running as Eio fibers, which sleep on the Eio
  clock.
- Live progress percentage updated via tick subscriptions.
- Multiple concurrent jobs without blocking the UI loop.

## Highlights

- `Mosaic_eio.run (Eio.Stdenv.domain_mgr env) app` is the whole glue: the
  program takes its steps on a domain of its own and waits there with Eio,
  and the work of `Cmd.perform` runs as fibers of the main domain, where it
  may call Eio.
- Cancelling the fiber that runs the program ends it, and the terminal is
  given back before `Mosaic_eio.run` raises, so the line the example writes
  after lands on the shell's screen.
