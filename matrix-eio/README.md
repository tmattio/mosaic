# Matrix_eio

Eio backend for the [Matrix](../matrix/README.md) terminal library.

Matrix_eio gives `Matrix.run` a backend whose waits yield to other Eio fibers:
while the loop waits for input, a wake, a signal or a deadline, the domain runs
the program's other fibers. Everything else is Matrix's: the run, its loop and
its frames are the same over every backend.

## Getting Started

Install via opam:

```bash
opam install matrix-eio
```

### Hello Terminal

```ocaml
open Matrix

let () =
  Eio_main.run @@ fun env ->
  let backend =
    Matrix_eio.backend ~clock:(Eio.Stdenv.mono_clock env) ~stdin:env#stdin
      ~stdout:env#stdout ()
  in
  Matrix.run ~backend @@ fun app ->
  let rec loop keys =
    match Matrix.next app with
    | Stopped | Event (Key { key = Escape; _ }) -> ()
    | Event (Key _) ->
        Matrix.request_redraw app;
        loop (keys + 1)
    | Frame ->
        Matrix.draw app (fun grid ->
            Grid.draw_text grid ~x:2 ~y:2 ~text:(Printf.sprintf "Keys: %d" keys));
        loop keys
    | Event _ | Timer _ | Posted _ -> loop keys
  in
  loop 0
```

The three parameters connect Matrix to the Eio runtime:

- `clock` — Eio's monotonic clock, which the wait sleeps on until a deadline
- `stdin` — Eio source for terminal input
- `stdout` — Eio sink for terminal output

The wait wakes on the input, on a `Matrix.post` from any thread or fiber, on
the signals the runtime takes, and at the next deadline; nothing polls, so an
idle program costs no wakeups.

## When to Use Matrix_eio

Use `Matrix_eio.backend` when:

- Your application already runs inside `Eio_main.run`
- You want terminal I/O to cooperate with other concurrent Eio fibers

If you are not using Eio, `Matrix.run` uses its Unix backend by default — no
adapter needed.

## License

Matrix_eio is licensed under the ISC license. See [LICENSE](../LICENSE) for details.
