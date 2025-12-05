# Mosaic

A high-level terminal UI framework for OCaml, built on [Matrix](../matrix/README.md).

## Why Mosaic?

- **Flexbox layout** – Powered by [Toffee](https://github.com/tmattio/toffee), a port of the Taffy layout engine. Define complex layouts with familiar CSS-like properties.
- **Renderable tree** – Composable UI components with automatic dirty tracking, z-ordering, and efficient incremental updates.
- **Built-in text handling** – Rich text with styled fragments, word/character wrapping, text selection, and Unicode support out of the box.
- **Event system** – Mouse, keyboard, and paste events with bubbling, focus management, and global handlers.
- **Performance-first** – Designed for 60+ FPS. Minimal allocations, pooled buffers, and smart layout caching.

## Getting Started

Install via opam:

```bash
opam install mosaic
```

Or build the library locally:

```bash
dune build @install
```

Run a demo to confirm everything works:

```bash
dune exec ./examples/01-counter/main.exe
```

### Hello Mosaic

Mosaic follows The Elm Architecture (TEA): define your model, messages, update logic, and view.

```ocaml
open Mosaic_tea

type msg = Increment | Decrement | Quit

let init () = (0, Cmd.none)

let update msg model =
  match msg with
  | Increment -> (model + 1, Cmd.none)
  | Decrement -> (model - 1, Cmd.none)
  | Quit -> (model, Cmd.quit)

let view model =
  box ~align_items:Center ~justify_content:Center
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_direction:Column ~align_items:Center ~gap:(gap 1)
        ~border:true ~padding:(padding 2) ~title:"Counter"
        [
          text ~content:(Printf.sprintf "Count: %d" model) ();
          text ~content:"Press + or - to change, q to quit" ();
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char '+') -> Some Increment
      | Char c when Uchar.equal c (Uchar.of_char '-') -> Some Decrement
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
```

Mosaic handles terminal setup, event dispatch, layout computation, and efficient rendering—you just define your app logic.

## Acknowledgements

Mosaic's internal architecture is heavily based on [OpenTUI](https://github.com/sst/opentui/). Mosaic predates OpenTUI, and when OpenTUI was released it aligned closely with where Mosaic was converging—but did virtually everything better. We rewrote large parts of Mosaic based on OpenTUI's design, including the text buffer abstraction, the event and selection subsystem, and rendering trees tightly coupled with the event loop.

Mosaic remains built on [Matrix](../matrix/README.md) for low-level terminal operations, and our high-level API follows The Elm Architecture (taking inspiration from [Bubble Tea](https://github.com/charmbracelet/bubbletea)), but the internal architecture mirrors OpenTUI.

Layout is powered by [Toffee](https://github.com/tmattio/toffee), a port of the [Taffy](https://github.com/DioxusLabs/taffy) Flexbox engine to OCaml.

## License

Mosaic is licensed under the ISC license. See [LICENSE](../LICENSE) for details.
