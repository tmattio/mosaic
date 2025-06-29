# Mosaic

A delightful OCaml framework for building modern terminal user interfaces. Inspired by The Elm Architecture and Bubble Tea.

## Features

- **Elm Architecture** - Simple, scalable app structure with model-view-update
- **Declarative UI** - Build layouts with flexbox-style primitives
- **Cross-platform** - Works on Unix/Linux, macOS, and Windows
- **Mouse & Keyboard** - Full input handling with modifiers
- **Unicode Support** - First-class support for Unicode throughout
- **Testable** - Pure functions and effects make testing straightforward

## Quick Start

```ocaml
open Mosaic

type model = int
type msg = Increment | Decrement | Quit

let init () = (0, Cmd.none)

let update msg model =
  match msg with
  | Increment -> (model + 1, Cmd.none)
  | Decrement -> (model - 1, Cmd.none)
  | Quit -> (model, Cmd.quit)

let view model =
  Ui.vbox [
    Ui.text "Counter Example";
    Ui.text (Printf.sprintf "Count: %d" model);
    Ui.text "";
    Ui.text "Press +/- to change, Ctrl+C to quit";
  ]

let subscriptions _ =
  Sub.batch [
    Sub.on_key ~ctrl:true (Char (Uchar.of_char 'C')) Quit;
    Sub.on_char '+' Increment;
    Sub.on_char '-' Decrement;
  ]

let () = Mosaic.run (Mosaic.app ~init ~update ~view ~subscriptions ())
```

## Installation

```bash
opam install mosaic
```

## License

ISC
