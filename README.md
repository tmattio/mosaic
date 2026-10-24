# Mosaic

Terminal user interfaces for OCaml.

This repository contains three packages that work together or
independently:

| Package                             | Description                                               |
| ----------------------------------- | --------------------------------------------------------- |
| [Mosaic](packages/mosaic/README.md) | High-level UI framework with The Elm Architecture         |
| [Matrix](packages/matrix/README.md) | Terminal toolkit: rendering, input, PTY, VTE               |
| [Toffee](packages/toffee/README.md) | CSS layout engine (Flexbox, Grid, Block) ported from Taffy |

**Mosaic** provides a TEA runtime (model / update / view), built-in
widgets, and CSS layout via Toffee. Editors, scrolling, selection, and focus
are model values; semantic callbacks return messages that `update` accepts,
transforms, or rejects. Selection follows application values across reordering.
`~on_focus_request` lets a pointer press propose focus before caret placement;
raw event callbacks remain optional-message filters.

**Matrix** is the terminal layer: double-buffered rendering that diffs
cell changes to emit minimal ANSI output, Kitty keyboard, SGR mouse,
bracketed paste, focus reporting, and a virtual terminal emulator.
Usable on its own for immediate-mode terminal apps.

**Toffee** is a pure OCaml port of [Taffy](https://github.com/DioxusLabs/taffy).
It computes Flexbox, Grid, and Block layout with no C stubs and no
runtime dependencies.

Syntax highlighting is backend-neutral. See the [tree-sitter recipe](doc/highlighting.md)
and the independent [Tree-sitter bindings](https://github.com/invariant-hq/ocaml-tree-sitter).

## Quick start

Install via opam:

```bash
opam install mosaic
```

Or build from source:

```bash
dune build @install
```

Run a demo:

```bash
dune exec ./packages/mosaic/examples/01-counter/main.exe
```

A minimal Mosaic app:

```ocaml
open Mosaic

type msg = Increment | Decrement | Quit

let init () = (0, Cmd.none)

let update msg model =
  match msg with
  | Increment -> (model + 1, Cmd.none)
  | Decrement -> (model - 1, Cmd.none)
  | Quit -> (model, Cmd.quit)

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [ text (Printf.sprintf "Count: %d" model) ];
      text "Press + / - to change, q to quit";
    ]

let subscriptions _model =
  Sub.on_keys
    [
      (Shortcut.char '+', Increment);
      (Shortcut.char '-', Decrement);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

let () = run { init; update; view; subscriptions }
```

## Examples

Mosaic ships with 27 examples and 4 showcase apps covering forms, tables,
markdown, syntax highlighting, charts, async commands, tree widgets, CSS
Grid layouts, and more. See [packages/mosaic/examples/](packages/mosaic/examples/README.md).

Matrix has 15 examples and 1 showcase demonstrating rain animation, game
of life, mandelbrot rendering, snake, a synthesizer, and a terminal
emulator. See [packages/matrix/examples/](packages/matrix/examples/README.md).

Run any example from the repo root:

```bash
# Mosaic: form with focus management
dune exec ./packages/mosaic/examples/12-form/main.exe

# Mosaic: ML dashboard showcase
dune exec ./packages/mosaic/examples/x-dashboard/main.exe

# Matrix: rain animation
dune exec ./packages/matrix/examples/01-rain/main.exe
```

## License

ISC. See [LICENSE](LICENSE).
