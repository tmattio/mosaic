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

## Getting started

Each package's README shows how to install it and write a first program, and
its examples are listed with their run commands:
[Mosaic](packages/mosaic/README.md) ([examples](packages/mosaic/examples/README.md)),
[Matrix](packages/matrix/README.md) ([examples](packages/matrix/examples/README.md)),
[Toffee](packages/toffee/README.md) ([examples](packages/toffee/examples/README.md)).

## License

ISC. See [LICENSE](LICENSE).
