# Mosaic

Mosaic is a high-level terminal UI framework for OCaml. It provides The Elm Architecture (TEA) for declarative UI development, with flexbox layout and efficient rendering.

The repository also includes:

| Package                    | Description                                                |
| -------------------------- | ---------------------------------------------------------- |
| [Matrix](matrix/README.md) | Low-level terminal toolkit: rendering, input, PTY, VTE     |
| [Toffee](toffee/README.md) | CSS layout engine (Flexbox, Grid, Block) ported from Taffy |

**Matrix** provides the foundation Mosaic builds on: efficient ANSI rendering, keyboard/mouse input parsing, pseudo-terminal management, and a virtual terminal emulator.

**Toffee** powers Mosaic's flexbox layout, but can also be used standalone for CSS-style layout in any context.

## Getting Started

Install via opam:

```bash
opam install mmosaic
```

Or build locally:

```bash
dune build @install
```

Run a demo:

```bash
# Matrix: rain animation
dune exec ./matrix/examples/01-rain/main.exe

# Mosaic: counter app
dune exec ./mosaic/examples/01-counter/main.exe
```

## License

Mosaic is licensed under the ISC license. See [LICENSE](LICENSE) for details.
