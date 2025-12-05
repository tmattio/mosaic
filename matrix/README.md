# Matrix

A fast, full-featured, modern terminal UI library for OCaml.

## Why Matrix?

- **Minimal dependencies** – Only depends on `uutf` for UTF-8 decoding. No transitive dependency bloat.
- **High performance** – Designed for 60+ FPS rendering. Zero-allocation frame diffing, efficient glyph pooling, and double-buffered rendering minimize GC pressure.
- **Modular architecture** – Small, focused libraries (`matrix.ansi`, `matrix.grid`, `matrix.pty`, `matrix.vte`, etc.) that you can use independently or combine as needed.
- **Immediate-mode API** – A simple render loop with `on_render`, `on_input`, and `on_resize` callbacks. No framework overhead—just draw your UI each frame.
- **Native alpha blending** – RGBA colors with proper alpha compositing for translucent overlays and smooth visual effects.
- **Full Unicode support** – Grapheme clusters, emoji, wide characters, and 24-bit color with correct alignment across terminal width calculation methods.
- **Modern terminal protocols** – Kitty keyboard (with auto-detection), SGR/X10/URXVT mouse tracking, bracketed paste, and focus reporting—all negotiated automatically.
- **Safe terminal handling** – Raw mode, alternate screen, and input protocols are restored on exit, even if your code raises an exception.
- **Declarative and imperative APIs** – Use the mutable `Grid` for performance-critical paths, or the Notty-inspired `Image` DSL for compositional layouts.
- **Built-in devtools** – Debug overlay for frame timing and FPS, and frame dumps to disk for diagnostics.

## Getting Started

Install via opam:

```bash
opam install matrix
```

Or build the library locally:

```bash
dune build @install
```

Run a demo to confirm everything works:

```bash
dune exec ./examples/01-rain/main.exe
```

### Hello Terminal

```ocaml
open Matrix

let () =
  let app = Matrix.create () in
  let frames = ref 0 in
  Matrix.run app
    ~on_frame:(fun _ ~dt:_ -> incr frames)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      Grid.clear grid;
      Grid.draw_text grid ~x:2 ~y:2
        ~text:(Printf.sprintf "Frames: %d" !frames))
```

Matrix switches the TTY to raw mode, negotiates terminal features, and restores everything on exit—even if a callback raises.

### Display Modes

Matrix supports three rendering modes:

- **`Alt`** (default) – Full-screen alternate buffer, content restored on exit
- **`Primary_inline`** – Renders below the shell prompt, grows dynamically
- **`Primary_split`** – Fixed region at bottom of primary screen

Use `Primary_inline` for CLI tools that should leave output in terminal history:

```ocaml
let app = Matrix.create ~mode:`Primary_inline () in
(* Use Matrix.static_print to write persistent output above the UI *)
```

## API Overview

Matrix is organized into focused libraries that can be used together or independently:

| Library           | Module     | Purpose                                               |
| ----------------- | ---------- | ----------------------------------------------------- |
| `matrix`          | `Matrix`   | Immediate-mode runtime with render loop and callbacks |
| `matrix`          | `Image`    | Declarative Notty-inspired composition DSL            |
| `matrix.grid`     | `Grid`     | Mutable framebuffer with colors and styles            |
| `matrix.screen`   | `Screen`   | Double-buffered rendering with ANSI diffing           |
| `matrix.input`    | `Input`    | Keyboard, mouse, paste, focus event parsing           |
| `matrix.terminal` | `Terminal` | TTY control and capability detection                  |
| `matrix.ansi`     | `Ansi`     | Low-level ANSI escape sequence generation             |
| `matrix.glyph`    | `Glyph`    | Unicode grapheme cluster management                   |
| `matrix.terminfo` | `Terminfo` | Terminal capability database                          |
| `matrix.pty`      | `Pty`      | Pseudo-terminal spawning (POSIX + Windows ConPTY)     |
| `matrix.vte`      | `Vte`      | Virtual terminal emulator for embedding output        |

API documentation is available in the corresponding `.mli` files under `lib/`.

## Acknowledgements

Matrix draws inspiration from several excellent projects:

- [Notty](https://github.com/pqwy/notty) – Declarative terminal graphics for OCaml - our `Image` API is directly inspired by Notty's.
- [OpenTUI](https://github.com/sst/opentui/) – TypeScript library for building terminal user interfaces (TUIs).
- [Rich](https://github.com/Textualize/rich) – Python library for rich text and beautiful formatting in the terminal.

## License

Matrix is licensed under the ISC license. See [LICENSE](../LICENSE) for details.
