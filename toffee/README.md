# Toffee

A high-performance CSS layout engine for OCaml.

Toffee is a pure OCaml port of
[Taffy](https://github.com/DioxusLabs/taffy), a battle‑tested CSS layout
library written in Rust. It implements the CSS **Block**, **Flexbox**, and
**Grid** layout algorithms and is designed to be embedded in UI frameworks,
terminal applications, game engines, or any project that needs reliable 2D
layout.

## Why Toffee?

- **CSS Grid, Flexbox, and Block** – Faithful implementations of CSS Grid
  Level 1, Flexbox, and traditional block layout with margin collapsing.
- **Pure OCaml** – No C stubs, no runtime dependencies beyond the OCaml
  standard library. Works wherever OCaml 5 runs.
- **High performance** – Arena‑style node storage, layout caching, and tight
  loops closely follow Taffy’s design to minimise allocations and GC pressure.
- **Composable architecture** – Use the high‑level `Toffee` tree API, or the
  lower‑level `toffee.tree` and `toffee.compute` libraries to plug layout into
  your own node representation.
- **Deterministic, side‑effect‑free layout** – Toffee computes positions and
  sizes; rendering and text layout are left to you. This makes it easy to
  integrate with any UI toolkit or renderer.

## Getting Started

Install via opam:

```bash
opam install toffee
```

Build locally:

```bash
dune build @install
```

Run the test suite:

```bash
dune runtest
```

## Hello Layout

This example creates a simple column layout with a fixed‑height header and a
flex‑growing body:

```ocaml
open Toffee

let () =
  let open Geometry in
  let open Style in

  (* Create a new layout tree *)
  let tree = new_tree () in

  (* Root: a flex column, 800x600 *)
  let root_style =
    make
      ~display:Display.Flex
      ~flex_direction:Flex_direction.Column
      ~size:Size.(make (Dimension.length 800.) (Dimension.length 600.))
      ()
  in

  (* Header: fixed height *)
  let header_style =
    make
      ~size:Size.(make (Dimension.length 800.) (Dimension.length 100.))
      ()
  in

  (* Body: flex_grow = 1.0 to fill the remaining space *)
  let body_style = make ~flex_grow:1. () in

  (* Build the node tree *)
  let root = Result.get_ok (new_leaf tree root_style) in
  let header = Result.get_ok (new_leaf tree header_style) in
  let body = Result.get_ok (new_leaf tree body_style) in

  let () =
    Result.get_ok (set_children tree root [| header; body |]) |> ignore
  in

  (* Compute layout with "max-content" constraints on both axes *)
  let available_space =
    Size.{ width = Available_space.max_content; height = Available_space.max_content }
  in
  Result.get_ok (compute_layout tree root available_space) |> ignore;

  (* Inspect the computed layout for the header *)
  let header_layout = Result.get_ok (layout tree header) in
  Printf.printf "Header: x=%f y=%f width=%f height=%f\n"
    header_layout.location.x header_layout.location.y
    header_layout.size.width header_layout.size.height
```

For more advanced use cases you can pass a custom measure function via
`compute_layout_with_measure` to integrate Toffee’s layout with text shaping,
images, or any other content that needs intrinsic sizing.

## High‑level vs Low‑level APIs

Toffee exposes two layers of abstraction, mirroring Taffy’s design:

- **High‑level API (`Toffee`)** – Manages node storage, caching, and dispatch
  to the correct layout algorithm for you. Use `new_tree`, `new_leaf`,
  `set_children`, `compute_layout`, and `layout` for most applications.
- **Low‑level API (`toffee.tree`, `toffee.compute`)** – Gives you the layout
  traits and algorithms used internally. Implement `Tree.LAYOUT_PARTIAL_TREE`
  for your own tree type and call `Compute.compute_root_layout`,
  `Compute.compute_flexbox_layout`, `Compute.compute_grid_layout`, etc. when
  you need full control.

Refer to the `.mli` files under `lib/` for detailed documentation and type
signatures.

## Learning CSS Layout

Toffee aims to match browser CSS layout semantics, so web‑oriented resources
translate directly:

- [MDN CSS reference](https://developer.mozilla.org/en-US/docs/Web/CSS) for
  property‑by‑property behaviour (e.g. `width`, `flex-grow`,
  `grid-template-columns`).
- Flexbox:
  - [Flexbox Froggy](https://flexboxfroggy.com/)
  - [A Complete Guide To Flexbox](https://css-tricks.com/snippets/css/a-guide-to-flexbox/)
- CSS Grid:
  - [CSS Grid Garden](https://cssgridgarden.com/)
  - [A Complete Guide To CSS Grid](https://css-tricks.com/snippets/css/complete-guide-grid/)

## Acknowledgements

Toffee is a direct port of the Rust crate
[Taffy](https://github.com/DioxusLabs/taffy). Taffy powers projects like
Servo, Bevy, Lapce, Zed, and others; Toffee benefits from that design and
validation work. Huge thanks to the Taffy maintainers and contributors.

## License

Toffee is licensed under the ISC license. See [LICENSE](LICENSE) for details.
