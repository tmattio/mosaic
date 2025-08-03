# Toffee

A CSS layout engine written in OCaml.

Toffee is a flexible, high-performance CSS layout engine that implements CSS Grid, Flexbox, and Block layout algorithms. It is a port of [Taffy](https://github.com/DioxusLabs/taffy) to OCaml.

## Features

- **CSS Grid Layout** - Full CSS Grid Level 1 implementation
- **Flexbox Layout** - Complete Flexbox implementation  
- **Block Layout** - Traditional block layout with margin collapsing
- **High Performance** - Optimized layout algorithms
- **Pure OCaml** - No external dependencies

## Installation

```bash
opam install toffee
```

## Usage

```ocaml
open Toffee

(* Create a layout tree *)
let tree = create () in

(* Create nodes with styles *)
let root_style = { Style.default with 
  display = Flex; 
  flex_direction = Row;
  size = { width = Dimension.percent 1.0; height = Dimension.percent 1.0 }
} in
let child_style = { Style.default with flex_grow = 1.0 } in

let root = new_leaf tree root_style in
let child1 = new_leaf tree child_style in
let child2 = new_leaf tree child_style in

(* Build tree structure *)
let () = Result.get_ok (add_child tree root child1) in
let () = Result.get_ok (add_child tree root child2) in

(* Compute layout *)
let available_space = { 
  width = Style.Available_space.Definite 800.0; 
  height = Style.Available_space.Definite 600.0 
} in
let () = Result.get_ok (compute_layout tree root available_space) in

(* Get computed positions and sizes *)
let child1_layout = Result.get_ok (layout tree child1) in
Printf.printf "Child 1: x=%f y=%f width=%f height=%f\n"
  child1_layout.location.x child1_layout.location.y 
  child1_layout.size.width child1_layout.size.height
```

## License

Toffee is licensed under the ISC license. See [LICENSE](LICENSE) for details.

## Acknowledgments

This project is based on [Taffy](https://github.com/DioxusLabs/taffy), a high-performance CSS layout library written in Rust. Thank you to the Taffy contributors for their excellent work.