(lang dune 3.19)

(name mosaic)

(generate_opam_files true)

(source
 (github tmattio/mosaic))

(authors "Thibaut Mattio <thibaut.mattio@gmail.com>")

(maintainers "Thibaut Mattio <thibaut.mattio@gmail.com>")

(license ISC)

(implicit_transitive_deps false)

(using menhir 3.0)

(package
 (name mosaic)
 (synopsis
  "A modern terminal UI framework for OCaml with React-inspired hooks")
 (description
  "Mosaic is a high-performance terminal UI framework that brings React's component model and hooks to terminal applications through OCaml 5's algebraic effects. It provides a direct-style API with hooks like use_state and use_effect, a rich component library, and supports both React-style and Elm Architecture patterns. Built on Matrix for terminal I/O and Toffee for CSS layouts.")
 (depends
  (ocaml
   (>= 5.1))
  dune
  matrix
  toffee
  uutf
  uucp
  (uuseg
   (>= 16.0.0))
  eio
  eio_main
  logs
  fmt
  ; for mosaic.markdown
  cmarkit
  ; for mosaic.syntax
  textmate-language
  yojson
  (alcotest :with-test)
  (ppx_expect :with-test))
 (tags
  (tui terminal react hooks "algebraic-effects" "user-interface")))

(package
 (name matrix)
 (synopsis "High-performance terminal infrastructure for OCaml")
 (description
  "Matrix is a highly optimized terminal programming toolkit providing efficient grid representation with damage tracking, double-buffered rendering, and comprehensive Unicode support. It includes an incremental input parser supporting Kitty keyboard protocol, complete ANSI escape sequence generation, terminal control (TTY), pseudo-terminal operations (PTY), and a virtual terminal emulator (VTE). Features mouse support with X10/SGR/URXVT protocols.")
 (depends
  (ocaml
   (>= 5.1))
  dune
  uutf
  uucp
  (uuseg
   (>= 16.0.0))
  fmt
  (alcotest :with-test))
 (tags
  (terminal ansi input grid rendering unicode pty vte)))

(package
 (name matrix-term)
 (synopsis "Terminal emulator example using Matrix and SDL2")
 (description
  "Matrix-term is an example terminal emulator application demonstrating Matrix's virtual terminal emulator (VTE) capabilities. It uses SDL2 for rendering and showcases how to build a fully functional terminal emulator with Matrix's infrastructure.")
 (depends
  ocaml
  dune
  matrix
  tsdl
  tsdl-ttf
  (alcotest :with-test)))

(package
 (name vcr)
 (synopsis "Terminal session recording and playback tool")
 (description
  "VCR is a comprehensive tool for recording, replaying, and converting terminal sessions. It captures terminal output and input events with accurate timing to create reproducible terminal animations, demos, and documentation. Supports various export formats for sharing terminal sessions.")
 (depends
  ocaml
  dune
  matrix
  menhir
  vg
  cmdliner
  logs
  fmt
  (alcotest :with-test))
 (tags
  (terminal recording playback demo)))

(package
 (name toffee)
 (synopsis "Pure OCaml port of the Taffy CSS layout engine")
 (description
  "Toffee is a high-performance CSS layout engine providing full CSS Grid Level 1, complete Flexbox algorithm, and traditional block layout with margin collapsing. It's a pure OCaml port of Taffy, enabling responsive terminal layouts with percentage-based sizing, calc() expressions, and the full CSS box model including padding, margin, and borders.")
 (depends
  (ocaml
   (>= 5.1))
  dune
  (alcotest :with-test))
 (tags
  (css layout flexbox grid "css-grid" taffy "layout-engine")))
