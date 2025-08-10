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
 (synopsis "A delightful TUI framework for OCaml inspired by Bubbletea")
 (description
  "Mosaic is a functional and composable framework for building terminal user interfaces in OCaml, inspired by Go's Bubbletea and The Elm Architecture")
 (depends
  (ocaml
   (>= 5.0.0))
  dune
  matrix
  toffee
  uutf
  uucp
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
  (tui terminal "user interface" elm)))

(package
 (name matrix)
 (synopsis "The infrastructure to build OCaml terminal applications")
 (description
  "Matrix is a foundational terminal programming toolkit that provides core terminal libraries. It includes input event parsing with support for modern keyboard protocols, complete ANSI escape sequence parser and generation, low-level terminal device control, pseudo-terminal (PTY) operations for terminal multiplexing, and a virtual terminal emulator (VTE) for terminal recording and testing.")
 (depends
  (ocaml
   (>= 5.0.0))
  dune
  uutf
  uucp
  uuseg
  fmt
  (alcotest :with-test))
 (tags
  (terminal input ansi pty vte)))

(package
 (name matrix-term)
 (synopsis "A terminal emulator built with Matrix and tsdl")
 (description "A terminal emulator application using the Matrix terminal toolkit and SDL2 for rendering")
 (depends
  ocaml
  dune
  matrix
  tsdl
  tsdl-ttf
  (alcotest :with-test)))

(package
 (name vcr)
 (synopsis "")
 (description "")
 (depends
  ocaml
  dune
  matrix
  menhir
  vg
  cmdliner
  logs
  fmt
  (alcotest :with-test)))

