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
  uutf
  uucp
  eio
  eio_main
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
 (name vcr)
 (synopsis "")
 (description "")
 (depends
  ocaml
  dune
  menhir
  vg
  cmdliner
  (alcotest :with-test)))
