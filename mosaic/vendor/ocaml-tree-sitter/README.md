# ocaml-tree-sitter

OCaml bindings to [Tree-sitter](https://tree-sitter.github.io/tree-sitter/), an
incremental parsing system for programming tools.

## Overview

Tree-sitter is a parser generator tool and incremental parsing library. It can
build a concrete syntax tree for a source file and efficiently update the syntax
tree as the source file is edited. This makes it ideal for use in text editors,
syntax highlighters, code formatters, and other programming tools.

This library provides OCaml bindings to the Tree-sitter C API, allowing you to:

- Parse source code into concrete syntax trees
- Query syntax trees using Tree-sitter's pattern-matching query language
- Incrementally re-parse edited source code
- Walk and inspect syntax tree nodes

## Installation

```bash
opam install tree-sitter
```

## Usage

```ocaml
open Tree_sitter

(* Load a language grammar *)
let language = Language.ocaml ()

(* Create a parser *)
let parser = Parser.create ()
let () = Parser.set_language parser language

(* Parse some source code *)
let source = "let x = 42"
let tree = Parser.parse_string parser source

(* Access the root node *)
let root = Tree.root_node tree

(* Inspect the tree *)
let () = Printf.printf "Root type: %s\n" (Node.kind root)
let () = Printf.printf "Child count: %d\n" (Node.child_count root)
```

## Language Grammars

Tree-sitter requires a language-specific grammar to parse source code. This
library provides bindings to load grammars. You can use pre-built grammars from
the `tree-sitter-<language>` packages or load custom grammars.

## License

This project is licensed under the MIT license. See [LICENSE](LICENSE) for
details.
