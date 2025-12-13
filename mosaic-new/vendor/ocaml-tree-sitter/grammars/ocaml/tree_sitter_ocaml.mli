(** OCaml grammar bindings for Tree-sitter.

    This module provides Tree-sitter language grammars for parsing OCaml source
    code. OCaml has three distinct syntactic contexts, each with its own
    grammar:

    - Implementation files ([.ml]) use the {!ocaml} grammar for full OCaml
      expressions, modules, and statements.
    - Interface files ([.mli]) use the {!interface} grammar for signatures and
      type declarations.
    - Type expressions use the {!type_} grammar for standalone type parsing.

    Each grammar is exposed as a {!Tree_sitter.Language.t} value that can be
    assigned to a {!Tree_sitter.Parser.t} for parsing source code into concrete
    syntax trees.

    This module requires the [tree-sitter-ocaml] C library to be linked. The
    library provides the underlying parser implementation; this module exposes
    it to OCaml.

    {1 Usage}

    Create a parser for OCaml implementation files:
    {[
      open Tree_sitter

      let parser = Parser.create () in
      Parser.set_language parser (Tree_sitter_ocaml.ocaml ());

      let source = "let add x y = x + y" in
      let tree = Parser.parse_string parser source in
      let root = Tree.root_node tree in
      Printf.printf "Parsed: %s\n" (Node.kind root)
    ]}

    Parse an OCaml interface file:
    {[
      let parser = Parser.create () in
      Parser.set_language parser (Tree_sitter_ocaml.interface ());

      let source = "val add : int -> int -> int" in
      let tree = Parser.parse_string parser source
    ]}

    Parse a standalone type expression:
    {[
      let parser = Parser.create () in
      Parser.set_language parser (Tree_sitter_ocaml.type_ ());

      let source = "int -> string -> bool" in
      let tree = Parser.parse_string parser source
    ]} *)

val ocaml : unit -> Tree_sitter.Language.t
(** [ocaml ()] returns the Tree-sitter language grammar for OCaml implementation
    files.

    Use this grammar to parse [.ml] files containing OCaml expressions,
    declarations, modules, and other implementation constructs.

    The grammar supports the full OCaml syntax including modules, functors,
    objects, polymorphic variants, and all expression forms. *)

val interface : unit -> Tree_sitter.Language.t
(** [interface ()] returns the Tree-sitter language grammar for OCaml interface
    files.

    Use this grammar to parse [.mli] files containing module signatures, type
    declarations, value specifications, and other interface constructs.

    The grammar parses signature items such as [val], [type], [module],
    [exception], and [class] declarations. *)

val type_ : unit -> Tree_sitter.Language.t
(** [type_ ()] returns the Tree-sitter language grammar for standalone OCaml
    type expressions.

    Use this grammar to parse type expressions in isolation, such as function
    types, polymorphic types, tuple types, and variant types.

    This is useful for tools that need to parse type annotations independently
    of full OCaml source files. *)
