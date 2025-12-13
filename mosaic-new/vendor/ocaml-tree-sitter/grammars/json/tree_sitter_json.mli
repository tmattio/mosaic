(** Tree-sitter JSON grammar bindings.

    This module provides access to the Tree-sitter grammar for JSON. Use it to
    parse JSON documents into concrete syntax trees with {!Tree_sitter.Parser}.

    Tree-sitter grammars enable incremental parsing and syntax tree queries. The
    JSON grammar follows RFC 8259.

    This module requires the [tree-sitter-json] C library to be linked. The
    library provides the underlying parser implementation; this module exposes
    it to OCaml.

    {1 Usage}

    Parse a JSON string:
    {[
      let parser = Tree_sitter.Parser.create () in
      Tree_sitter.Parser.set_language parser (Tree_sitter_json.language ());
      let source = {|{"key": "value", "number": 42}|} in
      let tree = Tree_sitter.Parser.parse_string parser source in
      let root = Tree_sitter.Tree.root_node tree in
      assert (Tree_sitter.Node.kind root = "document")
    ]}

    Query for specific nodes:
    {[
      let parser = Tree_sitter.Parser.create () in
      Tree_sitter.Parser.set_language parser (Tree_sitter_json.language ());
      let tree = Tree_sitter.Parser.parse_string parser {|[1, 2, 3]|} in
      let root = Tree_sitter.Tree.root_node tree in
      assert (Tree_sitter.Node.child_count root > 0)
    ]} *)

val language : unit -> Tree_sitter.Language.t
(** [language ()] returns the Tree-sitter JSON language.

    This language object configures parsers to recognize JSON syntax. It defines
    node types such as ["document"], ["object"], ["array"], ["string"], and
    ["number"].

    Call this once per parser via {!Tree_sitter.Parser.set_language}. The
    language object is immutable and safe to share across parsers. *)
