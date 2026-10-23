(** The grammars and queries of the tree-sitter bindings' [tree-sitter.ocaml]
    library, without the grammar, so the guide's recipe typechecks without
    depending on it. *)

val ocaml : unit -> Tree_sitter.Language.t
val interface : unit -> Tree_sitter.Language.t
val ocaml_highlights_query : string
val interface_highlights_query : string
