(** The grammar and query of the tree-sitter bindings' [tree-sitter.json]
    library, without the grammar. *)

val language : unit -> Tree_sitter.Language.t
val highlights_query : string
