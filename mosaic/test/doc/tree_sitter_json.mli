(** The highlighting interface of the tree-sitter bindings' [tree-sitter.json]
    library, without the grammar. *)

val highlight : string -> (int * int * string) list
