(** The highlighting interface of the tree-sitter bindings' [tree-sitter.ocaml]
    library, without the grammar, so the guide's recipe typechecks without
    depending on it. *)

val highlight : string -> (int * int * string) list
val highlight_interface : string -> (int * int * string) list
val highlight_type : string -> (int * int * string) list
