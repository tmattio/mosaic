external ocaml_ptr : unit -> nativeint = "caml_tree_sitter_ocaml_language"

external interface_ptr : unit -> nativeint
  = "caml_tree_sitter_ocaml_interface_language"

external type_ptr : unit -> nativeint = "caml_tree_sitter_ocaml_type_language"

let ocaml () = Tree_sitter.Language.of_address (ocaml_ptr ())
let interface () = Tree_sitter.Language.of_address (interface_ptr ())
let type_ () = Tree_sitter.Language.of_address (type_ptr ())
