external language_ptr : unit -> nativeint = "caml_tree_sitter_json_language"

let language () = Tree_sitter.Language.of_address (language_ptr ())
