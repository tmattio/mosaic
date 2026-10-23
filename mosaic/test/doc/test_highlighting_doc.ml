(* The guide's snippets are extracted into [Highlighting] by this directory's
   dune rule; building this test typechecks them against the current API and
   the tree-sitter bindings' parsing interface. *)

module type Empty = sig end

let () = ignore (Sys.opaque_identity (module Highlighting : Empty))
