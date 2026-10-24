val highlight :
  language:string -> content:string -> Mosaic_ui.Syntax_highlight.t
(** [highlight ~language ~content] colors strings, numbers, and a small keyword
    set in the examples' OCaml and JSON snippets. This is a lexical
    demonstration, not a parser; unknown languages have no keyword rules. *)
