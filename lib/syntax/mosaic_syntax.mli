(** Syntax highlighting for terminal UIs using Mosaic. *)

type lang =
  [ `OCaml
  | `OCaml_interface
  | `Dune
  | `Shell
  | `Diff
  | `Custom of TmLanguage.grammar ]
(** The supported languages for syntax highlighting. Use [`Custom] to provide a
    custom TextMate grammar. *)

type theme =
  [ `Dracula
  | `Solarized_dark
  | `Solarized_light
  | `Custom of (string * Ui.Style.t) list ]
(** The available themes for syntax highlighting. *)

type error = [ `Unknown_lang of string ]
(** The error type for highlighting operations. *)

val default_dark_theme : theme
(** A pre-defined dark theme suitable for most terminal backgrounds. *)

val highlight :
  ?theme:theme ->
  ?tm:TmLanguage.t ->
  lang:lang ->
  string ->
  (Ui.element, error) result
(** [highlight ?theme ?tm ~lang src] produces a `Ui.element` for the highlighted
    source code [src].

    @param theme
      The theme to use for styling. Defaults to {!default_dark_theme}.
    @param tm
      A pre-existing TextMate engine instance. If not provided, one will be
      created with the default grammars.
    @param lang The language to highlight.
    @param src The source code to highlight.
    @return
      A `Ui.element` on success, or an `error` if the language is not found. *)
