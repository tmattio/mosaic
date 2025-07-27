(** Color themes for syntax highlighting *)

type theme_def = (string * Ui.Style.t) list
(** A theme definition is a list of scope-to-style mappings. *)

type theme = [ `Dracula | `Custom of (string * Ui.Style.t) list ]
(** The supported themes for syntax highlighting. Use [`Custom] to provide a
    custom theme definition. *)

val dracula : theme_def
(** The Dracula theme - a popular dark theme with vibrant colors. Based on
    https://github.com/zenorocha/dracula-theme *)

val get_theme_def : theme -> theme_def
(** Get the theme definition for a theme variant. *)
