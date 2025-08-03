(** Theme support for the UI library.

    Themes provide a way to define consistent styling across an application,
    including colors, typography, spacing, and other visual properties. *)

type font_styles = {
  heading : Style.t;  (** Style for headings *)
  body : Style.t;  (** Style for body text *)
  code : Style.t;  (** Style for code/monospace text *)
  emphasis : Style.t;  (** Style for emphasized text *)
}

type t = {
  name : string;
      (** The name of the theme (e.g., "default", "dark", "light") *)
  primary : Style.color;  (** Primary brand color *)
  secondary : Style.color;  (** Secondary brand color *)
  background : Style.color;  (** Default background color *)
  surface : Style.color;  (** Color for surfaces like cards, modals *)
  error : Style.color;  (** Color for error states *)
  warning : Style.color;  (** Color for warning states *)
  success : Style.color;  (** Color for success states *)
  info : Style.color;  (** Color for informational states *)
  text_primary : Style.color;  (** Primary text color *)
  text_secondary : Style.color;  (** Secondary/muted text color *)
  border : Style.color;  (** Default border color *)
  border_style : Border.line_style;  (** Default border style *)
  spacing_unit : int;  (** Base spacing unit in characters (default: 1) *)
  font_styles : font_styles;
}

val default_dark : t
(** Default dark theme *)

val default_light : t
(** Default light theme *)

(** {2 Built-in Themes} *)

val dracula : t
(** Dracula theme - a dark theme with vibrant colors *)

val monokai : t
(** Monokai theme - popular among developers *)

val nord : t
(** Nord theme - an arctic, north-bluish color palette *)

val gruvbox : t
(** Gruvbox theme - retro groove color scheme *)

val tokyo_night : t
(** Tokyo Night theme - a clean dark theme *)

val catppuccin_mocha : t
(** Catppuccin Mocha theme - a soothing pastel theme *)

val solarized_dark : t
(** Solarized Dark theme - precision colors for machines and people *)

val solarized_light : t
(** Solarized Light theme - precision colors for machines and people *)

val all_themes : (string * t) list
(** List of all built-in themes as (name, theme) pairs *)

val get_theme : string -> t option
(** [get_theme name] returns the theme with the given name, if it exists *)

val make :
  ?name:string ->
  ?primary:Style.color ->
  ?secondary:Style.color ->
  ?background:Style.color ->
  ?surface:Style.color ->
  ?error:Style.color ->
  ?warning:Style.color ->
  ?success:Style.color ->
  ?info:Style.color ->
  ?text_primary:Style.color ->
  ?text_secondary:Style.color ->
  ?border:Style.color ->
  ?border_style:Border.line_style ->
  ?spacing_unit:int ->
  ?heading_style:Style.t ->
  ?body_style:Style.t ->
  ?code_style:Style.t ->
  ?emphasis_style:Style.t ->
  unit ->
  t
(** Create a custom theme with optional overrides of the default values *)

val with_theme : t -> Style.t -> Style.t
(** [with_theme theme style] applies theme colors to a style. This replaces any
    [Default] colors in the style with theme colors. *)

val spacing : t -> int -> int
(** [spacing theme n] returns n * theme.spacing_unit *)
