(* progress_bar.mli *)

(** Progress bar rendering with animation and rich customization.

    This module provides a customizable progress bar similar to Python's Rich
    library, with support for determinate progress, indeterminate pulsing
    animations, and various styling options. *)

type preset_def = {
  delimiters : (string * string) option;
  filled_char : string;
  empty_char : string;
  progress_stages : string list;
}
(** A record defining the characters used to render a progress bar. *)

(** A collection of pre-defined visual styles for the progress bar. *)
type preset =
  (* Classic Styles *)
  | ASCII  (** A standard ASCII bar: `[###---]` *)
  | UTF8  (** A high-resolution UTF-8 block bar: `│███▋ │` *)
  (* Line Styles *)
  | Line_double  (** Double-line characters: `╢═══───╟` *)
  | Line_single  (** Single-line characters: `├━━━───┤` *)
  | Line_arrow  (** Arrowhead progress: `>>--->` *)
  (* Block Styles *)
  | Block_shade_light  (** Light shading blocks: `▓▓▓░░░` *)
  | Block_shade_medium  (** Medium shading blocks: `███▒▒▒` *)
  | Block_shade_dark  (** Dark shading blocks: `███▓▓▓` *)
  | Block_dotted  (** Dotted blocks: `⣿⣿⣿⣀⣀⣀` *)
  (* Custom record for full control *)
  | Custom of preset_def

val progress_bar :
  ?total:float ->
  ?completed:float ->
  ?width:int ->
  ?pulse:bool ->
  ?animation_time:float ->
  ?bar_style:Style.t ->
  ?complete_style:Style.t ->
  ?finished_style:Style.t ->
  ?pulse_style:Style.t ->
  ?preset:preset ->
  ?delimiters:string * string ->
  ?filled_char:string ->
  ?empty_char:string ->
  ?progress_stages:string list ->
  unit ->
  Element.t
(** [progress_bar ?total ... ()] creates a progress bar element.

    @param total
      The total number of steps. Defaults to `Some 100.0`. Set to `None` for a
      pulsing bar.
    @param completed The number of steps completed. Defaults to `0.0`.
    @param width The total width of the bar in characters. Defaults to 20.
    @param pulse
      If true, force a pulsing animation even if `total` is defined. Defaults to
      `false`.
    @param animation_time
      Time in seconds, used for the pulsing animation. Defaults to `0.0`.

    @param bar_style Style for the empty part of the bar.
    @param complete_style Style for the filled part of the bar.
    @param finished_style
      Style for the bar once it is complete. Overrides `complete_style`.
    @param pulse_style Style for the bright part of the pulsing animation.

    @param preset
      A style preset for the bar's characters. If not provided, a default
      heavy-line style (`━━━━━━━━╸───`) is used. Specific character arguments
      below will override the chosen preset or the default.
    @param delimiters
      A `(left, right)` tuple of strings for the bar's ends. E.g., `Some
      ("[", "]")`.
    @param filled_char
      The character for a fully filled segment. E.g., `"#"` or `"█"`.
    @param empty_char The character for an empty segment. E.g., `"-"` or `" "`.
    @param progress_stages
      A list of characters for sub-character progress, from least to most
      filled. E.g., `[">"]` or `["▏", "▎", "▍", "▌", "▋", "▊", "▉"]`. *)
