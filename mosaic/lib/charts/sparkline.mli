(** Streaming terminal sparklines. *)

type kind = [ `Bars | `Braille ]
(** Rendering modes:
    - [`Bars]: vertical columns using block characters
    - [`Braille]: higher-resolution line using braille cells *)

type t
(** A mutable, fixed-capacity sparkline buffer. *)

val make :
  width:int ->
  height:int ->
  ?style:Ansi.Style.t ->
  ?auto_max:bool ->
  ?max_value:float ->
  ?data:float list ->
  unit ->
  t
(** [make ~width ~height ?style ?auto_max ?max_value ?data ()] creates a new
    sparkline buffer.
    - [width], [height]: initial drawing dimensions (minimum 1).
    - [style]: text style (foreground/background); default:
      [Ansi.Style.default].
    - [auto_max]: if [true], automatically increases [max_value] when pushed
      values exceed it; default: [true].
    - [max_value]: initial maximum value for scaling; default: [1.0].
    - [data]: optional initial list of values to push. *)

val clear : t -> unit
(** Clear all stored values. *)

val push : t -> float -> unit
(** Append a new sample value. Negative values are clamped to 0. *)

val push_all : t -> float list -> unit
(** Append all values in the given list, in order. *)

val set_max : t -> float -> unit
(** [set_max t m] sets the expected maximum value used for scaling. *)

val resize : t -> width:int -> height:int -> unit
(** Change the logical drawing size. Preserves as many of the most recent values
    as fit in the new width. *)

val draw :
  t ->
  kind:kind ->
  ?columns_only:bool ->
  Mosaic_ui.Canvas.t ->
  width:int ->
  height:int ->
  unit
(** [draw t ~kind ?columns_only canvas ~width ~height] renders the sparkline
    into [canvas] with the given dimensions.

    - [kind]: rendering mode ([`Bars] or [`Braille]).
    - [columns_only]: if [true], only draw the sparkline glyphs and do not fill
      the background, even if [style.bg] is set; default: [false].

    The internal buffer is resized to [width]×[height] before drawing,
    preserving the most recent values. *)
