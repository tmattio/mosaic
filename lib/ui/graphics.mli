(** Low-level Terminal Graphics Primitives.

    This module provides a stateless toolkit for drawing primitive shapes and
    processing text for display on a [Render.buffer]. It operates on concrete
    geometric and style data and is independent of the high-level UI element or
    layout structures. *)

(** {1 Core Drawing Functions} *)

val fill_rect :
  ?clip:Render.Clip.t option ->
  buffer:Render.buffer ->
  rect:int * int * int * int ->
  style:Render.Style.t ->
  unit ->
  unit
(** [fill_rect ?clip buffer ~rect ~style ()] fills a rectangular area with a
    background style.
    @param rect
      A tuple [(x, y, width, height)] defining the rectangle's geometry. *)

val draw_border :
  ?clip:Render.Clip.t option ->
  buffer:Render.buffer ->
  rect:int * int * int * int ->
  border:Border.t ->
  unit ->
  unit
(** [draw_border ?clip buffer ~rect ~border ()] draws a border within the given
    rectangle's edges.
    @param rect
      A tuple [(x, y, width, height)] defining the outer bounds of the border.
    @param border The [Border.t] specification describing the style and color.
*)

val draw_text :
  ?clip:Render.Clip.t option ->
  buffer:Render.buffer ->
  pos:int * int ->
  bounds:int * int ->
  text:string ->
  style:Render.Style.t ->
  align:[ `Start | `Center | `End | `Stretch ] ->
  tab_width:int ->
  wrap:bool ->
  unit ->
  unit
(** [draw_text ?clip buffer ~pos ~bounds ~text ~style ~align ~tab_width ~wrap
     ()] draws a string of text within a given bounding box.
    @param pos The top-left [(x, y)] starting position for the text block.
    @param bounds The [(width, height)] of the available area for the text. *)

val draw_rich_text :
  ?clip:Render.Clip.t option ->
  buffer:Render.buffer ->
  pos:int * int ->
  width:int ->
  segments:(string * Render.Style.t) list ->
  unit ->
  unit
(** [draw_rich_text ?clip buffer ~pos ~width ~segments ()] draws a single line
    of text composed of multiple styled segments, clipped to the given width.
    @param pos The starting [(x, y)] position of the rich text line. *)

(** {2 Text Processing Utilities} *)

val expand_tabs : string -> int -> string
(** [expand_tabs s tab_width] returns a new string with all tab characters
    replaced by the appropriate number of spaces. *)

val wrap_text : string -> int -> string list
(** [wrap_text text width] breaks a string into a list of lines, each of which
    fits within the given character width. *)

val unicode_substring : string -> int -> string
(** [unicode_substring str max_cells] returns a prefix of a string that fits
    within a given number of terminal cells, respecting Unicode character
    widths. *)
