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
  style:Style.t ->
  dark:bool ->
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
  dark:bool ->
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
  style:Style.t ->
  align:[ `Start | `Center | `End | `Stretch ] ->
  tab_width:int ->
  wrap:bool ->
  dark:bool ->
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
  segments:(string * Style.t) list ->
  dark:bool ->
  unit ->
  unit
(** [draw_rich_text ?clip buffer ~pos ~width ~segments ()] draws a single line
    of text composed of multiple styled segments, clipped to the given width.
    @param pos The starting [(x, y)] position of the rich text line. *)

val draw_line :
  ?clip:Render.Clip.t option ->
  buffer:Render.buffer ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  style:Style.t ->
  dark:bool ->
  kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line ?clip buffer ~x1 ~y1 ~x2 ~y2 ~style ~dark ~kind ()] draws a line
    between two points. *)

val draw_box :
  ?clip:Render.Clip.t option ->
  buffer:Render.buffer ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  style:Style.t ->
  dark:bool ->
  ?border:Border.t ->
  unit ->
  unit
(** [draw_box ?clip buffer ~x ~y ~width ~height ~style ~dark ?border ()] draws a
    rectangle, optionally with a border. If [border] is provided, draws the
    border; otherwise fills the rectangle with the style. *)

(** {2 Layout Utilities} *)

val wrap_text : string -> int -> string list
(** [wrap_text text width] breaks a string into a list of lines, each of which
    fits within the given character width. *)

val distribute_fairly : int -> int list -> int list
(** [distribute_fairly total sizes] distributes a total width across a list of
    sizes, ensuring each size gets at least its minimum while fairly sharing any
    remaining space. *)

val compute_sizes :
  defs:[< `Fixed of int | `Flex of 'a & int ] list ->
  mins:int list ->
  available:int ->
  spacing:int ->
  int list
(** [compute_sizes ~defs ~mins ~available ~spacing] computes the final sizes for
    a list of size definitions, ensuring that each size respects its minimum and
    that the total does not exceed the available space.
    @param defs A list of size definitions, either fixed or flexible.
    @param mins The minimum sizes for each definition.
    @param available The total available width to distribute.
    @param spacing The spacing between elements. *)
