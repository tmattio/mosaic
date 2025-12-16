(** Off-screen drawing canvas for procedural rendering.

    Canvas provides an off-screen grid buffer for imperative drawing operations.
    It supports text plotting, shapes, lines, and fills with automatic size
    tracking and layout integration. The canvas surface expands to match the
    renderable's layout dimensions during rendering.

    {1 Overview}

    Canvas combines an off-screen {!Grid.t} with a renderable node, enabling
    procedural graphics within the layout system. The canvas supports sub-cell
    line rendering via Braille characters.

    {1 Size Management}

    The canvas tracks two sizes:
    - Content size: Expands automatically when drawing outside current bounds.
      Drawing operations automatically expand content size to encompass drawn
      regions.
    - Intrinsic size: Explicit override for measurement, set via
      {!set_intrinsic_size}

    Measurement uses intrinsic size if set, otherwise content size. *)

module Props : sig
  type t

  val make :
    ?respect_alpha:bool ->
    ?width_method:Glyph.width_method ->
    ?initial_width:int ->
    ?initial_height:int ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

type t

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a canvas. This wires the
    render and measure functions and initialises the off-screen surface. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val set_draw : t -> (t -> width:int -> height:int -> unit) option -> unit
(** [set_draw t callback] installs a drawing callback that runs immediately, and
    again whenever canvas dimensions change via the [on_size_change] callback.
    Passing [None] removes the callback. *)

val set_on_resize : t -> (width:int -> height:int -> unit) option -> unit
(** [set_on_resize t callback] installs a resize callback that is invoked
    whenever the canvas size changes. This is useful for interactive
    applications that need to respond to layout changes, such as updating
    hit-testing layouts for charts. The callback receives the new width and
    height. Passing [None] removes the callback. *)

val width : t -> int
(** [width t] returns current off-screen buffer width in cells. *)

val height : t -> int
(** [height t] returns current off-screen buffer height in cells. *)

val request_render : t -> unit
(** [request_render t] schedules a redraw of the canvas contents. *)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear t] resets the canvas to a solid fill.

    All content is cleared and content size is reset to zero. Default color is
    fully transparent.

    @param color Fill color. Default is transparent (RGBA 0,0,0,0). *)

val plot : t -> x:int -> y:int -> ?style:Ansi.Style.t -> string -> unit
(** [plot t ~x ~y text] draws [text] starting at cell [(x, y)].

    The canvas expands if needed to accommodate the text. Only single-line text
    is supported; newlines are not processed and render as visible characters.

    @param style Text styling. Default is {!Ansi.Style.default}. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect t ~x ~y ~width ~height ~color] fills a rectangular region.

    The [color] applies to both foreground and background of all cells in the
    rectangle. Negative or zero dimensions are ignored. *)

val draw_box :
  t ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  ?border_style:Grid.Border.t ->
  ?border_sides:Grid.Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  unit ->
  unit
(** [draw_box t ~x ~y ~width ~height ()] draws a bordered box.

    @param border_style Border character set. Default is [Grid.Border.single].
    @param border_sides Sides to draw. Default is all sides.
    @param border_color Border color. Default is {!Ansi.Color.default}.
    @param background Interior fill color. Default is transparent.
    @param title Optional title text drawn on the top border.
    @param title_alignment
      Optional title alignment: [`Left] (default), [`Center], or [`Right].
      Left/right use a 2‑cell padding from the edge. *)

val draw_line :
  t ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  ?style:Ansi.Style.t ->
  ?kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line t ~x1 ~y1 ~x2 ~y2 ()] draws a line from [(x1, y1)] to [(x2, y2)].

    Line rendering uses Bresenham's algorithm. [`Braille] mode provides 2x4
    sub-cell resolution per cell using Unicode Braille patterns.

    @param kind Line rendering mode. Default is [`Line].
    @param style Line styling. Default is {!Ansi.Style.default}. *)

val set_intrinsic_size : t -> width:int -> height:int -> unit
(** [set_intrinsic_size t ~width ~height] overrides measurement size.

    The canvas reports this size during layout instead of content size. Useful
    for fixed-size canvases. Dimensions are clamped to minimum 1. *)

val clear_intrinsic_size : t -> unit
(** [clear_intrinsic_size t] removes intrinsic size override.

    Measurement falls back to content-driven size based on drawing operations.
*)

val apply_props : t -> Props.t -> unit
(** [apply_props canvas props] applies [props] to a mounted canvas where
    possible. Resizing the underlying surface remains creation-time only. *)
