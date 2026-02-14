(** Off-screen drawing canvas for procedural rendering.

    Canvas wraps an off-screen {!Grid.t} with a renderable node, enabling
    procedural graphics within the layout system. The canvas surface resizes to
    match the renderable's layout dimensions during rendering.

    {1 Usage}

    Drawing is done via {!set_draw}, which installs a callback invoked on each
    render. Use {!val-grid} to access the underlying {!Grid.t} for drawing:

    {[
      let canvas = Canvas.mount node in
      Canvas.set_draw canvas
        (Some
           (fun grid ~width ~height ->
             Grid.clear grid;
             Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello";
             Grid.draw_line grid ~x1:0 ~y1:1 ~x2:(width - 1) ~y2:1 ()))
    ]}

    Call {!request_render} after modifying the grid outside [set_draw] to
    schedule a redraw.

    {1 Size Management}

    The canvas can report an intrinsic size for layout measurement via
    {!set_intrinsic_size}. Without an intrinsic size, the canvas relies on
    parent constraints or defaults to its initial dimensions. *)

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

val set_draw : t -> (Grid.t -> width:int -> height:int -> unit) option -> unit
(** [set_draw t callback] installs a drawing callback that runs immediately, and
    again whenever canvas dimensions change via the [on_size_change] callback.
    The callback receives the underlying grid for direct drawing. Passing [None]
    removes the callback. *)

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

val grid : t -> Grid.t
(** [grid t] returns the underlying off-screen grid buffer.

    This enables direct access to {!Grid} drawing operations for advanced use
    cases like composing with [Matrix_charts]. The grid dimensions match the
    canvas layout size after rendering. *)

val request_render : t -> unit
(** [request_render t] schedules a redraw of the canvas contents. *)

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
