(** Scrollbar and slider widget with sub-cell precision.

    Slider provides a draggable scrollbar or value selector with sub-cell
    rendering precision using Unicode half-blocks. It supports horizontal and
    vertical orientations, mouse interaction, and viewport-based thumb sizing.

    {1 Overview}

    Slider uses a virtual coordinate system with 2x resolution per cell,
    enabling smooth thumb positioning and sizing. The thumb size automatically
    adjusts based on [viewport_size] relative to the value range, mimicking
    standard scrollbar behavior.

    {1 Viewport Sizing}

    The thumb size represents [viewport_size] relative to
    [max - min + viewport_size]. For example, with range 0-100 and viewport 10,
    the thumb occupies 10/110 of the track. This matches standard scrollbar
    behavior where thumb size indicates visible portion. *)

type orientation = [ `Horizontal | `Vertical ]
(** Slider orientation: horizontal or vertical layout. *)

module Props : sig
  type t

  val make :
    orientation:orientation ->
    ?min:float ->
    ?max:float ->
    ?value:float ->
    ?viewport_size:float ->
    ?track_color:Ansi.Color.t ->
    ?thumb_color:Ansi.Color.t ->
    ?on_change:(float -> unit) ->
    unit ->
    t

  val equal : t -> t -> bool
  val orientation : t -> orientation
  val min : t -> float
  val max : t -> float
  val value : t -> float
  val viewport_size : t -> float option
  val track_color : t -> Ansi.Color.t
  val thumb_color : t -> Ansi.Color.t
  val on_change : t -> (float -> unit) option
end

type t

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a slider. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val value : t -> float
(** [value t] returns current slider value. *)

val set_value : t -> float -> unit
(** [set_value t value] updates slider position. Value is clamped to [min..max].
    Fires [on_change] callback. *)

val set_range : t -> min:float -> max:float -> unit
(** [set_range t ~min ~max] updates value range. Automatically swaps if
    [max < min]. Clamps current value to new range. *)

val set_viewport_size : t -> float -> unit
(** [set_viewport_size t size] updates viewport size for thumb calculation.
    Clamped to range size with minimum 0.01. *)

val set_track_color : t -> Ansi.Color.t -> unit
(** [set_track_color t color] updates background track color. *)

val set_thumb_color : t -> Ansi.Color.t -> unit
(** [set_thumb_color t color] updates draggable thumb color. *)

val set_on_change : t -> (float -> unit) option -> unit
(** [set_on_change t callback] registers value change handler. *)

val apply_props : t -> Props.t -> unit
(** [apply_props slider props] applies [props] to a mounted slider using its
    setters. *)
