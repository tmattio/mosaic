(** Scrollbar and slider widget with sub-cell precision.

    Slider provides a draggable scrollbar or value selector with sub-cell
    rendering precision using Unicode half-blocks. It supports horizontal and
    vertical orientations, mouse interaction, and viewport-based thumb sizing.

    {1 Overview}

    Slider uses a virtual coordinate system with 2x resolution per cell,
    enabling smooth thumb positioning and sizing. The thumb size automatically
    adjusts based on [viewport_size] relative to the value range, mimicking
    standard scrollbar behavior.

    The slider can function as either a scrollbar (with viewport sizing) or a
    simple value selector (without viewport sizing). When used as a scrollbar,
    the thumb size indicates the visible portion of content.

    {1 Viewport Sizing}

    The thumb size represents [viewport_size] relative to
    [max - min + viewport_size]. For example, with range 0-100 and viewport 10,
    the thumb occupies 10/110 of the track. This matches standard scrollbar
    behavior where thumb size indicates visible portion.

    When [viewport_size] is not set, the thumb has a fixed minimal size suitable
    for value selection rather than scroll indication.

    {1 Mouse Interaction}

    The slider supports mouse drag interactions for repositioning the thumb.
    Click and drag on the thumb to adjust the value. Clicking on the track jumps
    the value to the clicked position. *)

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
  (** [make ~orientation ()] constructs slider properties.

      @param orientation Layout direction (required)
      @param min Minimum value. Default is 0.0.
      @param max Maximum value. Default is 100.0.
      @param value Initial value. Default is 0.0, clamped to [min..max].
      @param viewport_size
        Optional viewport size for scrollbar-style thumb sizing. When set, thumb
        size reflects visible portion. When unset, thumb has minimal fixed size.
      @param track_color Background track color.
      @param thumb_color Draggable thumb color.
      @param on_change Callback fired when value changes. *)

  val equal : t -> t -> bool
  (** [equal a b] checks structural equality of slider properties. *)

  val orientation : t -> orientation
  (** [orientation t] returns the slider orientation. *)

  val min : t -> float
  (** [min t] returns the minimum value. *)

  val max : t -> float
  (** [max t] returns the maximum value. *)

  val value : t -> float
  (** [value t] returns the current value. *)

  val viewport_size : t -> float option
  (** [viewport_size t] returns the viewport size if set. *)

  val track_color : t -> Ansi.Color.t
  (** [track_color t] returns the track background color. *)

  val thumb_color : t -> Ansi.Color.t
  (** [thumb_color t] returns the thumb color. *)

  val on_change : t -> (float -> unit) option
  (** [on_change t] returns the value change callback if set. *)
end

type t
(** Slider state. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a slider with sub-cell
    precision rendering. Wires up mouse event handlers for drag interactions. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val value : t -> float
(** [value t] returns current slider value in range [min..max]. *)

val set_value : t -> float -> unit
(** [set_value t value] updates slider position. Value is clamped to [min..max].
    Triggers [on_change] callback and re-render. *)

val set_range : t -> min:float -> max:float -> unit
(** [set_range t ~min ~max] updates value range. Automatically swaps [min] and
    [max] if [max < min]. Clamps current value to new range and triggers
    re-render. *)

val set_viewport_size : t -> float -> unit
(** [set_viewport_size t size] updates viewport size for thumb calculation.
    Clamped to maximum of range size [max - min] with minimum 0.01. Affects
    thumb visual size to indicate visible portion. *)

val set_track_color : t -> Ansi.Color.t -> unit
(** [set_track_color t color] updates background track color. Triggers
    re-render. *)

val set_thumb_color : t -> Ansi.Color.t -> unit
(** [set_thumb_color t color] updates draggable thumb color. Triggers re-render.
*)

val set_on_change : t -> (float -> unit) option -> unit
(** [set_on_change t callback] registers value change handler. Callback receives
    the new value when the slider position changes. *)

val apply_props : t -> Props.t -> unit
(** [apply_props slider props] applies [props] to a mounted slider using its
    setters for all mutable properties. Creation-time [orientation] remains
    unchanged. *)
