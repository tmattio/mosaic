(** Scroll bar widget with arrows, slider track, and keyboard navigation.

    Scroll_bar provides a complete scrollbar control with optional arrow buttons
    at each end and an internal slider for the track and thumb. It supports
    keyboard navigation, repeated scrolling when arrow buttons are held, and
    automatic visibility based on content overflow.

    {1 Overview}

    A scroll bar manages scroll position within a range defined by [scroll_size]
    and [viewport_size]. The thumb size automatically reflects the visible
    portion of content. Arrow buttons provide incremental scrolling, and the
    track allows direct position manipulation via mouse or keyboard.

    {1 Coordinate System}

    - [scroll_position]: Current scroll offset in cells
    - [viewport_size]: Size of visible area in cells
    - [scroll_size]: Total content size in cells
    - Valid range: 0 to [max(0, scroll_size - viewport_size)]

    {1 Scroll Units}

    The [scroll_by] function supports four unit types:
    - [`Absolute]: Multiply delta by 1 cell
    - [`Viewport]: Multiply delta by viewport size
    - [`Content]: Multiply delta by content size
    - [`Step]: Multiply delta by custom step size (default 1 if unset) *)

type orientation = [ `Vertical | `Horizontal ]
(** Scroll bar orientation: vertical or horizontal layout. *)

type scroll_unit = [ `Absolute | `Viewport | `Content | `Step ]
(** Units for scroll delta calculations in {!scroll_by}. *)

type arrow_chars = { up : string; down : string; left : string; right : string }
(** Arrow button character overrides for each direction. *)

type arrow_style = {
  foreground : Ansi.Color.t option;
  background : Ansi.Color.t option;
  attributes : Ansi.Attr.flag list option;
  chars : arrow_chars option;
}
(** Arrow button styling with optional character and color overrides.

    When [foreground] is [None], uses [background] as foreground if available.
    Default arrow characters are "▲", "▼", "◀", "▶". *)

type track_style = {
  track_color : Ansi.Color.t option;
  thumb_color : Ansi.Color.t option;
}
(** Slider track and thumb color overrides. *)

module Props : sig
  type t

  val make :
    orientation:orientation ->
    ?show_arrows:bool ->
    ?arrow_style:arrow_style ->
    ?track_style:track_style ->
    ?track_viewport_size:int ->
    ?on_change:(int -> unit) ->
    ?autofocus:bool ->
    unit ->
    t
  (** [make ~orientation ()] constructs scroll bar properties.

      @param orientation Layout direction (required)
      @param show_arrows Display arrow buttons at each end. Default is [false].
      @param arrow_style Arrow button styling. Default uses built-in characters.
      @param track_style Track and thumb color overrides.
      @param track_viewport_size
        Initial logical viewport size for thumb sizing before real viewport is
        set via {!set_viewport_size}.
      @param on_change Callback fired when scroll position changes.
      @param autofocus Request focus on mount. Default is [false]. *)

  val default_vertical : t
  (** [default_vertical] is the default vertical scroll bar properties. *)

  val default_horizontal : t
  (** [default_horizontal] is the default horizontal scroll bar properties. *)

  val equal : t -> t -> bool
  (** [equal a b] checks structural equality of scroll bar properties. *)
end

type t
(** Scroll bar state. *)

val apply_props : t -> Props.t -> unit
(** [apply_props bar props] applies [props] to a mounted scroll bar using its
    setters where supported. Creation-time fields such as [orientation] remain
    unchanged. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a scroll bar and marks it
    focusable so it can receive keyboard scrolling events. Arrow buttons and
    slider track are created as child renderables. *)

val node : t -> Renderable.t
(** [node t] returns the root renderable node. *)

val set_on_change : t -> (int -> unit) option -> unit
(** [set_on_change t callback] registers a callback triggered when scroll
    position changes. Passes the new position as an integer. *)

val scroll_position : t -> int
(** [scroll_position t] returns current scroll offset in cells. *)

val set_scroll_position : ?emit:bool -> t -> int -> unit
(** [set_scroll_position ?emit bar value] sets the scroll position, clamped to
    valid range [0, max(0, scroll_size - viewport_size)].

    @param emit
      When [true] (default), triggers [on_change] callback. Set to [false] for
      programmatic updates without notifications. *)

val scroll_size : t -> int
(** [scroll_size t] returns total content size in cells. *)

val set_scroll_size : t -> int -> unit
(** [set_scroll_size t size] updates content size used to compute valid scroll
    range. Clamps current position if needed. *)

val viewport_size : t -> int
(** [viewport_size t] returns visible area size in cells. *)

val set_viewport_size : t -> int -> unit
(** [set_viewport_size t size] updates visible area size. Affects thumb sizing
    and scroll range calculations. *)

val set_show_arrows : t -> bool -> unit
(** [set_show_arrows t flag] shows or hides arrow buttons. *)

val update_arrow_style : t -> arrow_style -> unit
(** [update_arrow_style t style] applies new styling to arrow buttons. *)

val update_track_style : t -> track_style -> unit
(** [update_track_style t style] applies new styling to slider track and thumb.
*)

val set_track_viewport_size : t -> int option -> unit
(** [set_track_viewport_size t size] sets an initial logical viewport size used
    to compute thumb size before a real [viewport_size] is provided via
    {!set_viewport_size}. *)

val set_scroll_step : t -> int option -> unit
(** [set_scroll_step t step] sets optional logical step size used by
    {!scroll_by} when [unit] is [`Step]. Default is 1 if unset. *)

val scroll_by : t -> float -> unit:scroll_unit -> unit
(** [scroll_by t delta ~unit] adjusts scroll position by [delta] expressed in
    [unit]. The delta is multiplied by the unit's cell value and added to the
    current position. Position is clamped to valid range after adjustment. *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] processes keyboard interactions for scrolling. Returns
    [true] when the event is consumed, [false] otherwise. Supports Up/Down or
    Left/Right depending on orientation. *)

val reset_visibility_control : t -> unit
(** [reset_visibility_control t] re-enables automatic visibility toggling based
    on content overflow after manual visibility override via
    {!set_visible_override}. *)

val set_visible_override : t -> bool -> unit
(** [set_visible_override t visible] explicitly sets visibility and disables
    automatic visibility control until {!reset_visibility_control} is called.
    Useful for maintaining scroll bar visibility regardless of content state. *)
