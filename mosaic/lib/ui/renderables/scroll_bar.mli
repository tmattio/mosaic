(** Scroll bar renderable composed of child slider and arrow renderables.

    Provides programmatic scrolling, keyboard handling, and optional repeated
    scrolling when holding arrow buttons. *)

type orientation = [ `Vertical | `Horizontal ]
type scroll_unit = [ `Absolute | `Viewport | `Content | `Step ]
type arrow_chars = { up : string; down : string; left : string; right : string }

type arrow_style = {
  foreground : Ansi.Color.t option;
  background : Ansi.Color.t option;
  attributes : Ansi.Attr.flag list option;
  chars : arrow_chars option;
}

type track_style = {
  track_color : Ansi.Color.t option;
  thumb_color : Ansi.Color.t option;
}

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

  val default_vertical : t
  val default_horizontal : t
  val equal : t -> t -> bool
end

type t

val apply_props : t -> Props.t -> unit
(** [apply_props bar props] applies [props] to a mounted scroll bar using its
    setters where supported. Creation-time fields such as [orientation] remain
    unchanged. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render a scroll bar and marks it
    focusable so it can receive keyboard scrolling events. *)

val node : t -> Renderable.t
(** Root renderable node for mounting in the tree. *)

val set_on_change : t -> (int -> unit) option -> unit
(** Register callback triggered when scroll position changes. Positions are
    integers. *)

val scroll_position : t -> int

val set_scroll_position : ?emit:bool -> t -> int -> unit
(** [set_scroll_position ?emit bar value] sets the position, clamped to valid
    range. When [emit] is [true] (default), the on_change callback is triggered.
*)

val scroll_size : t -> int

val set_scroll_size : t -> int -> unit
(** Content size used to compute scroll range. *)

val viewport_size : t -> int

val set_viewport_size : t -> int -> unit
(** Visible portion size. *)

val set_show_arrows : t -> bool -> unit
(** Show or hide arrow buttons. *)

val update_arrow_style : t -> arrow_style -> unit
(** Apply new styling to arrows. *)

val update_track_style : t -> track_style -> unit
(** Apply new styling to underlying slider. *)

val set_track_viewport_size : t -> int option -> unit
(** Set an initial logical viewport size for the track used to compute the thumb
    size before a real [viewport_size] is provided. *)

val set_scroll_step : t -> int option -> unit
(** Set optional logical "step" size used for [scroll_by] with [`Step] unit. *)

val scroll_by : t -> float -> unit:scroll_unit -> unit
(** Adjust scroll position by a delta expressed in the selected unit. *)

val handle_key : t -> Event.key -> bool
(** Process keyboard interaction, returning [true] when consumed. *)

val reset_visibility_control : t -> unit
(** Allow automatic visibility toggling again after manual changes. *)

val set_visible_override : t -> bool -> unit
(** [set_visible_override t visible] explicitly sets visibility and disables
    automatic visibility control until {!reset_visibility_control} is called. *)
