(** A clipped viewport rendering an application's accepted {!Scroll.t}. Input
    proposes a new value through [on_scroll]; only [apply_props] or [set_scroll]
    accepts it. Wheel events at a boundary bubble to the enclosing viewport. *)

module Scroll_accel : sig
  type t
  (** The type for scroll gesture acceleration. *)

  val linear : unit -> t
  (** [linear ()] applies no acceleration. *)

  val macos : ?a:float -> ?tau:float -> ?max_multiplier:float -> unit -> t
  (** [macos ()] uses exponential acceleration with defaults [0.8], [3.0]
      seconds, and [6.0]. *)

  val tick : t -> now:float -> float
  (** [tick t ~now] advances the gesture clock in milliseconds. *)

  val reset : t -> unit
  (** [reset t] clears its velocity history. *)
end

module Props : sig
  type t

  val make :
    ?scroll_x:bool ->
    ?scroll_y:bool ->
    ?scroll:Scroll.t ->
    ?background:Ansi.Color.t ->
    ?show_scrollbars:bool ->
    ?scrollbar_props:Scroll_bar.Props.t ->
    ?vertical_bar_props:Scroll_bar.Props.t ->
    ?horizontal_bar_props:Scroll_bar.Props.t ->
    unit ->
    t
  (** [make ()] declares position [Scroll.at 0], vertical scrolling only, and
      visible scrollbars. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] compares declared properties. *)
end

type t

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?scroll:Scroll.t ->
  ?background:Ansi.Color.t ->
  ?scroll_accel:Scroll_accel.t ->
  ?show_scrollbars:bool ->
  ?scrollbar_props:Scroll_bar.Props.t ->
  ?vertical_bar_props:Scroll_bar.Props.t ->
  ?horizontal_bar_props:Scroll_bar.Props.t ->
  ?on_scroll:(Scroll.t -> unit) ->
  unit ->
  t
(** [create ~parent ()] attaches a viewport. User children are routed to its
    content node. Missing [on_scroll] rejects interactive scroll proposals. *)

val node : t -> Renderable.t
val content : t -> Renderable.t
val viewport : t -> Renderable.t
val vertical_bar : t -> Scroll_bar.t
val horizontal_bar : t -> Scroll_bar.t
val scroll_top : t -> int
val scroll_left : t -> int
val scroll_width : t -> int
val scroll_height : t -> int
val viewport_width : t -> int
val viewport_height : t -> int

val set_scroll : t -> Scroll.t -> unit
(** [set_scroll t state] accepts a declaration without emitting [on_scroll]. *)

val set_scroll_top : t -> int -> unit
val set_scroll_left : t -> int -> unit

val scroll_to : t -> ?x:int -> ?y:int -> unit -> unit
(** [scroll_to t ?x ?y ()] proposes the requested absolute position. *)

val scroll_by : t -> ?x:int -> ?y:int -> unit -> unit
(** [scroll_by t ?x ?y ()] proposes movement from the accepted offset. *)

val scroll_by_unit :
  t -> ?x:float -> ?y:float -> unit:Scroll_bar.scroll_unit -> unit -> unit
(** [scroll_by_unit t ?x ?y ~unit ()] proposes movement in the given units. *)

val set_on_scroll : t -> (Scroll.t -> unit) option -> unit
val set_scroll_accel : t -> Scroll_accel.t -> unit
val set_background : t -> Ansi.Color.t option -> unit
val set_show_scrollbars : t -> bool -> unit
val handle_key : t -> Event.key -> bool
val set_style : t -> Toffee.Style.t -> unit

val apply_props : t -> Props.t -> unit
(** [apply_props t props] accepts the declaration and resolves it against the
    viewport; following and reveal remain active after content changes. *)

val pp : Format.formatter -> t -> unit
