(** Scrollable container with viewport clipping, sticky scrolling, and owned
    scroll bars.

    Scroll_box uses a three-node structure: a root container, an internal
    viewport that owns clipping via [overflow = Hidden], and a content node that
    is translated at render time to implement scrolling without triggering
    relayout. It supports horizontal/vertical scrolling, keyboard/mouse
    navigation, and optional sticky behavior. Horizontal and vertical scroll
    bars are created as children (wrapper/viewport siblings) and bound
    automatically; external bars can still be wired via {!bind_scroll_bars} when
    needed.

    Children are attached to an internal content node rather than the root.
    External code interacts with the root via {!node}, while the reconciler and
    low-level wiring use {!content} as the parent for user children.

    Viewport culling can be enabled to only iterate and render children that
    overlap the visible area, improving performance for long lists. *)

module Props : sig
  type t

  val make :
    ?background:Ansi.Color.t ->
    ?scroll_x:bool ->
    ?scroll_y:bool ->
    ?scroll_acceleration:[ `Linear | `MacOS ] ->
    ?sticky_scroll:bool ->
    ?sticky_start:[ `Top | `Bottom | `Left | `Right ] ->
    ?viewport_culling:bool ->
    ?autofocus:bool ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

type t

val apply_props : t -> Props.t -> unit
(** [apply_props box props] applies [props] to a mounted scroll box using its
    setters for runtime-adjustable fields such as background, sticky scroll,
    scroll acceleration, and viewport culling. Creation-time flags like
    [scroll_x] and [scroll_y] remain unchanged. *)

val mount : ?props:Props.t -> ?renderer:Renderer.t -> Renderable.t -> t
(** [mount ?props ?renderer node] configures [node] to behave as a scroll
    container. [renderer], when provided, is used to request selection updates
    during auto-scroll gestures. *)

val node : t -> Renderable.t
(** [node t] returns the root scrollable container node. *)

val wrapper : t -> Renderable.t
(** [wrapper t] returns the internal wrapper node that stacks viewport and
    horizontal scroll bar. *)

val content : t -> Renderable.t
(** [content t] returns the internal content node that holds user children. *)

val horizontal_bar : t -> Scroll_bar.t option
(** [horizontal_bar t] returns the internally managed horizontal scroll bar, if
    created. *)

val vertical_bar : t -> Scroll_bar.t option
(** [vertical_bar t] returns the internally managed vertical scroll bar, if
    created. *)

val scroll_to : ?x:int -> ?y:int -> ?manual:bool -> t -> unit
(** [scroll_to t ~x ~y ()] sets absolute scroll position in cells.

    Position is clamped to scroll limits. If [manual] is [true], disables sticky
    scroll behavior.

    @param manual Marks scroll as user-initiated. Default is [true]. *)

val scroll_by : ?x:int -> ?y:int -> ?manual:bool -> t -> unit
(** [scroll_by t ~x ~y ()] adjusts scroll position by relative offset.

    @param manual Marks scroll as user-initiated. Default is [true]. *)

val scroll_position : t -> int * int
(** [scroll_position t] returns current [(scroll_x, scroll_y)] position. *)

val scroll_limits : t -> int * int
(** [scroll_limits t] returns maximum scroll [(max_x, max_y)] based on content
    and viewport sizes. *)

val scroll_width : t -> int
(** [scroll_width t] returns content width in cells. *)

val scroll_height : t -> int
(** [scroll_height t] returns content height in cells. *)

val viewport_size : t -> int * int
(** [viewport_size t] returns [(width, height)] of visible area in cells. *)

val sticky_scroll_enabled : t -> bool
(** [sticky_scroll_enabled t] returns current sticky scroll state. *)

val set_sticky_scroll : t -> bool -> unit
(** [set_sticky_scroll t flag] enables or disables sticky scrolling and updates
    sticky edge state. *)

val set_sticky_start : t -> [ `Top | `Bottom | `Left | `Right ] option -> unit
(** [set_sticky_start t edge] changes the sticky edge. Reapplies sticky position
    if enabled. *)

val reset_sticky : t -> unit
(** [reset_sticky t] clears manual scroll flag and reapplies sticky positioning.
*)

val sticky_start : t -> [ `Top | `Bottom | `Left | `Right ] option
(** [sticky_start t] returns the configured sticky edge, if any. *)

val has_manual_scroll : t -> bool
(** [has_manual_scroll t] returns whether the user has scrolled manually, which
    disables automatic sticky adjustments until reset. *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] delegates keyboard navigation to bound scroll bars,
    returning [true] when one consumes the event. *)

val on_scroll : t -> (x:int -> y:int -> unit) -> unit
(** [on_scroll t handler] registers a callback invoked when scroll position
    changes. *)

val set_on_scroll : t -> (x:int -> y:int -> unit) option -> unit
(** [set_on_scroll t cb] replaces the scroll listeners with [cb] if provided, or
    clears listeners when [None]. *)

val set_background : t -> Ansi.Color.t option -> unit
(** [set_background t color] updates the container background color. *)

val append_child : t -> Renderable.t -> (unit, Renderable.error) result
(** [append_child t child] adds [child] to the end of the content container. *)

val insert_child :
  t -> index:int -> Renderable.t -> (unit, Renderable.error) result
(** [insert_child t ~index child] inserts [child] at position [index] in the
    content container. *)

val remove_child : t -> Renderable.t -> (unit, Renderable.error) result
(** [remove_child t child] removes [child] from the content container. *)

val bind_scroll_bars :
  t -> ?horizontal:Scroll_bar.t -> ?vertical:Scroll_bar.t -> unit -> unit
(** [bind_scroll_bars t ?horizontal ?vertical ()] wires external scroll bars to
    the scroll box. Bars receive size updates and drive scroll when changed, in
    addition to the internally managed scroll bars. *)

val set_scroll_acceleration : t -> [ `Linear | `MacOS ] -> unit
(** Set the scroll acceleration mode at runtime. *)

val set_viewport_culling : t -> bool -> unit
(** Enable/disable viewport culling at runtime. *)

val scroll_by_units :
  ?x:float * [ `Absolute | `Viewport | `Content | `Step ] ->
  ?y:float * [ `Absolute | `Viewport | `Content | `Step ] ->
  ?manual:bool ->
  t ->
  unit
(** Adjust scroll position by a delta expressed in logical units.

    - [`Absolute] multiplies by 1 cell
    - [`Viewport] multiplies by the current viewport size on the axis
    - [`Content] multiplies by the total content size on the axis
    - [`Step] multiplies by the optional bound scroll bar's step or 1 if unset

    Defaults to [`Absolute] when not specified. *)

val set_scroll_acceleration_params :
  t -> a:float -> tau:float -> max_multiplier:float -> unit
(** Tune macOS-like scroll acceleration parameters. No-op for `Linear`. *)
