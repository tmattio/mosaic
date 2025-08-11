(** Event subscription helpers for Tile components

    This module provides hooks for subscribing to mouse and keyboard events for
    specific UI elements identified by keys. *)

val use_click : Ui.Attr.key -> (unit -> unit) -> unit
(** Subscribe to click events on an element. The callback is invoked when the
    element is clicked.

    Example:
    {[
      let button ~on_click () =
        let key = Tile.use_key ~prefix:"btn" in
        Tile_events.use_click key on_click;
        Ui.with_key key (...)
    ]} *)

val use_hover : Ui.Attr.key -> bool
(** Subscribe to hover events on an element. The callback is invoked with [true]
    when the mouse enters and [false] when it leaves.

    Returns the current hover state. *)

val use_drag : Ui.Attr.key -> (Engine.Input_router.drag_event -> unit) -> unit
(** Subscribe to drag events on an element. The callback receives drag events
    with position and phase information. *)

val use_focus : Ui.Attr.key -> bool
(** Subscribe to focus events on an element. The callback is invoked with [true]
    when the element gains focus and [false] when it loses focus.

    Returns the current focus state. *)

val use_key_press : Ui.Attr.key -> (Input.key_event -> unit) -> unit
(** Subscribe to keyboard events when the element has focus. The callback
    receives keyboard events when the element is focused. *)

val use_bounds : Ui.Attr.key -> Ui.Layout_snapshot.rect option
(** Get the current bounds of an element from the layout snapshot. Returns
    [None] if the element hasn't been rendered yet. *)

val use_focusable :
  Ui.Attr.key -> ?tab_index:int -> ?auto_focus:bool -> unit -> unit
(** Register an element as focusable with optional tab index. Elements with
    lower tab indices are focused before those with higher indices. Elements
    without tab indices follow document order. *)
