(** Event subscription helpers for Tile components

    This module provides hooks for subscribing to mouse and keyboard events for
    specific UI elements identified by keys. *)

(** {1 Event Types} *)

type key_event = Input.key_event
(** Keyboard event type *)

type drag_phase = [ `Start | `Move | `End ]
(** Drag event phase *)

type drag = {
  phase : drag_phase;
  x : int;
  y : int;
  dx : int;
  dy : int;
  start_x : int;
  start_y : int;
}
(** Drag event with position and delta information *)

(** {1 Hook-based Event Subscriptions} *)

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

val use_drag : Ui.Attr.key -> (drag -> unit) -> unit
(** Subscribe to drag events on an element. The callback receives drag events
    with position and phase information. *)

val use_focus : Ui.Attr.key -> bool
(** Subscribe to focus events on an element. The callback is invoked with [true]
    when the element gains focus and [false] when it loses focus.

    Returns the current focus state. *)

val use_key_press : Ui.Attr.key -> (key_event -> unit) -> unit
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

val use_keyboard :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  Input.key ->
  unit Engine.Cmd.t ->
  unit
(** Global-ish shortcut helper that dispatches a command when a chord is hit.
    Intended for app-level bindings; for element-local keys use [on_key]. *)

(** {1 Direct Event Subscriptions}

    These are convenience functions for direct event subscription that return
    unit, making them more composable in UI building. *)

val on_click : Ui.Attr.key -> (unit -> unit) -> unit
(** Direct subscription to click events *)

val on_hover : Ui.Attr.key -> (bool -> unit) -> unit
(** Direct subscription to hover events *)

val on_drag : Ui.Attr.key -> (drag -> unit) -> unit
(** Direct subscription to drag events *)

val on_focus : Ui.Attr.key -> (bool -> unit) -> unit
(** Direct subscription to focus events *)

val on_key : Ui.Attr.key -> (key_event -> unit) -> unit
(** Direct subscription to keyboard events *)

val focusable :
  Ui.Attr.key -> ?tab_index:int -> ?auto_focus:bool -> unit -> unit
(** Alias for use_focusable for backward compatibility *)
