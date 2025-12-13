(** Text selection with anchor and focus points.

    Points are normalized so anchor precedes focus in reading order
    (top-to-bottom, left-to-right). Supports dynamic anchor positioning via
    callback for scrollable content. *)

(** {1 Types} *)

type point = { x : int; y : int }
(** 2D coordinate (0-based column and row). *)

type local_bounds = {
  anchor_x : int;
  anchor_y : int;
  focus_x : int;
  focus_y : int;
  is_active : bool;
}
(** Selection bounds in local coordinates after transformation. *)

type bounds = { x : int; y : int; width : int; height : int }
(** Bounding rectangle of a selection. *)

type t
(** Mutable text selection. *)

(** {1 Construction} *)

val create :
  ?anchor_provider:(unit -> point) ->
  anchor:point ->
  focus:point ->
  get_selected_text:(unit -> string) ->
  unit ->
  t
(** [create ?anchor_provider ~anchor ~focus ~get_selected_text ()] creates a
    selection with [is_active] and [is_selecting] set to [true].

    If [anchor_provider] is given, anchor is recomputed on each access (useful
    for selections in scrollable content). *)

(** {1 Position} *)

val anchor : t -> point
(** [anchor t] returns the normalized anchor (always before focus). *)

val focus : t -> point
(** [focus t] returns the normalized focus (always after anchor). *)

val set_focus : t -> point -> unit
(** [set_focus t p] updates the focus and re-normalizes. *)

val set_anchor : t -> point -> unit
(** [set_anchor t p] updates the anchor and re-normalizes. *)

val bounds : t -> bounds
(** [bounds t] returns the bounding rectangle. *)

(** {1 State} *)

val is_active : t -> bool
(** [is_active t] returns whether the selection should be rendered. *)

val set_is_active : t -> bool -> unit
(** [set_is_active t v] shows or hides the selection. *)

val is_selecting : t -> bool
(** [is_selecting t] returns whether user is actively selecting. *)

val set_is_selecting : t -> bool -> unit
(** [set_is_selecting t v] updates the selecting state. *)

(** {1 Text} *)

val get_selected_text : t -> string
(** [get_selected_text t] returns the selected text via the callback. *)

(** {1 Coordinate Transformation} *)

val convert_global_to_local :
  t option -> local_origin_x:int -> local_origin_y:int -> local_bounds option
(** [convert_global_to_local t ~local_origin_x ~local_origin_y] transforms
    global coordinates to local. Returns [None] if [t] is [None] or inactive. *)
