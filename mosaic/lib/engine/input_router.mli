type t

type mouse_event =
  | Button_down of Input.mouse_button
  | Button_up of Input.mouse_button
  | Motion

type phase = [ `Start | `Move | `End ]

type drag_event = {
  x : int;
  y : int;
  phase : phase;
  start_x : int;
  start_y : int;
  dx : int;
  dy : int;
}

type handler =
  | Click of Ui.Attr.key * (unit -> bool)
  | Hover of Ui.Attr.key * (bool -> unit)
  | Drag of Ui.Attr.key * (drag_event -> unit)
  | Focus of Ui.Attr.key * (bool -> unit)
  | Key_press of Ui.Attr.key * (Input.key_event -> bool)
      (** Key press handler that returns [true] if the event was consumed and
          should not propagate further *)
  | Scroll of Ui.Attr.key * (int -> unit)
(* delta: positive for down, negative for up *)

type subscription_id = int

val create : unit -> t
val subscribe : t -> handler -> subscription_id
val unsubscribe : t -> subscription_id -> unit
val set_snapshot : t -> Ui.Layout_snapshot.t -> unit
val set_focused : t -> Ui.Attr.key option -> unit
val on_mouse : t -> x:int -> y:int -> mouse_event -> bool

val on_keyboard : t -> Input.key_event -> bool
(** [on_keyboard t event] dispatches a keyboard event to the focused element's
    handlers. Returns [true] if any handler consumed the event (indicated the
    event should not propagate further). *)

val get_hovered : t -> Ui.Attr.key option
val get_dragging : t -> Ui.Attr.key option
val get_focused : t -> Ui.Attr.key option
val reset : t -> unit
val pp_phase : Format.formatter -> phase -> unit
val pp_drag_event : Format.formatter -> drag_event -> unit
val pp_mouse_event : Format.formatter -> mouse_event -> unit
