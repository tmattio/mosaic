(** Subscriptions represent sources of external events *)

type window_size = { width : int; height : int }
type 'msg t

val none : 'msg t
(** A subscription that produces no events *)

val keyboard : (Input.key_event -> 'msg) -> 'msg t
(** Subscribe to keyboard events *)

val keyboard_filter : (Input.key_event -> 'msg option) -> 'msg t
(** Subscribe to keyboard events with filtering *)

val mouse : (Input.mouse_event -> 'msg) -> 'msg t
(** Subscribe to mouse events *)

val mouse_filter : (Input.mouse_event -> 'msg option) -> 'msg t
(** Subscribe to mouse events with filtering *)

val window : (window_size -> 'msg) -> 'msg t
(** Subscribe to window resize events *)

val window_filter : (window_size -> 'msg option) -> 'msg t
(** Subscribe to window resize events with filtering *)

val focus : (unit -> 'msg) -> 'msg t
(** Subscribe to terminal focus events *)

val blur : (unit -> 'msg) -> 'msg t
(** Subscribe to terminal blur events *)

(** {2 Convenience functions} *)

val mouse_motion : (int -> int -> 'msg) -> 'msg t
(** Subscribe to mouse motion events *)

val mouse_click : (int -> int -> 'msg) -> 'msg t
(** Subscribe to mouse click events *)

val window_resize : (int -> int -> 'msg) -> 'msg t
(** Subscribe to window resize events with width and height *)

val on_key :
  ?ctrl:bool -> ?alt:bool -> ?shift:bool -> Input.key -> 'msg -> 'msg t
(** Subscribe to a specific key with modifiers *)

val on_char : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> char -> 'msg -> 'msg t
(** Subscribe to a specific character *)

val on_click : (int -> int -> Input.mouse_button -> 'msg) -> 'msg t
(** Just mouse clicks, not motion or release *)

val on_left_click : (int -> int -> 'msg) -> 'msg t
(** [on_left_click f] triggers on left mouse button presses *)

val on_right_click : (int -> int -> 'msg) -> 'msg t
(** [on_right_click f] triggers on right mouse button presses *)

val on_scroll_up : (int -> int -> 'msg) -> 'msg t
(** [on_scroll_up f] triggers on mouse wheel up events *)

val on_scroll_down : (int -> int -> 'msg) -> 'msg t
(** [on_scroll_down f] triggers on mouse wheel down events *)

val on_resize : (int -> int -> 'msg) -> 'msg t
(** Window resize with width and height *)

val on_focus : 'msg -> 'msg t
(** Terminal gained focus *)

val on_blur : 'msg -> 'msg t
(** Terminal lost focus *)

val batch : 'msg t list -> 'msg t
(** Combine multiple subscriptions *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transform messages produced by subscriptions *)

val collect_keyboard :
  (Input.key_event -> 'msg option) list ->
  'msg t ->
  (Input.key_event -> 'msg option) list
(** Internal functions for the runtime *)

val collect_mouse :
  (Input.mouse_event -> 'msg option) list ->
  'msg t ->
  (Input.mouse_event -> 'msg option) list

val collect_window :
  (window_size -> 'msg option) list ->
  'msg t ->
  (window_size -> 'msg option) list

val collect_focus :
  (unit -> 'msg option) list -> 'msg t -> (unit -> 'msg option) list

val collect_blur :
  (unit -> 'msg option) list -> 'msg t -> (unit -> 'msg option) list

val pp :
  (Format.formatter -> 'msg -> unit) -> Format.formatter -> 'msg t -> unit
(** Pretty-printing *)
