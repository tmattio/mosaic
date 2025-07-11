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

(** {3 Common key event shorthands} *)

val on_enter : 'msg -> 'msg t
(** Subscribe to Enter key press *)

val on_escape : 'msg -> 'msg t
(** Subscribe to Escape key press *)

val on_tab : 'msg -> 'msg t
(** Subscribe to Tab key press *)

val on_backspace : 'msg -> 'msg t
(** Subscribe to Backspace key press *)

val on_delete : 'msg -> 'msg t
(** Subscribe to Delete key press *)

val on_up : 'msg -> 'msg t
(** Subscribe to Up arrow key press *)

val on_down : 'msg -> 'msg t
(** Subscribe to Down arrow key press *)

val on_left : 'msg -> 'msg t
(** Subscribe to Left arrow key press *)

val on_right : 'msg -> 'msg t
(** Subscribe to Right arrow key press *)

val on_page_up : 'msg -> 'msg t
(** Subscribe to Page Up key press *)

val on_page_down : 'msg -> 'msg t
(** Subscribe to Page Down key press *)

val on_home : 'msg -> 'msg t
(** Subscribe to Home key press *)

val on_end : 'msg -> 'msg t
(** Subscribe to End key press *)

(** {3 Common ctrl key combinations} *)

val on_ctrl_c : 'msg -> 'msg t
(** Subscribe to Ctrl+C key press *)

val on_ctrl_x : 'msg -> 'msg t
(** Subscribe to Ctrl+X key press *)

val on_ctrl_v : 'msg -> 'msg t
(** Subscribe to Ctrl+V key press *)

val on_ctrl_z : 'msg -> 'msg t
(** Subscribe to Ctrl+Z key press *)

val on_ctrl_a : 'msg -> 'msg t
(** Subscribe to Ctrl+A key press *)

val on_ctrl_s : 'msg -> 'msg t
(** Subscribe to Ctrl+S key press *)

val on_ctrl_d : 'msg -> 'msg t
(** Subscribe to Ctrl+D key press *)

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
