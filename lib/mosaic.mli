(** Mosaic - A delightful OCaml TUI framework inspired by The Elm Architecture
*)

(** {1 Styling} *)

module Style : sig
  (** Terminal styling (colors and text attributes) *)

  type t
  (** A style is a collection of attributes *)

  type color = Ansi.color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white
    | Index of int  (** 256-color palette (0-255) *)
    | RGB of int * int * int  (** 24-bit color (0-255 each) *)

  (** {2 Style Attributes} *)

  (** Style attributes for building styles from lists *)
  type attr =
    | Fg of color
    | Bg of color
    | Bold
    | Dim
    | Italic
    | Underline
    | Blink
    | Reverse
    | Strikethrough
    | Link of string

  (** {2 Creating Styles} *)

  val empty : t
  val fg : color -> t
  val bg : color -> t
  val bold : t
  val dim : t
  val italic : t
  val underline : t
  val blink : t
  val reverse : t
  val strikethrough : t
  val link : string -> t

  val of_list : attr list -> t
  (** Create a style from a list of attributes *)

  val ( ++ ) : t -> t -> t
  (** Combine styles. Right side takes precedence. *)

  (** {2 Color Helpers} *)

  val gray : int -> color
  (** [gray n] creates a grayscale color (0-23) *)

  val rgb_hex : int -> color
  (** [rgb_hex 0xFF00FF] creates an RGB color from hex *)
end

(** {1 Building User Interfaces} *)

module Ui : sig
  (** Build terminal user interfaces with automatic layout *)

  type element
  (** A UI element: text, boxes, or spacers *)

  (** {2 Basic Elements} *)

  val text : ?style:Style.t -> string -> element
  (** Text with optional styling. Truncated if too wide. *)

  val space : int -> element
  (** Empty space. In hbox: width. In vbox: height. *)

  (** {2 Layout} *)

  type padding = { top : int; right : int; bottom : int; left : int }

  val pad :
    ?all:int ->
    ?x:int ->
    ?y:int ->
    ?top:int ->
    ?right:int ->
    ?bottom:int ->
    ?left:int ->
    unit ->
    padding
  (** Create padding. [all] sets all sides. [x]/[y] set horizontal/vertical.
      Individual sides override [all]/[x]/[y]. All default to 0. *)

  val padding_all : int -> padding
  (** [padding_all n] creates uniform padding on all sides *)

  val padding_xy : int -> int -> padding
  (** [padding_xy x y] creates padding with x for left/right, y for top/bottom
  *)

  type border_style = Solid | Rounded | Double | Thick | ASCII
  type border = { style : border_style; color : Style.color option }

  val border : ?style:border_style -> ?color:Style.color -> unit -> border
  (** Create a border. Default style is [Solid]. *)

  type align =
    | Start
    | Center
    | End
    | Stretch  (** Alignment/justification within containers *)

  val hbox :
    ?gap:int ->
    ?width:int ->
    ?height:int ->
    ?padding:padding ->
    ?border:border ->
    ?align_items:align ->
    (* How items are aligned on the cross-axis (vertically) *)
    ?justify_content:align ->
    (* How items are distributed on the main-axis (horizontally) *)
    element list ->
    element
  (** Horizontal box. Children laid out left-to-right. *)

  val vbox :
    ?gap:int ->
    ?width:int ->
    ?height:int ->
    ?padding:padding ->
    ?border:border ->
    ?align_items:align ->
    (* How items are aligned on the cross-axis (horizontally) *)
    ?justify_content:align ->
    (* How items are distributed on the main-axis (vertically) *)
    element list ->
    element
  (** Vertical box. Children laid out top-to-bottom. *)

  val expand : element -> element
  (** Make element expand to fill available space *)

  val render : Render.buffer -> element -> unit
  (** Render an element to a Render buffer *)
end

(** {1 Commands (Effects)} *)

module Cmd : sig
  (** Commands represent effects like async I/O, timers, or quitting *)

  type 'msg exec_cmd = { run : unit -> unit; on_complete : 'msg }
  
  type 'msg t =
    | None
    | Msg of 'msg
    | Batch of 'msg t list
    | Perform of (unit -> 'msg option)
    | Exec of 'msg exec_cmd
    | Tick of float * (float -> 'msg)
    | Sequence of 'msg t list
    | Quit
    | Log of string
    | SetWindowTitle of string

  val none : 'msg t
  val msg : 'msg -> 'msg t
  val quit : 'msg t

  val batch : 'msg t list -> 'msg t
  (** Run multiple commands in parallel *)

  val seq : 'msg t list -> 'msg t
  (** Run commands in sequence *)

  val perform : (unit -> 'msg option) -> 'msg t
  (** Run async function in a fiber *)

  val exec : (unit -> unit) -> 'msg -> 'msg t
  (** Run external command with full terminal control *)

  val release_and_run : (unit -> unit) -> 'msg -> 'msg t
  (** [release_and_run f msg] temporarily releases the terminal, executes
      function f, restores terminal, and produces msg when complete. Useful for
      running external editors, pagers, etc. This is an alias for [exec]. *)

  val after : float -> 'msg -> 'msg t
  (** Send a message after delay (seconds) *)

  val tick : float -> (float -> 'msg) -> 'msg t
  (** Call function with actual elapsed time after delay *)

  val log : string -> 'msg t
  (** Debug print to stderr *)

  val set_window_title : string -> 'msg t
  (** Set the terminal window title *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map a function over the message type in a command *)
end

(** {1 Input Events} *)

(** These types are used by subscriptions and for pattern matching *)

type key =
  | Char of Uchar.t
  | Enter
  | Tab
  | Backspace
  | Delete
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Insert
  | F of int  (** F1-F20 *)

type modifier = { ctrl : bool; alt : bool; shift : bool }
type key_event = { key : key; modifier : modifier }

type mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Button of int  (** Extended buttons 4-11 *)

type mouse_event =
  | Press of int * int * mouse_button * modifier
  | Release of int * int * mouse_button * modifier
  | Motion of int * int * Input.mouse_button_state * modifier

(** {1 Subscriptions (Event Listeners)} *)

module Sub : sig
  (** Subscribe to external events like keyboard, mouse, or window resize *)

  type 'msg t

  val none : 'msg t
  val batch : 'msg t list -> 'msg t

  (** {2 Keyboard} *)

  val keyboard : (key_event -> 'msg) -> 'msg t
  val keyboard_filter : (key_event -> 'msg option) -> 'msg t

  val on_key : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> key -> 'msg -> 'msg t
  (** Subscribe to a specific key with modifiers *)

  val on_char : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> char -> 'msg -> 'msg t
  (** Subscribe to a specific character *)

  (** {2 Mouse} *)

  val mouse : (mouse_event -> 'msg) -> 'msg t
  val mouse_filter : (mouse_event -> 'msg option) -> 'msg t

  val on_click : (int -> int -> mouse_button -> 'msg) -> 'msg t
  (** Just mouse clicks, not motion or release *)

  val on_left_click : (int -> int -> 'msg) -> 'msg t
  (** [on_left_click f] triggers on left mouse button presses *)

  val on_right_click : (int -> int -> 'msg) -> 'msg t
  (** [on_right_click f] triggers on right mouse button presses *)

  val on_scroll_up : (int -> int -> 'msg) -> 'msg t
  (** [on_scroll_up f] triggers on mouse wheel up events *)

  val on_scroll_down : (int -> int -> 'msg) -> 'msg t
  (** [on_scroll_down f] triggers on mouse wheel down events *)

  (** {2 Window} *)

  val on_resize : (int -> int -> 'msg) -> 'msg t
  (** Window resize with width and height *)

  val on_focus : 'msg -> 'msg t
  val on_blur : 'msg -> 'msg t
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** {2 Internal Functions} *)

  type window_size = { width : int; height : int }

  val collect_keyboard :
    (key_event -> 'msg option) list -> 'msg t -> (key_event -> 'msg option) list

  val collect_mouse :
    (mouse_event -> 'msg option) list ->
    'msg t ->
    (mouse_event -> 'msg option) list

  val collect_window :
    (window_size -> 'msg option) list ->
    'msg t ->
    (window_size -> 'msg option) list

  val collect_focus :
    (unit -> 'msg option) list -> 'msg t -> (unit -> 'msg option) list

  val collect_blur :
    (unit -> 'msg option) list -> 'msg t -> (unit -> 'msg option) list
end

val key : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> key -> key_event
(** Build a key event for pattern matching *)

val char : ?ctrl:bool -> ?alt:bool -> ?shift:bool -> char -> key_event
(** Build a key event from a char *)

(** {1 Testing Support} *)

module Input = Input
module Event_source = Event_source
module Terminal = Terminal

(** {1 Building Applications} *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> Ui.element;
  subscriptions : 'model -> 'msg Sub.t;
}
(** The Elm Architecture: Model-View-Update with effects and subscriptions *)

val app :
  init:(unit -> 'model * 'msg Cmd.t) ->
  update:('msg -> 'model -> 'model * 'msg Cmd.t) ->
  view:('model -> Ui.element) ->
  ?subscriptions:('model -> 'msg Sub.t) ->
  unit ->
  ('model, 'msg) app
(** Create an application. [subscriptions] defaults to [Sub.none]. *)

val run :
  ?terminal:Terminal.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ('model, 'msg) app ->
  unit
(** Run an application.

    @param terminal Custom terminal for testing (default: stdin/stdout)
    @param alt_screen Use alternate screen buffer (default: true)
    @param mouse Enable mouse support (default: false)
    @param fps Target frames per second (default: 60)

    Note: Requires [Eio_main.run] context. *)

val run_eio :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?terminal:Terminal.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ('model, 'msg) app ->
  unit
(** Run with explicit Eio environment. Most users should use [run] instead. *)
