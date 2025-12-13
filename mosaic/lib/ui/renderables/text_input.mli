(** Interactive single-line text input (code unit-based editing).

    Text_input provides a focusable single-line text field with cursor
    navigation, placeholder text, and event callbacks for input, change, and
    submission. Keyboard input is handled automatically with printable ASCII
    keystrokes; programmatic updates can include any string. Mouse interactions
    are not handled.

    {1 Overview}

    Text_input uses a horizontal scrolling viewport to display text that exceeds
    available width. The cursor is kept visible within the viewport. Text
    editing operates on UTF-8 code units.

    {1 Event Sequence}

    - [on_input]: Fired on every keystroke
    - [on_change]: Fired when focus is lost or Enter is pressed
    - [on_submit]: Fired when Enter is pressed *)

type cursor_style = [ `Block | `Line | `Underline ]

module Props : sig
  type t

  val make :
    ?background:Ansi.Color.t ->
    ?text_color:Ansi.Color.t ->
    ?focused_background:Ansi.Color.t ->
    ?focused_text_color:Ansi.Color.t ->
    ?placeholder:string ->
    ?placeholder_color:Ansi.Color.t ->
    ?cursor_color:Ansi.Color.t ->
    ?cursor_style:cursor_style ->
    ?cursor_blinking:bool ->
    ?max_length:int ->
    ?value:string ->
    ?autofocus:bool ->
    unit ->
    t

  val default : t
  val equal : t -> t -> bool
end

type t

val apply_props : t -> Props.t -> unit
(** [apply_props input props] applies [props] to a mounted text input using its
    setters, updating colors, placeholder, cursor appearance, length limits, and
    value. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render an interactive text input.
*)

(** {1 Accessors} *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable node. *)

val value : t -> string
(** [value t] returns the current text content. *)

val cursor : t -> int
(** [cursor t] returns the cursor position as a code-unit index. *)

val hardware_cursor :
  t -> (int * int * Ansi.Color.t * cursor_style * bool) option
(** [hardware_cursor t] returns [(x, y, color, style, blinking)] for the
    terminal cursor in 1-based coordinates if the input is focused and visible;
    [None] otherwise. Intended for renderer integration. *)

(** {1 Mutations} *)

val set_value : t -> string -> unit
(** [set_value t text] replaces content and preserves the cursor position
    (clamped to bounds). Fires [on_input]. *)

val set_placeholder : t -> string -> unit
(** [set_placeholder t text] updates the placeholder text. *)

val set_cursor : t -> int -> unit
(** [set_cursor t index] moves the cursor to code-unit [index]. Clamped to valid
    range. *)

val set_max_length : t -> int -> unit
(** [set_max_length t length] updates maximum code-unit count. Truncates current
    text if needed. *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t color] updates unfocused background color. *)

val set_focused_background : t -> Ansi.Color.t -> unit
(** [set_focused_background t color] updates focused background color. *)

val set_text_color : t -> Ansi.Color.t -> unit
(** [set_text_color t color] updates unfocused text color. *)

val set_focused_text_color : t -> Ansi.Color.t -> unit
(** [set_focused_text_color t color] updates focused text color. *)

val set_placeholder_color : t -> Ansi.Color.t -> unit
(** [set_placeholder_color t color] updates placeholder text color. *)

val set_cursor_color : t -> Ansi.Color.t -> unit
(** [set_cursor_color t color] updates cursor color. *)

val set_cursor_style : t -> cursor_style -> unit
(** [set_cursor_style t style] updates the cursor style. *)

val set_cursor_blinking : t -> bool -> unit
(** [set_cursor_blinking t blinking] updates whether the cursor blinks. *)

(** {1 Event handlers} *)

val set_callbacks :
  t ->
  ?on_input:(string -> unit) ->
  ?on_change:(string -> unit) ->
  ?on_submit:(string -> unit) ->
  unit ->
  unit
(** [set_callbacks t ~on_input ~on_change ~on_submit ()] replaces registered
    callbacks. *)

val on_input : t -> (string -> unit) -> unit
(** [on_input t handler] registers a callback fired on every text modification.
*)

val on_change : t -> (string -> unit) -> unit
(** [on_change t handler] registers a callback fired when focus is lost or Enter
    is pressed, but only if text changed since last commit. *)

val on_submit : t -> (string -> unit) -> unit
(** [on_submit t handler] registers a callback fired when Enter is pressed. *)

(** {1 Event routing} *)

val focus : t -> bool
(** [focus t] attempts to focus the input field. Returns [true] if successful.
*)

val blur : t -> unit
(** [blur t] removes focus from input field. Fires [on_change] if text changed.
*)
