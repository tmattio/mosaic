(** A generic viewport component for displaying scrollable content.

    This is a foundational component that provides scrolling functionality for
    large content that doesn't fit in the visible area. It can be used by other
    components like Text, Table, and Select for consistent scrolling behavior.
*)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the viewport *)

type msg
(** Messages the viewport can handle *)

type content =
  | Text of string
  | Elements of Ui.element list
      (** Content can be either plain text or a list of UI elements *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init :
  ?content:content ->
  ?width:int ->
  ?height:int ->
  ?wrap_text:bool ->
  ?horizontal_scroll:bool ->
  unit ->
  model * msg Cmd.t
(** [init ?content ?width ?height ?wrap_text ?horizontal_scroll ()] creates a
    new viewport.

    @param content Initial content to display (default: empty)
    @param width Viewport width in columns (default: 80)
    @param height Viewport height in rows (default: 10)
    @param wrap_text Enable text wrapping for Text content (default: true)
    @param horizontal_scroll Enable horizontal scrolling (default: false) *)

(** {2 Accessors} *)

val content : model -> content
(** Get the current content *)

val scroll_position : model -> int * int
(** Get the current scroll position (x_offset, y_offset) *)

val dimensions : model -> int * int
(** Get the viewport dimensions (width, height) *)

val at_top : model -> bool
(** Check if viewport is at the top *)

val at_bottom : model -> bool
(** Check if viewport is at the bottom *)

val at_left : model -> bool
(** Check if viewport is at the leftmost position *)

val at_right : model -> bool
(** Check if viewport is at the rightmost position *)

val visible_lines : model -> string list
(** Get the currently visible lines (for text content) *)

val total_lines : model -> int
(** Get the total number of lines in the content *)

val scroll_percentage : model -> float * float
(** Get scroll percentage (horizontal, vertical) from 0.0 to 1.0 *)

(** {2 Actions} *)

val set_content : content -> model -> model
(** Update the viewport content *)

val set_dimensions : int -> int -> model -> model
(** Set viewport width and height *)

val scroll_to : int -> int -> model -> model
(** Scroll to specific position (x, y) *)

val scroll_up : int -> model -> model
(** Scroll up by the given number of lines *)

val scroll_down : int -> model -> model
(** Scroll down by the given number of lines *)

val scroll_left : int -> model -> model
(** Scroll left by the given number of columns *)

val scroll_right : int -> model -> model
(** Scroll right by the given number of columns *)

val page_up : model -> model
(** Scroll up by one page (viewport height) *)

val page_down : model -> model
(** Scroll down by one page (viewport height) *)

val half_page_up : model -> model
(** Scroll up by half a page *)

val half_page_down : model -> model
(** Scroll down by half a page *)

val go_to_top : model -> model
(** Scroll to the top of the content *)

val go_to_bottom : model -> model
(** Scroll to the bottom of the content *)

val go_to_left : model -> model
(** Scroll to the leftmost position *)

val go_to_right : model -> model
(** Scroll to the rightmost position *)

val scroll_to_left : model -> model * msg Cmd.t
(** Scroll to the leftmost position with message *)

val scroll_to_right : model -> model * msg Cmd.t
(** Scroll to the rightmost position with message *)

(** {2 Configuration} *)

val set_wrap_text : bool -> model -> model
(** Enable or disable text wrapping *)

val set_horizontal_scroll : bool -> model -> model
(** Enable or disable horizontal scrolling *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Handle messages and update the viewport state *)

val view : model -> Ui.element
(** Render the viewport *)

val subscriptions : model -> msg Sub.t
(** Subscribe to keyboard/mouse events for scrolling *)
