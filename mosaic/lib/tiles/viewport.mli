(** A generic viewport component for displaying and scrolling through content
    that exceeds the visible area.

    This foundational component provides scrolling functionality for large
    content, supporting both vertical and horizontal scrolling. It serves as a
    base for other scrollable components like Text, Table, and Select, ensuring
    consistent scrolling behavior across the framework.

    {2 Architecture}

    State tracks content, scroll position, and viewport dimensions. Renders only
    visible portion. Supports both wrapped text and UI elements.

    {2 Key Invariants}

    - Scroll position is always within valid bounds (0 to content size -
      viewport size)
    - Text content is automatically split into lines for rendering
    - Only visible content is rendered for performance
    - Scroll percentage is normalized between 0.0 and 1.0
    - Page-based scrolling respects viewport height

    {2 Example}

    {[
      (* Initialize a viewport with text content *)
      let viewport_model, viewport_cmd =
        Viewport.init ~content:(Text "Long text content here...") ~width:80
          ~height:10 ~wrap_text:true ()

      (* Scroll operations *)
      let model = Viewport.scroll_down 5 model
      let model = Viewport.page_down model
      let model = Viewport.go_to_bottom model

      (* Check scroll position *)
      let at_bottom = Viewport.at_bottom model
      let x_offset, y_offset = Viewport.scroll_position model
      let h_percent, v_percent = Viewport.scroll_percentage model

      (* Update content *)
      let model =
        Viewport.set_content
          (Elements [ Ui.text "Line 1"; Ui.text "Line 2" ])
          model
    ]} *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the viewport containing content, scroll position,
    dimensions, and configuration. *)

type msg
(** Messages that the viewport can handle for scrolling and content updates. *)

type content =
  | Text of string
  | Elements of Ui.element list
      (** Content types supported by the viewport.

          - [Text s] - Plain text content that will be split into lines
          - [Elements els] - List of UI elements to display *)

val component : (model, msg) Mosaic.app
(** The viewport component definition following The Elm Architecture. *)

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
(** [content model] returns the current viewport content. *)

val scroll_position : model -> int * int
(** [scroll_position model] returns the current scroll position as
    [(x_offset, y_offset)]. *)

val dimensions : model -> int * int
(** [dimensions model] returns the viewport dimensions as [(width, height)]. *)

val at_top : model -> bool
(** [at_top model] returns whether the viewport is scrolled to the top. *)

val at_bottom : model -> bool
(** [at_bottom model] returns whether the viewport is scrolled to the bottom. *)

val at_left : model -> bool
(** [at_left model] returns whether the viewport is at the leftmost position. *)

val at_right : model -> bool
(** [at_right model] returns whether the viewport is at the rightmost position.
*)

val visible_lines : model -> string list
(** [visible_lines model] returns the currently visible lines for text content.
*)

val total_lines : model -> int
(** [total_lines model] returns the total number of lines in the content. *)

val scroll_percentage : model -> float * float
(** [scroll_percentage model] returns the scroll percentage as
    [(horizontal, vertical)] where each value is between 0.0 and 1.0. *)

(** {2 Actions} *)

val set_content : content -> model -> model
(** [set_content content model] updates the viewport content. *)

val set_dimensions : int -> int -> model -> model
(** [set_dimensions width height model] sets the viewport dimensions. *)

val scroll_to : int -> int -> model -> model
(** [scroll_to x y model] scrolls to the specific position [(x, y)]. *)

val scroll_up : int -> model -> model
(** [scroll_up n model] scrolls up by [n] lines. *)

val scroll_down : int -> model -> model
(** [scroll_down n model] scrolls down by [n] lines. *)

val scroll_left : int -> model -> model
(** [scroll_left n model] scrolls left by [n] columns. *)

val scroll_right : int -> model -> model
(** [scroll_right n model] scrolls right by [n] columns. *)

val page_up : model -> model
(** [page_up model] scrolls up by one full viewport height. *)

val page_down : model -> model
(** [page_down model] scrolls down by one full viewport height. *)

val half_page_up : model -> model
(** [half_page_up model] scrolls up by half the viewport height. *)

val half_page_down : model -> model
(** [half_page_down model] scrolls down by half the viewport height. *)

val go_to_top : model -> model
(** [go_to_top model] scrolls to the top of the content. *)

val go_to_bottom : model -> model
(** [go_to_bottom model] scrolls to the bottom of the content. *)

val go_to_left : model -> model
(** [go_to_left model] scrolls to the leftmost position. *)

val go_to_right : model -> model
(** [go_to_right model] scrolls to the rightmost position. *)

val scroll_to_left : model -> model * msg Cmd.t
(** [scroll_to_left model] scrolls to the leftmost position, returning a message
    for component updates. *)

val scroll_to_right : model -> model * msg Cmd.t
(** [scroll_to_right model] scrolls to the rightmost position, returning a
    message for component updates. *)

(** {2 Configuration} *)

val set_wrap_text : bool -> model -> model
(** [set_wrap_text enabled model] enables or disables text wrapping for text
    content. *)

val set_horizontal_scroll : bool -> model -> model
(** [set_horizontal_scroll enabled model] enables or disables horizontal
    scrolling. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the viewport state. *)

val view : model -> Ui.element
(** [view model] renders the viewport with visible content. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns subscriptions for keyboard and mouse events
    for scrolling. *)
