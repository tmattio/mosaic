(** A multiline text area component

    This component provides a multiline text editor with support for scrolling,
    word wrapping, and syntax highlighting. Similar to huh's Text field.

    Example:
    {[
      (* Initialize a text area *)
      let text_model, text_cmd =
        Text.init
          ~title:"Enter description"
          ~placeholder:"Type here..."
          ~height:10
          ()

      (* In your update function *)
      | TextMsg msg ->
          let new_text, cmd = Text.update msg model.text in
          ({ model with text = new_text },
           Cmd.map (fun m -> TextMsg m) cmd)

      (* In your view *)
      Text.view model.text
    ]} *)

open Mosaic

type model
(** The internal state of the text area *)

type msg
(** Messages the text area can handle *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init :
  ?placeholder:string ->
  ?initial_value:string ->
  ?height:int ->
  ?width:int ->
  ?word_wrap:bool ->
  ?validate:(string -> (unit, string) result) ->
  unit ->
  model * msg Cmd.t
(** [init ?placeholder ?initial_value ?height ?width ?word_wrap ?validate ()]
    creates a new text area.

    @param placeholder Text shown when empty
    @param initial_value Initial text content
    @param height Number of visible lines (default: 5)
    @param width Fixed width for the text area (default: 60)
    @param word_wrap Enable word wrapping (default: true)
    @param validate Validation function *)

(** {2 Accessors} *)

val value : model -> string
(** Get the current text value *)

val lines : model -> string list
(** Get the text split into lines *)

val line_count : model -> int
(** Get the total number of lines *)

val cursor_position : model -> int * int
(** Get cursor position as (line, column) *)

val is_focused : model -> bool
(** Check if the text area is currently focused *)

val is_valid : model -> bool
(** Check if the current value passes validation *)

val error : model -> string option
(** Get the current validation error message *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** Focus the text area *)

val blur : model -> model * msg Cmd.t
(** Remove focus from the text area *)

val set_value : string -> model -> model
(** Set the text area's value programmatically *)

val clear : model -> model
(** Clear the text area's value *)

val insert_at_cursor : string -> model -> model
(** Insert text at the current cursor position *)

val go_to_line : int -> model -> model
(** Move cursor to a specific line (1-based) *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  error_style : Style.t;
  placeholder_style : Style.t;
  line_numbers_style : Style.t;
  cursor_style : Style.t;
}

val default_theme : theme
val with_theme : theme -> model -> model

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Update function for the component *)

val view : model -> Ui.element
(** View function for the component *)

val subscriptions : model -> msg Sub.t
(** Subscriptions for the component *)
