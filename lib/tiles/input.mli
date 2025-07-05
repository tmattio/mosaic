(** A single-line text input component with advanced features

    This component provides a text input field with support for validation,
    suggestions, placeholders, and password mode. It's similar to the huh
    library's Input field.

    Example:
    {[
      (* Initialize an input with validation *)
      let input_model, input_cmd =
        Input.init
          ~placeholder:"Enter email"
          ~validate:(fun s ->
            if String.contains s '@' then Ok ()
            else Error "Must be a valid email")
          ()

      (* In your update function *)
      | InputMsg msg ->
          let new_input, cmd = Input.update msg model.input in
          ({ model with input = new_input },
           Cmd.map (fun m -> InputMsg m) cmd)

      (* In your view *)
      Input.view model.input
    ]} *)

open Mosaic

type model
(** The internal state of the input *)

type msg
(** Messages the input can handle *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init :
  ?placeholder:string ->
  ?initial_value:string ->
  ?is_password:bool ->
  ?suggestions:string list ->
  ?validate:(string -> (unit, string) result) ->
  ?width:int ->
  unit ->
  model * msg Cmd.t
(** [init ?placeholder ?initial_value ?is_password ?suggestions ?validate ?width
     ()] creates a new input field.

    @param placeholder Text shown when empty and not focused
    @param initial_value Initial text content
    @param is_password Display asterisks instead of characters
    @param suggestions List of autocomplete suggestions
    @param validate Validation function returning Error with message on failure
    @param width Fixed width for the input field *)

(** {2 Accessors} *)

val value : model -> string
(** Get the current text value *)

val is_valid : model -> bool
(** Check if the current value passes validation *)

val error : model -> string option
(** Get the current validation error message, if any *)

val is_focused : model -> bool
(** Check if the input is currently focused *)

val suggestions : model -> string list
(** Get the current list of suggestions *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** Focus the input field *)

val blur : model -> model * msg Cmd.t
(** Remove focus from the input field *)

val set_value : string -> model -> model
(** Set the input's value programmatically *)

val clear : model -> model
(** Clear the input's value *)

val set_suggestions : string list -> model -> model
(** Update the suggestions list *)

val validate : model -> model
(** Force validation of the current value *)

val select_suggestion : int -> model -> model * msg Cmd.t
(** Select a suggestion by index *)

val hide_suggestions : model -> model * msg Cmd.t
(** Hide the suggestions dropdown *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  error_style : Style.t;
  placeholder_style : Style.t;
  suggestion_style : Style.t;
  selected_suggestion_style : Style.t;
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
