(** A single-line text input component with validation, suggestions, and
    advanced input handling features.

    This component provides a full-featured text input field supporting
    real-time validation, autocomplete suggestions, password masking, and
    customizable styling. It handles keyboard input, focus management, and
    provides visual feedback for validation errors.

    {2 Architecture}

    State tracks text value, focus, validation errors, and suggestion dropdown.
    Validation runs on every value change. Password mode masks input with
    asterisks.

    {2 Key Invariants}

    - Validation runs on every value change
    - Suggestions are hidden when input loses focus
    - Password mode displays asterisks instead of characters
    - Error state is cleared when validation passes
    - Suggestion selection updates the input value

    {2 Example}

    {[
      (* Initialize an email input with validation *)
      let input_model, input_cmd =
        Input.init
          ~placeholder:"Enter email"
          ~validate:(fun s ->
            if s = "" then Ok ()
            else if String.contains s '@' && String.contains s '.' then Ok ()
            else Error "Please enter a valid email address")
          ~suggestions:["user@example.com"; "admin@site.org"]
          ~width:40
          ()

      (* In your update function *)
      | Input_msg msg ->
          let new_input, cmd = Input.update msg model.input in
          ({ model with input = new_input },
           Cmd.map (fun m -> Input_msg m) cmd)

      (* In your view with error display *)
      let open Ui in
      vbox ~gap:1 [
        text "Email Address:";
        Input.view model.input;
        (match Input.error model.input with
         | Some err -> text ~style:Style.(fg Red) err
         | None -> empty);
      ]

      (* Get validated value *)
      if Input.is_valid model.input then
        let email = Input.value model.input in
        process_email email

      (* Password input example *)
      let password_input =
        Input.init ~placeholder:"Password" ~is_password:true ()
    ]} *)

open Mosaic

type model
(** The internal state of the input containing current value, validation state,
    suggestions, and configuration. *)

type msg
(** Messages that the input can handle, including text changes, focus events,
    and suggestion selection. *)

type key_binding = msg Key_binding.action
(** Key binding action type for the input component. *)

type key_config = msg Key_binding.config
(** Key binding configuration for the input component. *)

val default_key_config : key_config
(** Default key configuration - inserts all keys as text. *)

val component : (model, msg) Mosaic.app
(** The input component definition following The Elm Architecture. *)

(** {2 Initialization} *)

val init :
  ?placeholder:string ->
  ?initial_value:string ->
  ?is_password:bool ->
  ?suggestions:string list ->
  ?validate:(string -> (unit, string) result) ->
  ?width:int ->
  ?key_config:key_config ->
  unit ->
  model * msg Cmd.t
(** [init ?placeholder ?initial_value ?is_password ?suggestions ?validate ?width
     ()] creates a new input field.

    @param placeholder Text shown when empty and not focused
    @param initial_value Initial text content
    @param is_password Display asterisks instead of characters
    @param suggestions List of autocomplete suggestions
    @param validate Validation function returning Error with message on failure
    @param width Fixed width for the input field
    @param key_config Key binding configuration (default: all keys insert text)
*)

(** {2 Accessors} *)

val value : model -> string
(** [value model] returns the current text content of the input. *)

val is_valid : model -> bool
(** [is_valid model] returns whether the current value passes validation. *)

val error : model -> string option
(** [error model] returns the current validation error message, if any. *)

val is_focused : model -> bool
(** [is_focused model] returns whether the input currently has keyboard focus.
*)

val suggestions : model -> string list
(** [suggestions model] returns the list of autocomplete suggestions. *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** [focus model] sets keyboard focus on the input field. *)

val blur : model -> model * msg Cmd.t
(** [blur model] removes keyboard focus from the input field. *)

val set_value : string -> model -> model
(** [set_value text model] sets the input value to [text] and runs validation.
*)

val clear : model -> model
(** [clear model] clears the input value and resets validation state. *)

val set_suggestions : string list -> model -> model
(** [set_suggestions suggestions model] updates the list of autocomplete
    suggestions. *)

val validate : model -> model
(** [validate model] forces validation of the current value, updating the error
    state. *)

val select_suggestion : int -> model -> model * msg Cmd.t
(** [select_suggestion idx model] selects the suggestion at index [idx],
    updating the input value. *)

val hide_suggestions : model -> model * msg Cmd.t
(** [hide_suggestions model] hides the suggestions dropdown. *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  error_style : Style.t;
  placeholder_style : Style.t;
  suggestion_style : Style.t;
  selected_suggestion_style : Style.t;
}
(** Theme configuration for customizing the input appearance.

    - [focused_style] - Style when the input has focus
    - [blurred_style] - Style when the input lacks focus
    - [error_style] - Style when validation fails
    - [placeholder_style] - Style for placeholder text
    - [suggestion_style] - Style for suggestion items
    - [selected_suggestion_style] - Style for highlighted suggestion *)

val default_theme : theme
(** The default theme with standard styling for input states. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the input. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the input state, including
    text changes and validation. *)

val view : model -> Ui.element
(** [view model] renders the input field with current value, placeholder, and
    any visible suggestions. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns keyboard event subscriptions when the input
    has focus. *)
