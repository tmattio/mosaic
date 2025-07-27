(** Multiline text editor component with scrolling and validation.

    This component provides a text area for multiline input. Features include
    keyboard navigation, scrolling for long content, word wrapping, cursor
    positioning, and optional validation.

    {2 Architecture}

    State tracks text lines, cursor position (line/column), and viewport scroll.
    Word wrapping recalculates on width change. Cursor auto-scrolls into view.

    {2 Key Invariants}

    Cursor position always points to a valid location within the text. Scroll
    offset ensures the cursor remains visible. Line breaks are normalized to LF.
    Validation runs on every content change when a validator is provided.

    {2 Example}

    Creates a description editor with validation.
    {[
      (* Initialize with constraints *)
      let model, cmd = Text.init
        ~placeholder:"Enter a detailed description..."
        ~height:10
        ~validate:(fun s ->
          if String.length s < 10 then
            Error "Description must be at least 10 characters"
          else Ok ())
        ()

      (* Handle messages in update *)
      | Text_msg msg ->
          let model', cmd = Text.update msg model.text in
          { model with text = model' }, Cmd.map (fun m -> Text_msg m) cmd

      (* Render in view *)
      Text.view model.text

      (* Access the content *)
      let content = Text.value model.text in
      Printf.printf "Entered: %s" content
    ]} *)

open Mosaic

(** {2 Key Binding Configuration} *)

type msg
(** [msg] represents internal messages processed by the text area component.

    Messages include keyboard events, focus changes, content modifications, and
    cursor movements. Use [update] to process messages and produce new states.
*)

type key_binding = msg Key_binding.action
(** [key_binding] is an alias for the key binding action type from Key_binding.
*)

type key_config = msg Key_binding.config
(** [key_config] is an alias for the key binding configuration from Key_binding.
*)

val default_key_config : key_config
(** [default_key_config] inserts all keys as text with no special handling. *)

type model
(** [model] represents the internal state of a text area component.

    The model tracks text content, cursor position, scroll offset, focus state,
    and validation results. Models are immutable and updated through the
    [update] function. *)

val component : (model, msg) Mosaic.app
(** [component] provides the complete application interface for the text area.

    Bundles the [init], [update], [view], and [subscriptions] functions into a
    single record for use with the Mosaic framework. *)

(** {2 Initialization} *)

val init :
  ?placeholder:string ->
  ?initial_value:string ->
  ?height:int ->
  ?width:int ->
  ?word_wrap:bool ->
  ?validate:(string -> (unit, string) result) ->
  ?key_config:key_config ->
  unit ->
  model * msg Cmd.t
(** [init ?placeholder ?initial_value ?height ?width ?word_wrap ?validate ()]
    creates a new text area component.

    The component starts unfocused with cursor at the beginning. If
    [initial_value] is provided, it becomes the content and cursor moves to the
    end. Validation runs immediately if a validator is provided. Returns initial
    model and startup command.

    @param placeholder
      Text displayed when content is empty and unfocused (default: "")
    @param initial_value
      Initial text content, with cursor positioned at end (default: "")
    @param height
      Number of visible lines, clamped to positive values (default: 5)
    @param width Fixed character width for the text area (default: 60)
    @param word_wrap
      Whether to wrap long lines at word boundaries (default: true)
    @param validate
      Function to validate content, returning [Ok ()] or [Error message]
      (default: always valid)
    @param key_config
      Key binding configuration for handling keyboard events (default: all keys
      are inserted as text)

    Example: Creates a commit message editor.
    {[
      let model, cmd =
        Text.init ~placeholder:"Enter commit message..." ~height:3
          ~validate:(fun s ->
            if String.length s = 0 then Error "Message required"
            else if String.length s > 72 then Error "First line too long"
            else Ok ())
          ()
    ]} *)

(** {2 Accessors} *)

val value : model -> string
(** [value model] returns the current text content.

    Returns the complete text including all line breaks. Line endings are
    normalized to LF regardless of platform. *)

val lines : model -> string list
(** [lines model] returns the text content split by line breaks.

    Each element represents one line without trailing newlines. Empty text
    returns a single empty string. Preserves empty lines. *)

val line_count : model -> int
(** [line_count model] returns the total number of lines in the text.

    Empty text has 1 line. Each newline character increases the count by 1. *)

val cursor_position : model -> int * int
(** [cursor_position model] returns the cursor location as [(line, column)].

    Both line and column are 0-indexed. Column represents the character offset
    within the line. Position is always valid within the text bounds. *)

val is_focused : model -> bool
(** [is_focused model] checks whether the text area has keyboard focus.

    Focus affects visual styling and enables keyboard input. Focus is gained
    through [focus] action or user interaction. *)

val is_valid : model -> bool
(** [is_valid model] checks whether the current content passes validation.

    Always returns true if no validator was provided. Validation runs on every
    content change. *)

val error : model -> string option
(** [error model] returns the current validation error message.

    Returns [None] if content is valid or no validator provided. Returns
    [Some message] when validation fails. Updates automatically on content
    changes. *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** [focus model] gives keyboard focus to the text area.

    Updates visual styling to focused state. Enables keyboard input and
    navigation. Returns updated model and no command. *)

val blur : model -> model * msg Cmd.t
(** [blur model] removes keyboard focus from the text area.

    Updates visual styling to blurred state. Disables keyboard input. Returns
    updated model and no command. *)

val set_value : string -> model -> model
(** [set_value text model] replaces the entire content with new text.

    Moves cursor to end of new text. Runs validation if configured. Adjusts
    scroll to show cursor. *)

val clear : model -> model
(** [clear model] removes all text content.

    Sets content to empty string. Moves cursor to beginning. Resets scroll
    position. Runs validation. *)

val insert_at_cursor : string -> model -> model
(** [insert_at_cursor text model] inserts text at the current cursor position.

    Preserves existing content before and after cursor. Moves cursor to end of
    inserted text. Runs validation and adjusts scroll. *)

val go_to_line : int -> model -> model
(** [go_to_line line_num model] moves the cursor to the beginning of a line.

    Line numbers are 1-based. Clamps to valid range if out of bounds. Adjusts
    scroll to ensure line is visible. *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  error_style : Style.t;
  placeholder_style : Style.t;
  line_numbers_style : Style.t;
  cursor_style : Style.t;
}
(** [theme] controls the visual appearance of the text area component.

    Each style applies to different states: focused_style for active editing,
    blurred_style for inactive state, error_style for validation failures,
    placeholder_style for empty state hint, line_numbers_style for optional line
    numbers, and cursor_style for the text insertion point. *)

val default_theme : theme
(** [default_theme] provides a standard color scheme.

    Uses ANSI indexed colors for compatibility. Blue for focus, gray for blur,
    red for errors, dim for placeholder. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the text area.

    Updates all visual styles. Changes take effect immediately in next render.
*)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] processes a message to produce a new model state.

    Handles keyboard input, cursor movement, content changes, and focus events.
    Runs validation after content modifications. Returns updated model and any
    commands to execute.

    Example: Integrates with parent update function.
    {[
      | Text_msg msg ->
          let model', cmd = Text.update msg model.text in
          { model with text = model' }, Cmd.map (fun m -> Text_msg m) cmd
    ]} *)

val view : model -> Ui.element
(** [view model] renders the text area as a UI element.

    Displays text content with cursor, scroll bars when needed, and validation
    errors. Shows placeholder when empty and unfocused. Applies theme styles
    based on state.

    The rendered element includes the text area with optional line numbers and
    error messages. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns event subscriptions based on current state.

    Subscribes to keyboard events when focused for text input and navigation
    (arrows, home, end, page up/down). No subscriptions when blurred.
    Automatically managed based on focus state.

    Example: Combines with other subscriptions.
    {[
      Sub.batch
        [
          Text.subscriptions model.text |> Sub.map (fun m -> Text_msg m);
          Sub.on_key Escape `Cancel;
        ]
    ]} *)
