(** Single-choice selection component with optional filtering.

    This component displays a dropdown list where users select exactly one
    option. Features include keyboard navigation, optional search filtering,
    scrolling for long lists, and customizable theming.

    {2 Architecture}

    State tracks selection, dropdown visibility, highlight position, and filter
    text. Filter matching uses case-insensitive substring search. Highlight
    index auto-adjusts to remain within filtered results.

    {2 Key Invariants}

    Only one option can be selected at a time. The highlighted index always
    points to a valid filtered option. Scroll offset ensures the highlighted
    item remains visible. Filter text only affects display, not the underlying
    options.

    {2 Example}

    Creates a language selector with filtering.
    {[
      (* Initialize with options *)
      let model, cmd = Select.init
        ~options:[
          ("ocaml", "OCaml - Functional programming");
          ("rust", "Rust - Systems programming");
          ("go", "Go - Concurrent programming");
          ("python", "Python - General purpose");
        ]
        ~filterable:true
        ~placeholder:"Choose a language..."
        ()

      (* Handle messages in update *)
      | Select_msg msg ->
          let model', cmd = Select.update msg model.select in
          { model with select = model' }, Cmd.map (fun m -> Select_msg m) cmd

      (* Render in view *)
      Select.view model.select

      (* Access selected value *)
      match Select.value model.select with
      | Some lang_id -> Printf.printf "Selected: %s" lang_id
      | None -> Printf.printf "No selection"
    ]} *)

open Mosaic

type 'a model
(** [model] represents the internal state of a select component.

    The type parameter ['a] is the type of option values. The model tracks
    selected value, dropdown state, highlighted item, scroll position, and
    filter text. Models are immutable and updated through the [update] function.
*)

type msg
(** [msg] represents internal messages processed by the select component.

    Messages include keyboard events, focus changes, dropdown toggling, and
    selection actions. Use [update] to process messages and produce new states.
*)

(** {2 Initialization} *)

val init :
  ?options:('a * string) list ->
  ?default:'a ->
  ?height:int ->
  ?filterable:bool ->
  ?placeholder:string ->
  unit ->
  'a model * msg Cmd.t
(** [init ?options ?default ?height ?filterable ?placeholder ()] creates a new
    select component.

    The component starts with dropdown closed and no focus. If [default] matches
    an option value, it becomes selected. The highlighted index starts at the
    selected item or first item. Returns initial model and startup command.

    @param options
      List of [(value, label)] pairs defining selectable items (default: empty
      list)
    @param default
      Initially selected value if it exists in options (default: none)
    @param height
      Maximum visible items before scrolling, clamped to positive values
      (default: 5)
    @param filterable
      Whether to show search input when dropdown opens (default: false)
    @param placeholder
      Text displayed when no selection exists (default: "Select...")

    Example: Creates country selector with default.
    {[
      let model, cmd =
        Select.init
          ~options:
            [
              ("us", "United States"); ("uk", "United Kingdom"); ("ca", "Canada");
            ]
          ~default:"us" ~filterable:true ()
    ]} *)

(** {2 Accessors} *)

val value : 'a model -> 'a option
(** [value model] returns the currently selected option value.

    Returns [None] if no selection has been made. The value corresponds to the
    first element of the selected option tuple. *)

val selected_index : 'a model -> int option
(** [selected_index model] returns the index of the selected item in the
    original options list.

    Returns [None] if no selection exists. Index is based on unfiltered options,
    not current visible items. *)

val options : 'a model -> ('a * string) list
(** [options model] returns all available options regardless of filtering.

    Returns the complete list of [(value, label)] pairs in their original order.
*)

val filtered_options : 'a model -> ('a * string) list
(** [filtered_options model] returns options matching the current filter text.

    If filtering is disabled or filter text is empty, returns all options.
    Filter matching is case-insensitive substring search. *)

val is_focused : 'a model -> bool
(** [is_focused model] checks whether the select component has keyboard focus.

    Focus affects visual styling and enables keyboard navigation. Focus is
    gained through [focus] action or user interaction. *)

val is_open : 'a model -> bool
(** [is_open model] checks whether the dropdown is currently visible.

    The dropdown shows available options and filter input if enabled. Opens on
    Enter/Space when focused. *)

val filter_text : 'a model -> string
(** [filter_text model] returns the current search filter string.

    Empty string when filtering is disabled or no filter applied. Used for
    case-insensitive substring matching. *)

(** {2 Actions} *)

val focus : 'a model -> 'a model * msg Cmd.t
(** [focus model] gives keyboard focus to the select component.

    Updates visual styling to focused state. Enables keyboard navigation.
    Returns updated model and no command. *)

val blur : 'a model -> 'a model * msg Cmd.t
(** [blur model] removes keyboard focus from the select component.

    Closes dropdown if open. Updates visual styling to blurred state. Returns
    updated model and no command. *)

val open_dropdown : 'a model -> 'a model * msg Cmd.t
(** [open_dropdown model] shows the dropdown list of options.

    No effect if already open. Resets filter text when opening. Ensures
    highlighted item is visible. *)

val close_dropdown : 'a model -> 'a model * msg Cmd.t
(** [close_dropdown model] hides the dropdown list.

    No effect if already closed. Clears filter text when filterable. Preserves
    selection and highlight position. *)

val toggle_dropdown : 'a model -> 'a model * msg Cmd.t
(** [toggle_dropdown model] switches dropdown between open and closed states.

    Opens if closed, closes if open. Handles filter text appropriately.
    Convenience for click handlers. *)

val select : 'a -> 'a model -> 'a model
(** [select value model] sets the selected value directly.

    Updates highlighted index to match selection. No effect if value not in
    options. Does not close dropdown. *)

val select_index : int -> 'a model -> 'a model
(** [select_index idx model] selects the option at the given index.

    Index refers to original unfiltered options. Updates highlighted position.
    Clamps to valid range. *)

val clear : 'a model -> 'a model
(** [clear model] removes the current selection.

    Sets selected value to [None]. Highlighted index remains unchanged. Useful
    for reset functionality. *)

val set_options : ('a * string) list -> 'a model -> 'a model
(** [set_options opts model] replaces all available options.

    Clears selection if current value not in new options. Resets highlight to
    first item. Clears filter text. *)

val set_filter : string -> 'a model -> 'a model
(** [set_filter text model] updates the search filter text directly.

    Only affects display when filterable is enabled. Updates filtered options
    and adjusts highlight position. *)

val select_option : int -> 'a model -> 'a model * msg Cmd.t
(** [select_option idx model] selects the option at index [idx] in the filtered
    list.

    Unlike [select_index], this uses the index within currently visible filtered
    options. Closes dropdown after selection. Returns updated model and no
    command. *)

val update_filter : string -> 'a model -> 'a model * msg Cmd.t
(** [update_filter text model] updates filter text through the message system.

    Triggers filtering logic and highlight adjustment. Use this instead of
    [set_filter] for proper event flow. *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  highlighted_style : Style.t;
  placeholder_style : Style.t;
  filter_style : Style.t;
}
(** [theme] controls the visual appearance of the select component.

    Each style applies to different states: focused_style for focused component
    border, blurred_style for unfocused state, selected_style for the chosen
    option, highlighted_style for the cursor position, placeholder_style for
    empty state text, and filter_style for the search input. *)

val default_theme : theme
(** [default_theme] provides a standard color scheme.

    Uses ANSI indexed colors for compatibility. Blue for focus, gray for blur,
    green for selection, inverted for highlight. *)

val with_theme : theme -> 'a model -> 'a model
(** [with_theme theme model] applies a custom theme to the select.

    Updates all visual styles. Changes take effect immediately in next render.
*)

(** {2 Component Interface} *)

val update : msg -> 'a model -> 'a model * msg Cmd.t
(** [update msg model] processes a message to produce a new model state.

    Handles keyboard navigation, mouse clicks, focus changes, and selection
    updates. Returns updated model and any commands to execute. Pure function
    without side effects.

    Example: Integrates with parent update function.
    {[
      | Select_msg msg ->
          let model', cmd = Select.update msg model.select in
          { model with select = model' }, Cmd.map (fun m -> Select_msg m) cmd
    ]} *)

val view : 'a model -> Ui.element
(** [view model] renders the select component as a UI element.

    Displays current selection or placeholder. Shows dropdown when open with
    filtered options. Applies theme styles based on state. Handles layout for
    scrolling when needed.

    The rendered element includes the selection box and dropdown list when
    visible. *)

val subscriptions : 'a model -> msg Sub.t
(** [subscriptions model] returns event subscriptions based on current state.

    Subscribes to keyboard events when focused for navigation (arrows, enter,
    escape). No subscriptions when blurred. Automatically managed based on focus
    state.

    Example: Combines with other subscriptions.
    {[
      Sub.batch
        [
          Select.subscriptions model.select |> Sub.map (fun m -> Select_msg m);
          Sub.on_key Escape `Cancel;
        ]
    ]} *)
