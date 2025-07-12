(** A multiple-choice selection component for selecting multiple items from a
    list of options.

    This component provides an interactive list where users can select multiple
    items, with optional filtering, selection limits, and custom theming. It
    supports keyboard navigation and maintains focus state for accessibility.

    {2 Architecture}

    State tracks multiple selections, highlight position, and filter text.
    Supports min/max selection limits. Filter uses case-insensitive substring
    matching.

    {2 Key Invariants}

    - Selection count never exceeds the specified limit (if set)
    - Filtered options always reflect the current filter text
    - The cursor position is always within valid bounds
    - Only visible options are rendered for performance
    - Toggle operations respect the selection limit

    {2 Example}

    {[
      (* Initialize a multi-select with programming languages *)
      let multi_model, multi_cmd =
        Multi_select.init
          ~options:[
            ("ocaml", "OCaml");
            ("rust", "Rust");
            ("go", "Go");
            ("python", "Python");
            ("js", "JavaScript");
            ("ts", "TypeScript");
            ("haskell", "Haskell");
          ]
          ~default:["ocaml"; "rust"]
          ~limit:3
          ~filterable:true
          ()

      (* In your update function *)
      | Multi_msg msg ->
          let new_multi, cmd = Multi_select.update msg model.multi in
          ({ model with multi = new_multi },
           Cmd.map (fun m -> Multi_msg m) cmd)

      (* In your view *)
      Multi_select.view model.multi

      (* Get selected values *)
      let selected = Multi_select.values model.multi in
      Printf.printf "Selected %d languages: %s\n"
        (Multi_select.selection_count model.multi)
        (String.concat ", " selected)

      (* Check if at selection limit *)
      if Multi_select.is_at_limit model.multi then
        print_endline "Maximum selection reached"
    ]} *)

open Mosaic

(** {2 Key Binding Configuration} *)

type key_config = Input.key_event list
(** Key binding configuration - list of keys to pass through instead of
    handling. *)

val default_key_config : key_config
(** Default key configuration - empty list (handles all keys). *)

type 'a model
(** The internal state of the multi-select, parameterized by the option value
    type. Contains selected items, options, filter state, and theme. *)

type msg
(** Messages that the multi-select can handle, including selection changes and
    filter updates. *)

(** {2 Initialization} *)

val init :
  ?options:('a * string) list ->
  ?default:'a list ->
  ?limit:int ->
  ?height:int ->
  ?filterable:bool ->
  ?key_config:key_config ->
  unit ->
  'a model * msg Cmd.t
(** [init ?options ?default ?limit ?height ?filterable ()] creates a new
    multi-select component.

    @param options List of (value, label) pairs
    @param default Initially selected values
    @param limit Maximum number of selections allowed (0 = unlimited)
    @param height Maximum visible items (default: 5)
    @param filterable Enable search/filter input (default: false)
    @param key_config
      Key binding configuration for filtering pass-through keys (default:
      handles all keys) *)

(** {2 Accessors} *)

val values : 'a model -> 'a list
(** [values model] returns the list of currently selected values. *)

val selected_indices : 'a model -> int list
(** [selected_indices model] returns the indices of selected items in the
    options list. *)

val options : 'a model -> ('a * string) list
(** [options model] returns all available options as [(value, label)] pairs. *)

val filtered_options : 'a model -> ('a * string) list
(** [filtered_options model] returns the currently visible options after
    applying any filter text. *)

val is_selected : 'a -> 'a model -> bool
(** [is_selected value model] returns whether the specific value is currently
    selected. *)

val selection_count : 'a model -> int
(** [selection_count model] returns the current number of selected items. *)

val is_at_limit : 'a model -> bool
(** [is_at_limit model] returns whether the selection limit has been reached.
    Always returns false if no limit is set. *)

val is_focused : 'a model -> bool
(** [is_focused model] returns whether the multi-select has keyboard focus. *)

val filter_text : 'a model -> string
(** [filter_text model] returns the current filter text. Returns empty string if
    filtering is not enabled. *)

(** {2 Actions} *)

val focus : 'a model -> 'a model * msg Cmd.t
(** [focus model] sets keyboard focus on the multi-select component. *)

val blur : 'a model -> 'a model * msg Cmd.t
(** [blur model] removes keyboard focus from the multi-select component. *)

val toggle : 'a -> 'a model -> 'a model
(** [toggle value model] toggles the selection state of a specific value,
    respecting any selection limit. *)

val toggle_index : int -> 'a model -> 'a model
(** [toggle_index idx model] toggles the selection state of the option at the
    given index. *)

val select : 'a -> 'a model -> 'a model
(** [select value model] selects a specific value if the selection limit has not
    been reached. *)

val deselect : 'a -> 'a model -> 'a model
(** [deselect value model] removes a specific value from the selection. *)

val select_all : 'a model -> 'a model
(** [select_all model] selects all options up to the selection limit. *)

val clear : 'a model -> 'a model
(** [clear model] clears all current selections. *)

val set_options : ('a * string) list -> 'a model -> 'a model
(** [set_options options model] replaces the current options list. Selections
    for values not in the new options are removed. *)

val set_filter : string -> 'a model -> 'a model
(** [set_filter text model] updates the filter text for filterable
    multi-selects. *)

val update_filter : string -> 'a model -> 'a model * msg Cmd.t
(** [update_filter text model] updates the filter text, returning a message for
    component updates. *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  unselected_style : Style.t;
  highlighted_style : Style.t;
  limit_style : Style.t;
  filter_style : Style.t;
}
(** Theme configuration for customizing the multi-select appearance.

    - [focused_style] - Style when the component has focus
    - [blurred_style] - Style when the component lacks focus
    - [selected_style] - Style for selected items
    - [unselected_style] - Style for unselected items
    - [highlighted_style] - Style for the currently highlighted item
    - [limit_style] - Style for the selection limit indicator
    - [filter_style] - Style for the filter input *)

val default_theme : theme
(** The default theme with standard styling for multi-select states. *)

val with_theme : theme -> 'a model -> 'a model
(** [with_theme theme model] applies a custom theme to the multi-select. *)

(** {2 Component Interface} *)

val update : msg -> 'a model -> 'a model * msg Cmd.t
(** [update msg model] handles messages and updates the multi-select state. *)

val view : 'a model -> Ui.element
(** [view model] renders the multi-select component with the current selection
    state. *)

val subscriptions : 'a model -> msg Sub.t
(** [subscriptions model] returns keyboard event subscriptions when the
    component has focus. *)
