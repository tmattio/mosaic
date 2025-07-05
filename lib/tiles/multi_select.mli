(** A multiple-choice selection component

    This component provides a list of options where the user can select multiple
    items. Similar to huh's MultiSelect field.

    Example:
    {[
      (* Initialize a multi-select with options *)
      let multi_model, multi_cmd =
        Multi_select.init
          ~title:"Choose your skills"
          ~options:[
            ("ocaml", "OCaml");
            ("rust", "Rust");
            ("go", "Go");
            ("python", "Python");
            ("js", "JavaScript");
          ]
          ~limit:3
          ()

      (* In your update function *)
      | MultiMsg msg ->
          let new_multi, cmd = Multi_select.update msg model.multi in
          ({ model with multi = new_multi },
           Cmd.map (fun m -> MultiMsg m) cmd)

      (* In your view *)
      Multi_select.view model.multi

      (* Get selected values *)
      let selected = Multi_select.values model.multi in
      List.iter (Printf.printf "Selected: %s\n") selected
    ]} *)

open Mosaic

type 'a model
(** The internal state of the multi-select, parameterized by option type *)

type msg
(** Messages the multi-select can handle *)

(** {2 Initialization} *)

val init :
  ?options:('a * string) list ->
  ?default:'a list ->
  ?limit:int ->
  ?height:int ->
  ?filterable:bool ->
  unit ->
  'a model * msg Cmd.t
(** [init ?options ?default ?limit ?height ?filterable ()] creates a new
    multi-select component.

    @param options List of (value, label) pairs
    @param default Initially selected values
    @param limit Maximum number of selections allowed (0 = unlimited)
    @param height Maximum visible items (default: 5)
    @param filterable Enable search/filter input (default: false) *)

(** {2 Accessors} *)

val values : 'a model -> 'a list
(** Get the currently selected values *)

val selected_indices : 'a model -> int list
(** Get the indices of selected items *)

val options : 'a model -> ('a * string) list
(** Get all available options *)

val filtered_options : 'a model -> ('a * string) list
(** Get currently visible options (after filtering) *)

val is_selected : 'a -> 'a model -> bool
(** Check if a specific value is selected *)

val selection_count : 'a model -> int
(** Get the number of selected items *)

val is_at_limit : 'a model -> bool
(** Check if selection limit has been reached *)

val is_focused : 'a model -> bool
(** Check if the multi-select is currently focused *)

val filter_text : 'a model -> string
(** Get the current filter text (if filterable) *)

(** {2 Actions} *)

val focus : 'a model -> 'a model * msg Cmd.t
(** Focus the multi-select *)

val blur : 'a model -> 'a model * msg Cmd.t
(** Remove focus from the multi-select *)

val toggle : 'a -> 'a model -> 'a model
(** Toggle selection of a specific value *)

val toggle_index : int -> 'a model -> 'a model
(** Toggle selection by index *)

val select : 'a -> 'a model -> 'a model
(** Select a specific value (if not at limit) *)

val deselect : 'a -> 'a model -> 'a model
(** Deselect a specific value *)

val select_all : 'a model -> 'a model
(** Select all options (respecting limit) *)

val clear : 'a model -> 'a model
(** Clear all selections *)

val set_options : ('a * string) list -> 'a model -> 'a model
(** Update the list of options *)

val set_filter : string -> 'a model -> 'a model
(** Update the filter text (for filterable multi-selects) *)

val update_filter : string -> 'a model -> 'a model * msg Cmd.t
(** Update the filter text through a message *)

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

val default_theme : theme
val with_theme : theme -> 'a model -> 'a model

(** {2 Component Interface} *)

val update : msg -> 'a model -> 'a model * msg Cmd.t
(** Update function for the component *)

val view : 'a model -> Ui.element
(** View function for the component *)

val subscriptions : 'a model -> msg Sub.t
(** Subscriptions for the component *)
