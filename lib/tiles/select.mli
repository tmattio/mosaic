(** A single-choice selection component

    This component provides a list of options where the user can select exactly
    one item. Similar to huh's Select field.

    Example:
    {[
      (* Initialize a select with options *)
      let select_model, select_cmd =
        Select.init
          ~title:"Choose your favorite language"
          ~options:[
            ("ocaml", "OCaml");
            ("rust", "Rust");
            ("go", "Go");
            ("python", "Python");
          ]
          ()

      (* In your update function *)
      | SelectMsg msg ->
          let new_select, cmd = Select.update msg model.select in
          ({ model with select = new_select },
           Cmd.map (fun m -> SelectMsg m) cmd)

      (* In your view *)
      Select.view model.select

      (* Get selected value *)
      match Select.value model.select with
      | Some key -> Printf.printf "Selected: %s" key
      | None -> Printf.printf "Nothing selected yet"
    ]} *)

open Mosaic

type 'a model
(** The internal state of the select, parameterized by option type *)

type msg
(** Messages the select can handle *)

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

    @param options List of (value, label) pairs
    @param default Initially selected value
    @param height Maximum visible items (default: 5)
    @param filterable Enable search/filter input (default: false)
    @param placeholder Text shown when no selection (default: "Select...") *)

(** {2 Accessors} *)

val value : 'a model -> 'a option
(** Get the currently selected value *)

val selected_index : 'a model -> int option
(** Get the index of the selected item *)

val options : 'a model -> ('a * string) list
(** Get all available options *)

val filtered_options : 'a model -> ('a * string) list
(** Get currently visible options (after filtering) *)

val is_focused : 'a model -> bool
(** Check if the select is currently focused *)

val is_open : 'a model -> bool
(** Check if the dropdown is open *)

val filter_text : 'a model -> string
(** Get the current filter text (if filterable) *)

(** {2 Actions} *)

val focus : 'a model -> 'a model * msg Cmd.t
(** Focus the select *)

val blur : 'a model -> 'a model * msg Cmd.t
(** Remove focus from the select *)

val open_dropdown : 'a model -> 'a model * msg Cmd.t
(** Open the dropdown list *)

val close_dropdown : 'a model -> 'a model * msg Cmd.t
(** Close the dropdown list *)

val toggle_dropdown : 'a model -> 'a model * msg Cmd.t
(** Toggle the dropdown open/closed state *)

val select : 'a -> 'a model -> 'a model
(** Select a specific value *)

val select_index : int -> 'a model -> 'a model
(** Select by index *)

val clear : 'a model -> 'a model
(** Clear the current selection *)

val set_options : ('a * string) list -> 'a model -> 'a model
(** Update the list of options *)

val set_filter : string -> 'a model -> 'a model
(** Update the filter text (for filterable selects) *)

val select_option : int -> 'a model -> 'a model * msg Cmd.t
(** Select an option by its index in the filtered list *)

val update_filter : string -> 'a model -> 'a model * msg Cmd.t
(** Update the filter text through a message *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  highlighted_style : Style.t;
  placeholder_style : Style.t;
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
