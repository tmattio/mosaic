(** A file picker component

    This component provides a file browser with navigation capabilities. Similar
    to huh's FilePicker field.

    Example:
    {[
      (* Initialize a file picker *)
      let picker_model, picker_cmd =
        File_picker.init
          ~start_path:"."
          ~show_hidden:false
          ~extensions:[".ml"; ".mli"]
          ()

      (* In your update function *)
      | PickerMsg msg ->
          let new_picker, cmd = File_picker.update msg model.picker in
          ({ model with picker = new_picker },
           Cmd.map (fun m -> PickerMsg m) cmd)

      (* In your view - compose with labels as needed *)
      let open Ui in
      vbox ~gap:1 [
        text ~style:Style.bold "Select a file";
        File_picker.view model.picker;
        text ~style:Style.(fg (Index 8)) "(Enter to open/select, Backspace for parent)";
      ]

      (* Get selected file *)
      match File_picker.selected model.picker with
      | Some path -> Printf.printf "Selected: %s" path
      | None -> Printf.printf "No file selected"
    ]} *)

open Mosaic

type model
(** The internal state of the file picker *)

type msg
(** Messages the file picker can handle *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 File Types} *)

type file_type = [ `File | `Directory | `Symlink ]

type file_info = {
  name : string;
  path : string;
  file_type : file_type;
  size : int64;
  permissions : string;
}

(** {2 Initialization} *)

val init :
  ?start_path:string ->
  ?show_hidden:bool ->
  ?directories_only:bool ->
  ?extensions:string list ->
  ?height:int ->
  unit ->
  model * msg Cmd.t
(** [init ?start_path ?show_hidden ?directories_only ?extensions ?height ()]
    creates a new file picker.

    @param start_path Initial directory path (default: ".")
    @param show_hidden Show hidden files (default: false)
    @param directories_only Only allow directory selection (default: false)
    @param extensions Filter by file extensions (e.g., [".ml"; ".mli"])
    @param height Maximum visible items (default: 10) *)

(** {2 Accessors} *)

val selected : model -> string option
(** Get the currently selected file path *)

val current_directory : model -> string
(** Get the current directory being browsed *)

val files : model -> file_info list
(** Get the list of files in current directory *)

val is_focused : model -> bool
(** Check if the file picker is currently focused *)

val selected_file_info : model -> file_info option
(** Get detailed info about the selected file *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** Focus the file picker *)

val blur : model -> model * msg Cmd.t
(** Remove focus from the file picker *)

val navigate_to : string -> model -> model * msg Cmd.t
(** Navigate to a specific directory *)

val go_up : model -> model * msg Cmd.t
(** Navigate to parent directory *)

val refresh : model -> model * msg Cmd.t
(** Refresh the current directory listing *)

val toggle_hidden : model -> model * msg Cmd.t
(** Toggle showing hidden files *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  directory_style : Style.t;
  file_style : Style.t;
  symlink_style : Style.t;
  selected_style : Style.t;
  highlighted_style : Style.t;
  path_style : Style.t;
  info_style : Style.t;
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

(** {2 File Operations} *)

val load_directory : string -> model -> model * msg Cmd.t
(** Load a directory asynchronously *)

val select_file : string -> model -> model * msg Cmd.t
(** Select a file *)
