(** A file picker component for browsing and selecting files from the file
    system.

    This component provides a navigable file browser with support for filtering
    by extensions, showing/hiding hidden files, and directory-only selection. It
    includes keyboard navigation and maintains file metadata for display.

    {2 Architecture}

    State tracks current directory, selected file, and extension filters.
    Directory loading is asynchronous. Files filtered by extension whitelist when
    provided.

    {2 Key Invariants}

    - The current directory always exists and is accessible
    - File listings are sorted with directories first, then files alphabetically
    - Hidden files (starting with '.') are only shown when enabled
    - Extension filtering only applies to files, not directories
    - Navigation respects file system permissions

    {2 Example}

    {[
      (* Initialize a file picker for OCaml files *)
      let picker_model, picker_cmd =
        File_picker.init
          ~start_path:"./src"
          ~show_hidden:false
          ~extensions:[".ml"; ".mli"]
          ~height:15
          ()

      (* In your update function *)
      | Picker_msg msg ->
          let new_picker, cmd = File_picker.update msg model.picker in
          ({ model with picker = new_picker },
           Cmd.map (fun m -> Picker_msg m) cmd)

      (* In your view with navigation hints *)
      let open Ui in
      vbox ~gap:1 [
        text ~style:Style.bold "Select an OCaml file:";
        text ~style:Style.(fg (Index 8))
          (File_picker.current_directory model.picker);
        File_picker.view model.picker;
        text ~style:Style.(fg (Index 8))
          "↑/↓: Navigate • Enter: Open/Select • Backspace: Parent • h: Toggle hidden";
      ]

      (* Handle selected file *)
      match File_picker.selected model.picker with
      | Some path ->
          Printf.printf "Selected: %s\n" path;
          (* Process the selected file *)
      | None -> ()

      (* Get file info *)
      match File_picker.selected_file_info model.picker with
      | Some info ->
          Printf.printf "File: %s, Size: %Ld bytes, Type: %s\n"
            info.name info.size
            (match info.file_type with
             | `File -> "file"
             | `Directory -> "directory"
             | `Symlink -> "symlink")
      | None -> ()
    ]} *)

open Mosaic

type model
(** The internal state of the file picker containing current directory, file
    list, selection state, and configuration. *)

type msg
(** Messages that the file picker can handle, including navigation and selection
    actions. *)

val component : (model, msg) Mosaic.app
(** The file picker component definition following The Elm Architecture. *)

(** {2 File Types} *)

type file_type = [ `File | `Directory | `Symlink ]
(** File types distinguished by the file picker.

    - [`File] - Regular file
    - [`Directory] - Directory that can be navigated into
    - [`Symlink] - Symbolic link *)

type file_info = {
  name : string;
  path : string;
  file_type : file_type;
  size : int64;
  permissions : string;
}
(** Information about a file or directory.

    - [name] - The file or directory name
    - [path] - Full absolute path
    - [file_type] - Type of file system entry
    - [size] - Size in bytes
    - [permissions] - Unix permission string *)

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
(** [selected model] returns the currently selected file path, if any. *)

val current_directory : model -> string
(** [current_directory model] returns the path of the current directory. *)

val files : model -> file_info list
(** [files model] returns the list of files and directories in the current
    directory, filtered according to the current settings. *)

val is_focused : model -> bool
(** [is_focused model] returns whether the file picker has keyboard focus. *)

val selected_file_info : model -> file_info option
(** [selected_file_info model] returns detailed information about the currently
    selected file or directory. *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** [focus model] sets keyboard focus on the file picker. *)

val blur : model -> model * msg Cmd.t
(** [blur model] removes keyboard focus from the file picker. *)

val navigate_to : string -> model -> model * msg Cmd.t
(** [navigate_to path model] navigates to the specified directory path. *)

val go_up : model -> model * msg Cmd.t
(** [go_up model] navigates to the parent directory. *)

val refresh : model -> model * msg Cmd.t
(** [refresh model] reloads the current directory listing. *)

val toggle_hidden : model -> model * msg Cmd.t
(** [toggle_hidden model] toggles the visibility of hidden files (files starting
    with '.'). *)

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
(** Theme configuration for customizing the file picker appearance.

    - [focused_style] - Style when the component has focus
    - [blurred_style] - Style when the component lacks focus
    - [directory_style] - Style for directory entries
    - [file_style] - Style for file entries
    - [symlink_style] - Style for symbolic links
    - [selected_style] - Style for the selected item
    - [highlighted_style] - Style for the highlighted item under cursor
    - [path_style] - Style for the current path display
    - [info_style] - Style for file information display *)

val default_theme : theme
(** The default theme with standard styling for file types. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the file picker. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the file picker state. *)

val view : model -> Ui.element
(** [view model] renders the file picker with the current directory listing. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns keyboard event subscriptions when focused. *)

(** {2 File Operations} *)

val load_directory : string -> model -> model * msg Cmd.t
(** [load_directory path model] asynchronously loads the contents of the
    specified directory. *)

val select_file : string -> model -> model * msg Cmd.t
(** [select_file path model] selects the file at the given path. *)
