(** Implementation of the file picker component *)

open Mosaic

(* File types and info *)
type file_type = [ `File | `Directory | `Symlink ]

type file_info = {
  name : string;
  path : string;
  file_type : file_type;
  size : int64;
  permissions : string;
}

(* Theme configuration *)
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

let default_theme =
  {
    focused_style = Style.(fg (Index 6));
    blurred_style = Style.(fg (Index 8));
    directory_style = Style.(fg Blue ++ bold);
    file_style = Style.empty;
    symlink_style = Style.(fg Cyan);
    selected_style = Style.(fg Green);
    highlighted_style = Style.(bg (Index 8) ++ fg White);
    path_style = Style.(fg (Index 6));
    info_style = Style.(fg (Index 8));
  }

(* Model *)
type model = {
  (* Configuration *)
  show_hidden : bool;
  directories_only : bool;
  extensions : string list;
  height : int;
  (* State *)
  current_directory : string;
  files : file_info list;
  selected_path : string option;
  highlighted_index : int;
  is_focused : bool;
  scroll_offset : int;
  error : string option;
  (* Theme *)
  theme : theme;
}

(* Messages *)
type msg =
  | Key of Input.key_event
  | Focus
  | Blur
  | Load_directory of string
  | Directory_loaded of file_info list
  | Load_error of string
  | Select of string

(* Helper to check if file should be shown *)
let should_show_file model file =
  (* Check if hidden *)
  if
    (not model.show_hidden)
    && String.length file.name > 0
    && file.name.[0] = '.'
  then false
  else
    match file.file_type with
    | `Directory -> true
    | `File | `Symlink ->
        if model.directories_only then false
        else if model.extensions = [] then true
        else
          List.exists
            (fun ext -> String.ends_with ~suffix:ext file.name)
            model.extensions

(* Helper to get visible files *)
let get_visible_files model =
  List.filter (should_show_file model) model.files
  |> List.sort (fun a b ->
         (* Directories first, then alphabetical *)
         match (a.file_type, b.file_type) with
         | `Directory, `Directory -> String.compare a.name b.name
         | `Directory, _ -> -1
         | _, `Directory -> 1
         | _, _ -> String.compare a.name b.name)

(* Helper to ensure highlighted index is valid *)
let clamp_highlight model =
  let visible = get_visible_files model in
  let max_idx = max 0 (List.length visible - 1) in
  { model with highlighted_index = max 0 (min max_idx model.highlighted_index) }

(* Helper to ensure scroll shows highlighted item *)
let ensure_highlight_visible model =
  let visible_start = model.scroll_offset in
  let visible_end = model.scroll_offset + model.height - 1 in
  if model.highlighted_index < visible_start then
    { model with scroll_offset = model.highlighted_index }
  else if model.highlighted_index > visible_end then
    { model with scroll_offset = model.highlighted_index - model.height + 1 }
  else model

(* Load directory files using Unix file system APIs *)
let load_directory_files path =
  let dir_handle = Unix.opendir path in
  let rec read_entries acc =
    try
      let name = Unix.readdir dir_handle in
      (* Skip . and .. *)
      if name = "." || name = ".." then read_entries acc
      else
        let full_path = Filename.concat path name in
        try
          let stat = Unix.stat full_path in
          let file_type =
            match stat.st_kind with
            | Unix.S_REG -> `File
            | Unix.S_DIR -> `Directory
            | Unix.S_LNK -> `Symlink
            | _ -> `File (* Treat other types as files *)
          in
          let permissions =
            Printf.sprintf "%c%c%c%c%c%c%c%c%c%c"
              (match stat.st_kind with
              | Unix.S_REG -> '-'
              | Unix.S_DIR -> 'd'
              | Unix.S_LNK -> 'l'
              | Unix.S_CHR -> 'c'
              | Unix.S_BLK -> 'b'
              | Unix.S_FIFO -> 'p'
              | Unix.S_SOCK -> 's')
              (if stat.st_perm land 0o400 <> 0 then 'r' else '-')
              (if stat.st_perm land 0o200 <> 0 then 'w' else '-')
              (if stat.st_perm land 0o100 <> 0 then 'x' else '-')
              (if stat.st_perm land 0o040 <> 0 then 'r' else '-')
              (if stat.st_perm land 0o020 <> 0 then 'w' else '-')
              (if stat.st_perm land 0o010 <> 0 then 'x' else '-')
              (if stat.st_perm land 0o004 <> 0 then 'r' else '-')
              (if stat.st_perm land 0o002 <> 0 then 'w' else '-')
              (if stat.st_perm land 0o001 <> 0 then 'x' else '-')
          in
          let file_info =
            {
              name;
              path = full_path;
              file_type;
              size = Int64.of_int stat.st_size;
              permissions;
            }
          in
          read_entries (file_info :: acc)
        with Unix.Unix_error _ ->
          (* Skip files we can't stat *)
          read_entries acc
    with End_of_file ->
      Unix.closedir dir_handle;
      acc
  in
  Fun.protect
    ~finally:(fun () -> try Unix.closedir dir_handle with _ -> ())
    (fun () -> read_entries [])

(* Helper to get parent directory *)
let parent_directory path =
  if path = "/" then "/"
  else
    let parts = String.split_on_char '/' path in
    let rec remove_last = function
      | [] | [ _ ] -> []
      | h :: t -> h :: remove_last t
    in
    match remove_last parts with [] -> "/" | parts -> String.concat "/" parts

(* Initialization *)
let init ?(start_path = ".") ?(show_hidden = false) ?(directories_only = false)
    ?(extensions = []) ?(height = 10) () =
  let model =
    {
      show_hidden;
      directories_only;
      extensions;
      height;
      current_directory = start_path;
      files = [];
      selected_path = None;
      highlighted_index = 0;
      is_focused = false;
      scroll_offset = 0;
      error = None;
      theme = default_theme;
    }
  in
  (* Load initial directory asynchronously *)
  (model, Cmd.msg (Load_directory start_path))

(* Update *)
let update msg model =
  match msg with
  | Key { key; _ } -> (
      match key with
      | Up ->
          let model =
            { model with highlighted_index = model.highlighted_index - 1 }
          in
          (clamp_highlight model |> ensure_highlight_visible, Cmd.none)
      | Down ->
          let model =
            { model with highlighted_index = model.highlighted_index + 1 }
          in
          (clamp_highlight model |> ensure_highlight_visible, Cmd.none)
      | Page_up ->
          let model =
            {
              model with
              highlighted_index = max 0 (model.highlighted_index - model.height);
              scroll_offset = max 0 (model.scroll_offset - model.height);
            }
          in
          (clamp_highlight model, Cmd.none)
      | Page_down ->
          let visible = get_visible_files model in
          let max_idx = List.length visible - 1 in
          let model =
            {
              model with
              highlighted_index =
                min max_idx (model.highlighted_index + model.height);
              scroll_offset =
                min
                  (max 0 (max_idx - model.height + 1))
                  (model.scroll_offset + model.height);
            }
          in
          (clamp_highlight model, Cmd.none)
      | Enter -> (
          let visible = get_visible_files model in
          match List.nth_opt visible model.highlighted_index with
          | Some file -> (
              match file.file_type with
              | `Directory ->
                  (* Navigate into directory *)
                  (model, Cmd.msg (Load_directory file.path))
              | `File | `Symlink ->
                  (* Select file *)
                  (model, Cmd.msg (Select file.path)))
          | None -> (model, Cmd.none))
      | Backspace ->
          (* Go to parent directory *)
          let parent = parent_directory model.current_directory in
          if parent <> model.current_directory then
            (model, Cmd.msg (Load_directory parent))
          else (model, Cmd.none)
      | Char c when Uchar.to_char c = 'h' ->
          (* Toggle hidden files *)
          ({ model with show_hidden = not model.show_hidden }, Cmd.none)
      | _ -> (model, Cmd.none))
  | Focus -> ({ model with is_focused = true }, Cmd.none)
  | Blur -> ({ model with is_focused = false }, Cmd.none)
  | Load_directory path ->
      (* Start async loading *)
      ( { model with current_directory = path; files = []; error = None },
        Cmd.perform (fun () ->
            try
              let files = load_directory_files path in
              Some (Directory_loaded files)
            with
            | Unix.Unix_error (Unix.ENOENT, _, _) ->
                Some
                  (Load_error (Printf.sprintf "Directory not found: %s" path))
            | Unix.Unix_error (Unix.EACCES, _, _) ->
                Some (Load_error (Printf.sprintf "Permission denied: %s" path))
            | Unix.Unix_error (err, _, _) ->
                Some
                  (Load_error
                     (Printf.sprintf "Error accessing %s: %s" path
                        (Unix.error_message err)))
            | exn ->
                Some
                  (Load_error
                     (Printf.sprintf "Failed to load directory %s: %s" path
                        (Printexc.to_string exn)))) )
  | Directory_loaded files -> ({ model with files; error = None }, Cmd.none)
  | Load_error error -> ({ model with error = Some error }, Cmd.none)
  | Select path -> ({ model with selected_path = Some path }, Cmd.none)

(* View *)
let view model =
  let open Ui in
  (* Current path *)
  let path_elem =
    hbox
      [
        text ~style:model.theme.path_style "Path: ";
        text model.current_directory;
      ]
  in

  (* File list *)
  let visible = get_visible_files model in
  let visible_files =
    let start = model.scroll_offset in
    let end_ = min (List.length visible) (start + model.height) in
    let rec take n lst =
      match (n, lst) with
      | 0, _ | _, [] -> []
      | n, h :: t -> h :: take (n - 1) t
    in
    let rec drop n lst =
      match (n, lst) with
      | 0, _ -> lst
      | _, [] -> []
      | n, _ :: t -> drop (n - 1) t
    in
    visible |> drop start |> take (end_ - start)
  in

  let file_items =
    List.mapi
      (fun i file ->
        let actual_idx = model.scroll_offset + i in
        let is_selected = Some file.path = model.selected_path in
        let is_highlighted = actual_idx = model.highlighted_index in

        let icon =
          match file.file_type with
          | `Directory -> "📁 "
          | `File -> "📄 "
          | `Symlink -> "🔗 "
        in

        let file_style =
          match file.file_type with
          | `Directory -> model.theme.directory_style
          | `File -> model.theme.file_style
          | `Symlink -> model.theme.symlink_style
        in

        let style =
          if is_highlighted && model.is_focused then
            model.theme.highlighted_style
          else if is_selected then model.theme.selected_style
          else file_style
        in

        let size_str =
          if file.file_type = `Directory then ""
          else Printf.sprintf " (%Ld bytes)" file.size
        in

        text ~style (icon ^ file.name ^ size_str))
      visible_files
  in

  let file_list =
    vbox ~padding:(padding_all 1)
      ~border:
        (border ~style:Solid
           ~color:(if model.is_focused then Style.Index 6 else Style.Index 8)
           ())
      ~height:(model.height + 2) file_items
  in

  (* Selected file info *)
  let selected_info =
    match model.selected_path with
    | Some path -> [ text ~style:model.theme.info_style ("Selected: " ^ path) ]
    | None -> []
  in

  (* Error message *)
  let error_elem =
    match model.error with
    | Some err -> [ text ~style:(Style.fg Red) ("Error: " ^ err) ]
    | None -> []
  in

  (* Layout *)
  vbox ([ path_elem; file_list ] @ selected_info @ error_elem)

(* Subscriptions *)
let subscriptions model =
  if model.is_focused then Sub.keyboard (fun k -> Key k) else Sub.none

(* Component export *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()

(* Actions *)
let load_directory path model = (model, Cmd.msg (Load_directory path))
let select_file path model = (model, Cmd.msg (Select path))

(* Accessors *)
let selected model = model.selected_path
let current_directory model = model.current_directory
let files model = model.files
let is_focused model = model.is_focused

let selected_file_info model =
  match model.selected_path with
  | Some path -> List.find_opt (fun f -> f.path = path) model.files
  | None -> None

(* Actions *)
let focus model = ({ model with is_focused = true }, Cmd.msg Focus)
let blur model = ({ model with is_focused = false }, Cmd.msg Blur)

let navigate_to path model =
  let files = load_directory_files path in
  ( {
      model with
      current_directory = path;
      files;
      highlighted_index = 0;
      scroll_offset = 0;
      error = None;
    },
    Cmd.none )

let go_up model =
  let parent = parent_directory model.current_directory in
  navigate_to parent model

let refresh model =
  let files = load_directory_files model.current_directory in
  ({ model with files; error = None }, Cmd.none)

let toggle_hidden model =
  let model = { model with show_hidden = not model.show_hidden } in
  (clamp_highlight model, Cmd.none)

(* Theming *)
let with_theme theme model = { model with theme }
