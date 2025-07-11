open Mosaic
module Ui = Mosaic.Ui

type column = { title : string; width : int }
type row = string list

type theme = {
  header_style : Style.t;
  cell_style : Style.t;
  selected_style : Style.t;
  border : Ui.border option;
}

let default_theme =
  {
    header_style = Style.(bold);
    cell_style = Style.empty;
    selected_style = Style.(bg (Index 240));
    border = None;
  }

type model = {
  columns : column list;
  rows : row list;
  cursor : int;
  viewport_offset : int;
  height : int;
  focused : bool;
  theme : theme;
}

type msg = MoveUp | MoveDown | PageUp | PageDown | Home | End | Focus | Blur

let clamp value min_val max_val = max min_val (min max_val value)

let init ?(columns = []) ?(rows = []) ?(height = 10) ?(focused = false) () =
  let model =
    {
      columns;
      rows;
      cursor = 0;
      viewport_offset = 0;
      height;
      focused;
      theme = default_theme;
    }
  in
  (model, Cmd.none)

(* Accessors *)

let selected_row model = List.nth_opt model.rows model.cursor
let cursor model = model.cursor
let rows model = model.rows
let columns model = model.columns
let is_focused model = model.focused
let row_count model = List.length model.rows

let visible_rows model =
  let start_idx = model.viewport_offset in
  let end_idx = min (start_idx + model.height) (List.length model.rows) in
  let rec take n lst =
    match (n, lst) with 0, _ | _, [] -> [] | n, h :: t -> h :: take (n - 1) t
  in
  let rec drop n lst =
    match (n, lst) with
    | 0, _ -> lst
    | _, [] -> []
    | n, _ :: t -> drop (n - 1) t
  in
  model.rows |> drop start_idx |> take (end_idx - start_idx)

(* Actions *)

let ensure_visible model =
  let cursor = model.cursor in
  let viewport_offset =
    if cursor < model.viewport_offset then cursor
    else if cursor >= model.viewport_offset + model.height then
      cursor - model.height + 1
    else model.viewport_offset
  in
  {
    model with
    viewport_offset =
      clamp viewport_offset 0 (max 0 (List.length model.rows - model.height));
  }

let focus model = ({ model with focused = true }, Cmd.none)
let blur model = { model with focused = false }
let request_focus model = (model, Cmd.msg Focus)
let request_blur model = (model, Cmd.msg Blur)
let set_columns columns model = { model with columns }

let set_rows rows model =
  let cursor = clamp model.cursor 0 (max 0 (List.length rows - 1)) in
  { model with rows; cursor } |> ensure_visible

let set_cursor cursor model =
  let cursor = clamp cursor 0 (max 0 (List.length model.rows - 1)) in
  { model with cursor } |> ensure_visible

let move_up n model =
  let cursor = max 0 (model.cursor - n) in
  { model with cursor } |> ensure_visible

let move_down n model =
  let cursor = min (List.length model.rows - 1) (model.cursor + n) in
  { model with cursor } |> ensure_visible

let go_to_start model = { model with cursor = 0; viewport_offset = 0 }

let go_to_end model =
  let cursor = max 0 (List.length model.rows - 1) in
  let viewport_offset = max 0 (cursor - model.height + 1) in
  { model with cursor; viewport_offset }

let set_height height model =
  { model with height = max 1 height } |> ensure_visible

let with_theme theme model = { model with theme }

(* Update *)

let update msg model =
  match msg with
  | MoveUp -> (move_up 1 model, Cmd.none)
  | MoveDown -> (move_down 1 model, Cmd.none)
  | PageUp -> (move_up model.height model, Cmd.none)
  | PageDown -> (move_down model.height model, Cmd.none)
  | Home -> (go_to_start model, Cmd.none)
  | End -> (go_to_end model, Cmd.none)
  | Focus -> focus model
  | Blur -> (blur model, Cmd.none)

(* View *)

let truncate_string str max_width =
  Render.truncate_string_with_ellipsis str max_width "..."

let pad_string = Render.pad_string

let render_header model =
  if model.columns = [] then []
  else
    let cells =
      List.map
        (fun col ->
          let text =
            pad_string (truncate_string col.title col.width) col.width
          in
          Ui.text ~style:model.theme.header_style text)
        model.columns
    in
    [ Ui.hbox cells ]

let render_row model row_idx row =
  let is_selected = row_idx + model.viewport_offset = model.cursor in
  let style =
    if is_selected then model.theme.selected_style else model.theme.cell_style
  in

  let cells =
    List.mapi
      (fun col_idx cell ->
        match List.nth_opt model.columns col_idx with
        | Some col ->
            let text = pad_string (truncate_string cell col.width) col.width in
            Ui.text ~style text
        | None -> Ui.text ~style cell)
      row
  in

  Ui.hbox cells

let render_empty_state model =
  let width = List.fold_left (fun acc col -> acc + col.width) 0 model.columns in
  let text = if width > 0 then pad_string "No rows" width else "No rows" in
  [ Ui.text ~style:Style.(fg (gray 8)) text ]

let view model =
  let header = render_header model in
  let visible = visible_rows model in
  let body =
    if visible = [] then render_empty_state model
    else List.mapi (render_row model) visible
  in

  (* Pad to height if needed *)
  let current_rows = List.length body in
  let padding =
    if current_rows < model.height then
      List.init (model.height - current_rows) (fun _ -> Ui.text " ")
    else []
  in

  let all_rows = header @ body @ padding in

  match model.theme.border with
  | Some border -> Ui.vbox ~border all_rows
  | None -> Ui.vbox all_rows

(* Subscriptions *)

let subscriptions model =
  if model.focused then
    Sub.batch
      [
        Sub.on_up MoveUp;
        Sub.on_down MoveDown;
        Sub.on_page_up PageUp;
        Sub.on_page_down PageDown;
        Sub.on_home Home;
        Sub.on_end End;
        Sub.on_char 'k' MoveUp;
        Sub.on_char 'j' MoveDown;
        Sub.on_char 'g' Home;
        Sub.on_char ~shift:true 'G' End;
      ]
  else Sub.none

(* Redefine component with actual functions *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()
