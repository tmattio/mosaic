(** Implementation of the multiline text area component *)

open Mosaic

(* Theme configuration *)
type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  error_style : Style.t;
  placeholder_style : Style.t;
  line_numbers_style : Style.t;
  cursor_style : Style.t;
}

let default_theme =
  {
    focused_style = Style.(fg (Index 6));
    blurred_style = Style.(fg (Index 8));
    error_style = Style.(fg Red);
    placeholder_style = Style.(fg (Index 8));
    line_numbers_style = Style.(fg (Index 8));
    cursor_style = Style.reverse;
  }

(* Model *)
type model = {
  (* Content *)
  lines : string list;
  cursor_line : int;
  cursor_col : int;
  (* Configuration *)
  placeholder : string;
  height : int;
  width : int;
  word_wrap : bool;
  validate : (string -> (unit, string) result) option;
  (* State *)
  is_focused : bool;
  error : string option;
  scroll_offset : int;
  (* Theme *)
  theme : theme;
}

(* Messages *)
type msg = Key of key_event | Focus | Blur

(* Helpers *)
let string_of_lines lines = String.concat "\n" lines
let lines_of_string s = if s = "" then [ "" ] else String.split_on_char '\n' s

let validate_value model =
  match model.validate with
  | Some f -> (
      match f (string_of_lines model.lines) with
      | Ok () -> { model with error = None }
      | Error err -> { model with error = Some err })
  | None -> model

let ensure_cursor_visible model =
  let visible_start = model.scroll_offset in
  let visible_end = model.scroll_offset + model.height - 1 in
  if model.cursor_line < visible_start then
    { model with scroll_offset = model.cursor_line }
  else if model.cursor_line > visible_end then
    { model with scroll_offset = model.cursor_line - model.height + 1 }
  else model

let clamp_cursor model =
  let line_count = List.length model.lines in
  let cursor_line = max 0 (min (line_count - 1) model.cursor_line) in
  let current_line = List.nth model.lines cursor_line in
  let cursor_col = max 0 (min (String.length current_line) model.cursor_col) in
  { model with cursor_line; cursor_col }

(* Initialization *)
let init ?(placeholder = "") ?(initial_value = "") ?(height = 5) ?(width = 60)
    ?(word_wrap = true) ?validate () =
  let lines = lines_of_string initial_value in
  let model =
    {
      lines;
      cursor_line = 0;
      cursor_col = 0;
      placeholder;
      height;
      width;
      word_wrap;
      validate;
      is_focused = false;
      error = None;
      scroll_offset = 0;
      theme = default_theme;
    }
  in
  let model = validate_value model in
  (model, Cmd.none)

(* Update *)
let update msg model =
  match msg with
  | Key { key; modifier } -> (
      match key with
      | Char c
        when Uchar.to_int c >= 32
             && Uchar.to_int c < 127
             && (not modifier.ctrl) && not modifier.alt ->
          let char = String.make 1 (Uchar.to_char c) in
          let current_line = List.nth model.lines model.cursor_line in
          let before = String.sub current_line 0 model.cursor_col in
          let after =
            String.sub current_line model.cursor_col
              (String.length current_line - model.cursor_col)
          in
          let new_line = before ^ char ^ after in
          let lines =
            List.mapi
              (fun i line -> if i = model.cursor_line then new_line else line)
              model.lines
          in
          let model = { model with lines; cursor_col = model.cursor_col + 1 } in
          (validate_value model |> ensure_cursor_visible, Cmd.none)
      | Enter ->
          let current_line = List.nth model.lines model.cursor_line in
          let before = String.sub current_line 0 model.cursor_col in
          let after =
            String.sub current_line model.cursor_col
              (String.length current_line - model.cursor_col)
          in
          let lines =
            let rec insert_at n lst new_items =
              match lst with
              | [] -> new_items
              | h :: t ->
                  if n = 0 then new_items @ lst
                  else h :: insert_at (n - 1) t new_items
            in
            List.mapi
              (fun i line -> if i = model.cursor_line then before else line)
              model.lines
            |> fun lines -> insert_at (model.cursor_line + 1) lines [ after ]
          in
          let model =
            {
              model with
              lines;
              cursor_line = model.cursor_line + 1;
              cursor_col = 0;
            }
          in
          (validate_value model |> ensure_cursor_visible, Cmd.none)
      | Backspace when model.cursor_col > 0 || model.cursor_line > 0 ->
          if model.cursor_col > 0 then
            (* Delete character in current line *)
            let current_line = List.nth model.lines model.cursor_line in
            let before = String.sub current_line 0 (model.cursor_col - 1) in
            let after =
              String.sub current_line model.cursor_col
                (String.length current_line - model.cursor_col)
            in
            let new_line = before ^ after in
            let lines =
              List.mapi
                (fun i line -> if i = model.cursor_line then new_line else line)
                model.lines
            in
            let model =
              { model with lines; cursor_col = model.cursor_col - 1 }
            in
            (validate_value model, Cmd.none)
          else
            (* Join with previous line *)
            let prev_line = List.nth model.lines (model.cursor_line - 1) in
            let current_line = List.nth model.lines model.cursor_line in
            let joined = prev_line ^ current_line in
            let lines =
              List.filteri (fun i _ -> i <> model.cursor_line) model.lines
            in
            let lines =
              List.mapi
                (fun i line ->
                  if i = model.cursor_line - 1 then joined else line)
                lines
            in
            let model =
              {
                model with
                lines;
                cursor_line = model.cursor_line - 1;
                cursor_col = String.length prev_line;
              }
            in
            (validate_value model |> ensure_cursor_visible, Cmd.none)
      | Delete when model.cursor_line < List.length model.lines ->
          let current_line = List.nth model.lines model.cursor_line in
          if model.cursor_col < String.length current_line then
            (* Delete character in current line *)
            let before = String.sub current_line 0 model.cursor_col in
            let after =
              if model.cursor_col < String.length current_line - 1 then
                String.sub current_line (model.cursor_col + 1)
                  (String.length current_line - model.cursor_col - 1)
              else ""
            in
            let new_line = before ^ after in
            let lines =
              List.mapi
                (fun i line -> if i = model.cursor_line then new_line else line)
                model.lines
            in
            (validate_value { model with lines }, Cmd.none)
          else if model.cursor_line < List.length model.lines - 1 then
            (* Join with next line *)
            let next_line = List.nth model.lines (model.cursor_line + 1) in
            let joined = current_line ^ next_line in
            let lines =
              List.filteri (fun i _ -> i <> model.cursor_line + 1) model.lines
            in
            let lines =
              List.mapi
                (fun i line -> if i = model.cursor_line then joined else line)
                lines
            in
            (validate_value { model with lines }, Cmd.none)
          else (model, Cmd.none)
      | Up when model.cursor_line > 0 ->
          let model = { model with cursor_line = model.cursor_line - 1 } in
          (clamp_cursor model |> ensure_cursor_visible, Cmd.none)
      | Down when model.cursor_line < List.length model.lines - 1 ->
          let model = { model with cursor_line = model.cursor_line + 1 } in
          (clamp_cursor model |> ensure_cursor_visible, Cmd.none)
      | Left when model.cursor_col > 0 ->
          ({ model with cursor_col = model.cursor_col - 1 }, Cmd.none)
      | Left when model.cursor_line > 0 ->
          let prev_line = List.nth model.lines (model.cursor_line - 1) in
          let model =
            {
              model with
              cursor_line = model.cursor_line - 1;
              cursor_col = String.length prev_line;
            }
          in
          (ensure_cursor_visible model, Cmd.none)
      | Right ->
          let current_line = List.nth model.lines model.cursor_line in
          if model.cursor_col < String.length current_line then
            ({ model with cursor_col = model.cursor_col + 1 }, Cmd.none)
          else if model.cursor_line < List.length model.lines - 1 then
            let model =
              { model with cursor_line = model.cursor_line + 1; cursor_col = 0 }
            in
            (ensure_cursor_visible model, Cmd.none)
          else (model, Cmd.none)
      | Home -> ({ model with cursor_col = 0 }, Cmd.none)
      | End ->
          let current_line = List.nth model.lines model.cursor_line in
          ({ model with cursor_col = String.length current_line }, Cmd.none)
      | Page_up ->
          let model =
            {
              model with
              cursor_line = max 0 (model.cursor_line - model.height);
              scroll_offset = max 0 (model.scroll_offset - model.height);
            }
          in
          (clamp_cursor model, Cmd.none)
      | Page_down ->
          let max_line = List.length model.lines - 1 in
          let model =
            {
              model with
              cursor_line = min max_line (model.cursor_line + model.height);
              scroll_offset =
                min
                  (max 0 (max_line - model.height + 1))
                  (model.scroll_offset + model.height);
            }
          in
          (clamp_cursor model, Cmd.none)
      | _ -> (model, Cmd.none))
  | Focus -> ({ model with is_focused = true }, Cmd.none)
  | Blur -> ({ model with is_focused = false }, Cmd.none)

(* Word wrapping helper *)
let wrap_line line width =
  if width <= 0 then [ line ]
  else
    let rec wrap acc current_line pos =
      if pos >= String.length line then
        if current_line = "" then List.rev acc
        else List.rev (current_line :: acc)
      else if String.length current_line >= width then
        wrap (current_line :: acc) "" pos
      else
        let next_char = String.sub line pos 1 in
        wrap acc (current_line ^ next_char) (pos + 1)
    in
    wrap [] "" 0

(* View *)
let view model =
  let open Ui in
  (* Text area content *)
  let content =
    if model.lines = [ "" ] && not model.is_focused then
      [ text ~style:model.theme.placeholder_style model.placeholder ]
    else
      let wrapped_lines =
        if model.word_wrap then
          List.concat_map (fun line -> wrap_line line model.width) model.lines
        else model.lines
      in
      let visible_lines =
        let start = model.scroll_offset in
        let end_ = min (List.length wrapped_lines) (start + model.height) in
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
        wrapped_lines |> drop start |> take (end_ - start)
      in
      List.mapi
        (fun i line ->
          let actual_line_num = model.scroll_offset + i in
          let is_cursor_line = actual_line_num = model.cursor_line in

          (* Apply cursor if on this line and focused *)
          let line_content =
            if is_cursor_line && model.is_focused then
              let col = min model.cursor_col (String.length line) in
              let before = String.sub line 0 col in
              let at_cursor =
                if col < String.length line then String.sub line col 1 else " "
              in
              let after =
                if col < String.length line - 1 then
                  String.sub line (col + 1) (String.length line - col - 1)
                else ""
              in
              hbox
                [
                  text before;
                  text ~style:model.theme.cursor_style at_cursor;
                  text after;
                ]
            else text line
          in

          hbox ~gap:2
            [
              text ~style:model.theme.line_numbers_style
                (Printf.sprintf "%3d" (actual_line_num + 1));
              line_content;
            ])
        visible_lines
  in

  let border_color =
    if model.is_focused then Style.Index 6 else Style.Index 8
  in

  let text_area =
    vbox ~padding:(padding_all 1)
      ~border:(border ~style:Solid ~color:border_color ())
      ~width:model.width ~height:(model.height + 2) content
  in

  (* Error message below text area *)
  let error_elem =
    match model.error with
    | Some err -> [ text ~style:model.theme.error_style err ]
    | None -> []
  in

  (* Layout *)
  vbox ([ text_area ] @ error_elem)

(* Subscriptions *)
let subscriptions model =
  if model.is_focused then Sub.keyboard (fun k -> Key k) else Sub.none

(* Component export *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()

(* Accessors *)
let value model = string_of_lines model.lines
let lines model = model.lines
let line_count model = List.length model.lines
let cursor_position model = (model.cursor_line + 1, model.cursor_col + 1)
let is_focused model = model.is_focused

let is_valid model =
  match model.validate with
  | Some f -> ( match f (value model) with Ok () -> true | Error _ -> false)
  | None -> true

let error model = model.error

(* Actions *)
let focus model = ({ model with is_focused = true }, Cmd.msg Focus)
let blur model = ({ model with is_focused = false }, Cmd.msg Blur)

let set_value value model =
  let lines = lines_of_string value in
  let model = { model with lines; cursor_line = 0; cursor_col = 0 } in
  validate_value model |> clamp_cursor

let clear model = set_value "" model

let insert_at_cursor text model =
  let current_line = List.nth model.lines model.cursor_line in
  let before = String.sub current_line 0 model.cursor_col in
  let after =
    String.sub current_line model.cursor_col
      (String.length current_line - model.cursor_col)
  in
  let new_line = before ^ text ^ after in
  let lines =
    List.mapi
      (fun i line -> if i = model.cursor_line then new_line else line)
      model.lines
  in
  { model with lines; cursor_col = model.cursor_col + String.length text }
  |> validate_value

let go_to_line line_num model =
  let line_idx = max 0 (min (List.length model.lines - 1) (line_num - 1)) in
  { model with cursor_line = line_idx; cursor_col = 0 } |> ensure_cursor_visible

(* Theming *)
let with_theme theme model = { model with theme }

(* Re-export for component interface *)
let update = update
let view = view
let subscriptions = subscriptions
