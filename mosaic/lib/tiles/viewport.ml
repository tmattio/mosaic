open Mosaic
module Ui = Mosaic.Ui

type content = Text of string | Elements of Ui.element list

type model = {
  content : content;
  width : int;
  height : int;
  x_offset : int;
  y_offset : int;
  wrap_text : bool;
  horizontal_scroll : bool;
  (* Cached computations *)
  lines : string list; (* For text content *)
  total_height : int;
  max_line_width : int;
}

type msg =
  | Scroll_up of int
  | Scroll_down of int
  | Scroll_left of int
  | Scroll_right of int
  | Page_up
  | Page_down
  | Half_page_up
  | Half_page_down
  | Home
  | End
  | Scroll_to_left
  | Scroll_to_right

(* Helper functions *)

let lines_of_string s =
  let lines = String.split_on_char '\n' s in
  if lines = [] then [ "" ] else lines

let wrap_line line width =
  if width <= 0 || String.length line <= width then [ line ]
  else
    let rec wrap acc current pos =
      if pos >= String.length line then
        List.rev (if current = "" then acc else current :: acc)
      else
        let remaining = String.length line - pos in
        let chunk_size = min width remaining in
        let chunk = String.sub line pos chunk_size in
        wrap (chunk :: acc) "" (pos + chunk_size)
    in
    wrap [] "" 0

let process_text_content text width wrap =
  let raw_lines = lines_of_string text in
  let lines =
    if wrap then List.concat_map (fun line -> wrap_line line width) raw_lines
    else raw_lines
  in
  let max_width =
    List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
  in
  (lines, max_width)

let clamp value min_val max_val = max min_val (min max_val value)

(* Initialization *)

let init ?(content = Text "") ?(width = 80) ?(height = 10) ?(wrap_text = true)
    ?(horizontal_scroll = false) () =
  let lines, max_line_width =
    match content with
    | Text text -> process_text_content text width wrap_text
    | Elements _ -> ([], 0)
  in
  let total_height =
    match content with
    | Text _ -> List.length lines
    | Elements elems -> List.length elems
  in
  let model =
    {
      content;
      width = max 1 width;
      height = max 1 height;
      x_offset = 0;
      y_offset = 0;
      wrap_text;
      horizontal_scroll;
      lines;
      total_height;
      max_line_width;
    }
  in
  (model, Cmd.none)

(* Accessors *)

let content model = model.content
let scroll_position model = (model.x_offset, model.y_offset)
let dimensions model = (model.width, model.height)
let at_top model = model.y_offset <= 0
let at_bottom model = model.y_offset >= max 0 (model.total_height - model.height)
let at_left model = model.x_offset <= 0

let at_right model =
  (not model.horizontal_scroll)
  || model.x_offset >= max 0 (model.max_line_width - model.width)

let visible_lines model =
  match model.content with
  | Text _ ->
      let start_idx = model.y_offset in
      let end_idx = min (start_idx + model.height) (List.length model.lines) in
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
      model.lines |> drop start_idx |> take (end_idx - start_idx)
  | Elements _ -> []

let total_lines model = model.total_height

let scroll_percentage model =
  let h_percent =
    if model.max_line_width <= model.width then 1.0
    else
      float_of_int model.x_offset
      /. float_of_int (model.max_line_width - model.width)
  in
  let v_percent =
    if model.total_height <= model.height then 1.0
    else
      float_of_int model.y_offset
      /. float_of_int (model.total_height - model.height)
  in
  (clamp h_percent 0.0 1.0, clamp v_percent 0.0 1.0)

(* Actions *)

let update_content model =
  match model.content with
  | Text text ->
      let lines, max_line_width =
        process_text_content text model.width model.wrap_text
      in
      { model with lines; max_line_width; total_height = List.length lines }
  | Elements elems ->
      {
        model with
        total_height = List.length elems;
        lines = [];
        max_line_width = 0;
      }

let set_content content model = { model with content } |> update_content

let set_dimensions width height model =
  let model = { model with width = max 1 width; height = max 1 height } in
  let model = update_content model in
  (* Adjust scroll position if needed *)
  let max_y = max 0 (model.total_height - model.height) in
  let max_x =
    if model.horizontal_scroll then max 0 (model.max_line_width - model.width)
    else 0
  in
  {
    model with
    y_offset = clamp model.y_offset 0 max_y;
    x_offset = clamp model.x_offset 0 max_x;
  }

let scroll_to x y model =
  let max_y = max 0 (model.total_height - model.height) in
  let max_x =
    if model.horizontal_scroll then max 0 (model.max_line_width - model.width)
    else 0
  in
  { model with x_offset = clamp x 0 max_x; y_offset = clamp y 0 max_y }

let scroll_up n model = scroll_to model.x_offset (model.y_offset - n) model
let scroll_down n model = scroll_to model.x_offset (model.y_offset + n) model

let scroll_left n model =
  if model.horizontal_scroll then
    scroll_to (model.x_offset - n) model.y_offset model
  else model

let scroll_right n model =
  if model.horizontal_scroll then
    scroll_to (model.x_offset + n) model.y_offset model
  else model

let page_up model = scroll_up model.height model
let page_down model = scroll_down model.height model
let half_page_up model = scroll_up (model.height / 2) model
let half_page_down model = scroll_down (model.height / 2) model
let go_to_top model = scroll_to model.x_offset 0 model

let go_to_bottom model =
  let max_y = max 0 (model.total_height - model.height) in
  scroll_to model.x_offset max_y model

let go_to_left model = scroll_to 0 model.y_offset model

let go_to_right model =
  if model.horizontal_scroll then
    let max_x = max 0 (model.max_line_width - model.width) in
    scroll_to max_x model.y_offset model
  else model

let set_wrap_text wrap model =
  let model = { model with wrap_text = wrap } in
  update_content model

let set_horizontal_scroll enabled model =
  let model = { model with horizontal_scroll = enabled } in
  if not enabled then { model with x_offset = 0 } else model

let scroll_to_left model = (model, Cmd.msg Scroll_to_left)
let scroll_to_right model = (model, Cmd.msg Scroll_to_right)

(* Update *)

let update msg model =
  match msg with
  | Scroll_up n -> (scroll_up n model, Cmd.none)
  | Scroll_down n -> (scroll_down n model, Cmd.none)
  | Scroll_left n -> (scroll_left n model, Cmd.none)
  | Scroll_right n -> (scroll_right n model, Cmd.none)
  | Page_up -> (page_up model, Cmd.none)
  | Page_down -> (page_down model, Cmd.none)
  | Half_page_up -> (half_page_up model, Cmd.none)
  | Half_page_down -> (half_page_down model, Cmd.none)
  | Home -> (go_to_top model, Cmd.none)
  | End -> (go_to_bottom model, Cmd.none)
  | Scroll_to_left -> (go_to_left model, Cmd.none)
  | Scroll_to_right -> (go_to_right model, Cmd.none)

(* View *)

let truncate_string str start_col width =
  (* UTF-8 safe string truncation with starting column offset *)
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let buf = Buffer.create (String.length str) in
  let rec skip_cols cols =
    if cols <= 0 then true
    else
      match Uutf.decode decoder with
      | `Uchar u ->
          let w =
            match Uucp.Break.tty_width_hint u with
            | -1 -> 1 (* Control characters *)
            | n -> n
          in
          skip_cols (cols - w)
      | `End | `Malformed _ | `Await -> false
  in
  let rec take_width w =
    if w <= 0 then ()
    else
      match Uutf.decode decoder with
      | `Uchar u ->
          let char_width =
            match Uucp.Break.tty_width_hint u with
            | -1 -> 1 (* Control characters *)
            | n -> n
          in
          if char_width <= w then (
            Uutf.Buffer.add_utf_8 buf u;
            take_width (w - char_width))
          else ()
      | `End | `Malformed _ | `Await -> ()
  in
  if skip_cols start_col then take_width width;
  Buffer.contents buf

let pad_string = Render.pad_string

let view model =
  match model.content with
  | Text _ ->
      let visible = visible_lines model in
      let rendered_lines =
        List.map
          (fun line ->
            if model.horizontal_scroll && model.x_offset > 0 then
              let truncated = truncate_string line model.x_offset model.width in
              pad_string truncated model.width
            else
              let truncated = truncate_string line 0 model.width in
              pad_string truncated model.width)
          visible
      in

      (* Pad to height if needed *)
      let current_lines = List.length rendered_lines in
      let padding =
        if current_lines < model.height then
          List.init (model.height - current_lines) (fun _ ->
              String.make model.width ' ')
        else []
      in

      let all_lines = rendered_lines @ padding in
      Ui.vbox (List.map (fun line -> Ui.text line) all_lines)
  | Elements elems ->
      let start_idx = model.y_offset in
      let end_idx = min (start_idx + model.height) (List.length elems) in
      let visible_elems =
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
        elems |> drop start_idx |> take (end_idx - start_idx)
      in

      (* Pad if needed *)
      let current_count = List.length visible_elems in
      let padding =
        if current_count < model.height then
          List.init (model.height - current_count) (fun _ -> Ui.text " ")
        else []
      in

      Ui.vbox (visible_elems @ padding)

(* Subscriptions *)

let subscriptions model =
  let base_subs =
    [
      Sub.on_up (Scroll_up 1);
      Sub.on_down (Scroll_down 1);
      Sub.on_page_up Page_up;
      Sub.on_page_down Page_down;
      Sub.on_home Home;
      Sub.on_end End;
      Sub.on_char ~ctrl:true 'u' Half_page_up;
      Sub.on_char ~ctrl:true 'd' Half_page_down;
    ]
  in
  let horizontal_subs =
    if model.horizontal_scroll then
      [
        Sub.on_left (Scroll_left 1);
        Sub.on_right (Scroll_right 1);
        Sub.on_char 'h' (Scroll_left 1);
        Sub.on_char 'l' (Scroll_right 1);
      ]
    else []
  in
  Sub.batch (base_subs @ horizontal_subs)

(* Redefine component with actual functions *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()
