open Text_buffer

type wrap_mode = [ `None | `Char | `Word ]
type viewport = { x : int; y : int; width : int; height : int }
type measure_info = { line_count : int; max_width : int }

module Virtual_line = Text_buffer.Virtual_line

let min_wrap_width = 1

let selection_equal (a : Text_buffer.Selection.t) (b : Text_buffer.Selection.t)
    =
  match (a, b) with
  | Text_buffer.Selection.None, Text_buffer.Selection.None -> true
  | Text_buffer.Selection.Linear l1, Text_buffer.Selection.Linear l2 ->
      l1.start = l2.start && l1.stop = l2.stop
      && Ansi.Style.equal l1.style l2.style
  | Text_buffer.Selection.Local l1, Text_buffer.Selection.Local l2 ->
      l1.anchor_x = l2.anchor_x && l1.anchor_y = l2.anchor_y
      && l1.focus_x = l2.focus_x && l1.focus_y = l2.focus_y
      && Ansi.Style.equal l1.style l2.style
  | _ -> false

type t = {
  buffer : Text_buffer.t;
  mutable wrap_mode : wrap_mode;
  mutable wrap_width : int option;
  mutable viewport : viewport option;
  mutable tab_indicator : int option;
  mutable tab_indicator_color : Ansi.Color.t option;
  mutable selection : Text_buffer.Selection.t;
  mutable local_selection : Text_buffer.Selection.local option;
  mutable virtual_lines : Virtual_line.t array;
  mutable line_starts : int array;
  mutable line_widths : int array;
  mutable line_first_vline : int array;
  mutable line_vline_counts : int array;
  mutable observed_version : int;
  mutable dirty : bool;
}

let create buffer =
  {
    buffer;
    wrap_mode = `Word;
    wrap_width = None;
    viewport = None;
    tab_indicator = None;
    tab_indicator_color = None;
    selection = Text_buffer.Selection.None;
    local_selection = None;
    virtual_lines = [||];
    line_starts = [||];
    line_widths = [||];
    line_first_vline = [||];
    line_vline_counts = [||];
    observed_version = -1;
    dirty = true;
  }

let buffer view = view.buffer
let wrap_mode view = view.wrap_mode
let wrap_width view = view.wrap_width
let viewport view = view.viewport
let tab_indicator view = view.tab_indicator
let tab_indicator_color view = view.tab_indicator_color
let selection view = view.selection
let mark_dirty view = view.dirty <- true

let normalize_wrap_width = function
  | Some w when w <= 0 -> Some min_wrap_width
  | other -> other

let buffer_wrap_mode = function
  | `None -> `Word
  | `Char -> `Char
  | `Word -> `Word

let effective_wrap_width view =
  match view.wrap_mode with
  | `None -> None
  | _ -> normalize_wrap_width view.wrap_width

let update_virtual_lines view =
  let version = Text_buffer.version view.buffer in
  if view.dirty || view.observed_version <> version then (
    let snapshot =
      Text_buffer.build_virtual_lines view.buffer
        ~wrap_mode:(buffer_wrap_mode view.wrap_mode)
        ~wrap_width:(effective_wrap_width view)
    in
    view.virtual_lines <- snapshot.lines;
    view.line_starts <- snapshot.line_starts;
    view.line_widths <- snapshot.line_widths;
    view.line_first_vline <- snapshot.line_first_vline;
    view.line_vline_counts <- snapshot.line_vline_counts;
    view.observed_version <- version;
    view.dirty <- false)

let viewport_vertical_bounds view total_len =
  match view.viewport with
  | None -> (0, total_len)
  | Some vp ->
      let start = max 0 (min vp.y total_len) in
      if vp.height <= 0 then (start, start)
      else (start, min total_len (start + vp.height))

let slice_within arr (start_idx, stop_idx) =
  let total = Array.length arr in
  if start_idx <= 0 && stop_idx >= total then arr
  else
    let len = max 0 (stop_idx - start_idx) in
    if len <= 0 then [||] else Array.sub arr start_idx len

let set_wrap_mode view mode =
  if view.wrap_mode <> mode then (
    view.wrap_mode <- mode;
    mark_dirty view)

let set_wrap_width view width =
  let normalized = normalize_wrap_width width in
  if view.wrap_width <> normalized then (
    view.wrap_width <- normalized;
    mark_dirty view)

let set_viewport view viewport =
  let changed =
    match (view.viewport, viewport) with
    | Some a, Some b ->
        a.x <> b.x || a.y <> b.y || a.width <> b.width || a.height <> b.height
    | None, None -> false
    | _ -> true
  in
  if changed then view.viewport <- viewport

let set_viewport_size view ~width ~height =
  let width = max 0 width in
  let height = max 0 height in
  let next =
    match view.viewport with
    | Some vp -> Some { vp with width; height }
    | None -> Some { x = 0; y = 0; width; height }
  in
  set_viewport view next

let set_tab_indicator view indicator =
  if view.tab_indicator <> indicator then view.tab_indicator <- indicator

let set_tab_indicator_color view color =
  if view.tab_indicator_color <> color then view.tab_indicator_color <- color

let ensure_lines view =
  update_virtual_lines view;
  view.virtual_lines

let virtual_lines view =
  let lines = ensure_lines view in
  let bounds = viewport_vertical_bounds view (Array.length lines) in
  slice_within lines bounds

let line_info view =
  update_virtual_lines view;
  let total = Array.length view.line_starts in
  let bounds = viewport_vertical_bounds view total in
  let starts = slice_within view.line_starts bounds in
  let widths = slice_within view.line_widths bounds in
  let max_width =
    match view.viewport with
    | Some _ ->
        Array.fold_left (fun acc v -> if v > acc then v else acc) 0 widths
    | None -> (Text_buffer.logical_line_info view.buffer).max_width
  in
  { starts = Array.copy starts; widths = Array.copy widths; max_width }

let logical_line_info view = Text_buffer.logical_line_info view.buffer

let measure_info view =
  update_virtual_lines view;
  let line_count = Array.length view.virtual_lines in
  let max_width =
    let acc = ref 0 in
    Array.iter (fun w -> if w > !acc then acc := w) view.line_widths;
    !acc
  in
  { line_count; max_width }

let measure_for_dimensions view ~width ~height:_ =
  let wrap_enabled = view.wrap_mode <> `None in
  let desired_wrap =
    if not wrap_enabled then None else if width <= 0 then None else Some width
  in
  let current = effective_wrap_width view in
  if current <> desired_wrap then set_wrap_width view desired_wrap;
  measure_info view

let find_visual_line_index view ~logical_row ~logical_col =
  update_virtual_lines view;
  let total_lines = Array.length view.line_first_vline in
  if total_lines = 0 then 0
  else
    let row =
      if logical_row < 0 then 0
      else if logical_row >= total_lines then total_lines - 1
      else logical_row
    in
    let first_idx = view.line_first_vline.(row) in
    let vline_count =
      if row < Array.length view.line_vline_counts then
        view.line_vline_counts.(row)
      else 0
    in
    if vline_count = 0 then first_idx
    else
      let lines = view.virtual_lines in
      let max_idx = Array.length lines in
      let last_candidate = min max_idx (first_idx + vline_count) - 1 in
      let target_col = max 0 logical_col in
      let rec search i =
        if i > last_candidate || i >= max_idx then last_candidate
        else
          let vline = lines.(i) in
          let start_col = vline.Virtual_line.source_col_offset in
          let end_col = start_col + vline.Virtual_line.width in
          let is_last = i = last_candidate in
          let end_check =
            if is_last then target_col <= end_col else target_col < end_col
          in
          if target_col >= start_col && end_check then i else search (i + 1)
      in
      let found = search first_idx in
      if found < 0 then first_idx else found

let viewport_x_offset view =
  match (view.viewport, view.wrap_mode) with Some vp, `None -> vp.x | _ -> 0

let viewport_y_offset view =
  match view.viewport with Some vp -> vp.y | None -> 0

let column_to_virtual_index view (vline : Virtual_line.t) column =
  let limit = vline.start_index + vline.length in
  let column = max 0 column in
  let chars = Text_buffer.drawing_chars view.buffer in
  let widths = Text_buffer.drawing_widths view.buffer in
  let tab_width = max 1 (Text_buffer.tab_width view.buffer) in
  let rec loop i acc =
    if i >= vline.length then limit
    else
      let idx = vline.start_index + i in
      let code = Bigarray.Array1.unsafe_get chars idx in
      let base_width = Bigarray.Array1.unsafe_get widths idx in
      let width =
        if code = 9 then
          let next = ((acc / tab_width) + 1) * tab_width in
          max 1 (next - acc)
        else base_width
      in
      if width <= 0 then loop (i + 1) acc
      else if column <= acc then idx
      else
        let next_acc = acc + width in
        if column < next_acc then idx else loop (i + 1) next_acc
  in
  loop 0 0

let clamp_line_index lines idx =
  let max_idx = Array.length lines - 1 in
  if max_idx < 0 then 0 else max 0 (min idx max_idx)

let position_to_index view ~x ~y =
  let lines = ensure_lines view in
  if Array.length lines = 0 then 0
  else
    let line_idx = clamp_line_index lines (y + viewport_y_offset view) in
    let vline = lines.(line_idx) in
    let line_width = vline.Virtual_line.width in
    let effective_x = x + viewport_x_offset view in
    let column =
      if effective_x < 0 then 0
      else if effective_x > line_width then line_width
      else effective_x
    in
    let idx = column_to_virtual_index view vline column in
    let limit = vline.start_index + vline.length in
    min idx limit

let set_selection view selection =
  view.selection <- selection;
  view.local_selection <-
    (match selection with
    | Text_buffer.Selection.Local local -> Some local
    | _ -> None)

let clear_selection view =
  view.selection <- Text_buffer.Selection.None;
  view.local_selection <- None

let reset_local_selection view = view.local_selection <- None

let calculate_multi_line_selection view (local : Text_buffer.Selection.local) =
  let lines = ensure_lines view in
  if Array.length lines = 0 then None
  else
    let x_offset = viewport_x_offset view in
    let y_offset = viewport_y_offset view in
    let adjusted_anchor_y = local.Text_buffer.Selection.anchor_y + y_offset in
    let adjusted_focus_y = local.Text_buffer.Selection.focus_y + y_offset in
    let startY = min adjusted_anchor_y adjusted_focus_y in
    let endY = max adjusted_anchor_y adjusted_focus_y in
    let selStartX, selEndX =
      if
        local.Text_buffer.Selection.anchor_y
        < local.Text_buffer.Selection.focus_y
        || local.Text_buffer.Selection.anchor_y
           = local.Text_buffer.Selection.focus_y
           && local.Text_buffer.Selection.anchor_x
              <= local.Text_buffer.Selection.focus_x
      then
        ( local.Text_buffer.Selection.anchor_x + x_offset,
          local.Text_buffer.Selection.focus_x + x_offset )
      else
        ( local.Text_buffer.Selection.focus_x + x_offset,
          local.Text_buffer.Selection.anchor_x + x_offset )
    in
    let selection_start = ref None in
    let selection_end = ref None in
    let clamp_within width value = max 0 (min value width) in
    Array.iteri
      (fun idx vline ->
        if idx >= startY && idx <= endY then
          let line_start = vline.Virtual_line.char_offset in
          let line_width = vline.Virtual_line.width in
          let line_end = line_start + line_width in
          if idx > startY && idx < endY then (
            if !selection_start = None then selection_start := Some line_start;
            selection_end := Some line_end)
          else if idx = startY && idx = endY then (
            let local_start_x = clamp_within line_width selStartX in
            let local_end_x = clamp_within line_width selEndX in
            if local_start_x <> local_end_x then (
              selection_start := Some (line_start + local_start_x);
              selection_end := Some (line_start + local_end_x)))
          else if idx = startY then (
            let local_start_x = clamp_within line_width selStartX in
            if local_start_x < line_width then (
              selection_start := Some (line_start + local_start_x);
              selection_end := Some line_end))
          else if idx = endY then (
            let local_end_x = clamp_within line_width selEndX in
            if !selection_start = None then selection_start := Some line_start;
            selection_end := Some (line_start + local_end_x)))
      lines;
    match (!selection_start, !selection_end) with
    | Some s, Some e when s < e -> Some (s, e)
    | _ -> None

let set_local_selection view ~anchor_x ~anchor_y ~focus_x ~focus_y ~style =
  let coords_changed =
    match view.local_selection with
    | Some prev ->
        prev.anchor_x <> anchor_x || prev.anchor_y <> anchor_y
        || prev.focus_x <> focus_x || prev.focus_y <> focus_y
        || not (Ansi.Style.equal prev.style style)
    | None -> true
  in
  let new_local =
    { Text_buffer.Selection.anchor_x; anchor_y; focus_x; focus_y; style }
  in
  view.local_selection <- Some new_local;
  let selection_changed = ref coords_changed in
  let char_selection = calculate_multi_line_selection view new_local in
  (match char_selection with
  | Some (start, stop) ->
      let linear =
        Text_buffer.Selection.Linear { start; stop; style = new_local.style }
      in
      if not (selection_equal view.selection linear) then (
        view.selection <- linear;
        selection_changed := true)
  | None ->
      if view.selection <> Text_buffer.Selection.None then (
        view.selection <- Text_buffer.Selection.None;
        selection_changed := true));
  !selection_changed

let selection_bounds view =
  match view.selection with
  | Text_buffer.Selection.None -> None
  | Text_buffer.Selection.Linear { Text_buffer.Selection.start; stop; _ }
    when start < stop ->
      Some (start, stop)
  | Text_buffer.Selection.Linear _ -> None
  | Text_buffer.Selection.Local local ->
      calculate_multi_line_selection view local

let selection_style view =
  match view.selection with
  | Text_buffer.Selection.None -> None
  | Text_buffer.Selection.Linear { Text_buffer.Selection.style; _ }
  | Text_buffer.Selection.Local { Text_buffer.Selection.style; _ } ->
      Some style

let get_selected_text view =
  match selection_bounds view with
  | None -> ""
  | Some (start, stop) -> Text_buffer.get_text_range view.buffer ~start ~stop
