module Grid = Matrix_grid
module Screen = Matrix_screen
module Style = Ansi.Style

let is_blank s = String.length (String.trim s) = 0
let clamp_nonneg n = max 0 n

type corner = [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
type section = { title : string option; rows : (string * string) list }

type theme = {
  background : Ansi.Color.t;
  foreground : Ansi.Color.t;
  accent : Ansi.Color.t;
}

type panel = {
  mutable sections : section list;
  theme : theme;
  padding : int;
  gap : int;
}

type line_style = [ `Title | `Row | `Spacer ]
type line = { text : string; style : line_style }
type layout = { lines : line list; width : int; height : int }

let default_theme =
  {
    background = Ansi.Color.of_rgba 20 20 40 200;
    foreground = Ansi.Color.of_rgb 230 230 230;
    accent = Ansi.Color.of_rgb 150 200 255;
  }

let panel ?(theme = default_theme) ?(padding = 1) ?(gap = 1) () =
  {
    sections = [];
    theme;
    padding = clamp_nonneg padding;
    gap = clamp_nonneg gap;
  }

let set_sections t sections = t.sections <- sections
let section ?title rows = { title; rows }

let normalize_title = function
  | None -> None
  | Some title when is_blank title -> None
  | some -> some

let format_row (label, value) =
  match (is_blank label, is_blank value) with
  | true, true -> ""
  | true, false -> value
  | false, true -> label
  | false, false -> Printf.sprintf "%s: %s" label value

let line_of_row row =
  let text = format_row row in
  if text = "" then None else Some { text; style = `Row }

let lines_of_section section =
  let rows = List.filter_map line_of_row section.rows in
  match normalize_title section.title with
  | None -> rows
  | Some title -> { text = title; style = `Title } :: rows

let blank_lines gap =
  if gap <= 0 then []
  else List.init gap (fun _ -> { text = ""; style = `Spacer })

let build_lines t =
  let gap_lines = blank_lines t.gap in
  let rec aux acc = function
    | [] -> List.rev acc
    | section :: rest ->
        let acc = List.rev_append (lines_of_section section) acc in
        let acc =
          if rest <> [] && gap_lines <> [] then List.rev_append gap_lines acc
          else acc
        in
        aux acc rest
  in
  aux [] t.sections

let layout t ~width_method =
  match build_lines t with
  | [] -> None
  | lines ->
      let content_width =
        List.fold_left
          (fun acc line ->
            match line.style with
            | `Spacer -> acc
            | _ ->
                (* Use Matrix_text.measure for correct Unicode width
                   calculation *)
                let w =
                  Matrix_text.measure ~width_method ~tab_width:2 line.text
                in
                max acc w)
          0 lines
      in
      let base_width = max 1 content_width in
      let width = base_width + (t.padding * 2) in
      let height = List.length lines + (t.padding * 2) in
      Some { lines; width; height }

let draw_layout t grid { lines; width; height } ~x ~y =
  Grid.fill_rect grid ~x ~y ~width ~height ~color:t.theme.background;
  let text_x = x + t.padding in
  let text_y = y + t.padding in
  let normal_style =
    Style.make ~fg:t.theme.foreground ~bg:t.theme.background ()
  in
  let title_style =
    Style.make ~fg:t.theme.accent ~bg:t.theme.background ~bold:true ()
  in
  List.iteri
    (fun idx line ->
      let row = text_y + idx in
      match line.style with
      | `Spacer -> ()
      | `Title ->
          Grid.draw_text grid ~x:text_x ~y:row ~style:title_style
            ~text:line.text
      | `Row ->
          Grid.draw_text grid ~x:text_x ~y:row ~style:normal_style
            ~text:line.text)
    lines

let render ?corner ?(offset = (0, 0)) t grid =
  match layout t ~width_method:(Grid.width_method grid) with
  | None -> ()
  | Some l -> (
      let dx, dy = offset in
      match corner with
      | None -> draw_layout t grid l ~x:dx ~y:dy
      | Some corner ->
          let grid_width = Grid.width grid in
          let grid_height = Grid.height grid in
          let base_x =
            match corner with
            | `Top_left | `Bottom_left -> 0
            | `Top_right | `Bottom_right -> max 0 (grid_width - l.width)
          in
          let base_y =
            match corner with
            | `Top_left | `Top_right -> 0
            | `Bottom_left | `Bottom_right -> max 0 (grid_height - l.height)
          in
          draw_layout t grid l ~x:(base_x + dx) ~y:(base_y + dy))

module Avg_ring = struct
  type t = {
    capacity : int;
    samples : float array;
    mutable count : int;
    mutable head : int;
    mutable sum : float; (* Running sum for O(1) average *)
  }

  let create capacity =
    if capacity <= 0 then
      { capacity = 0; samples = [||]; count = 0; head = 0; sum = 0. }
    else
      {
        capacity;
        samples = Array.make capacity 0.;
        count = 0;
        head = 0;
        sum = 0.;
      }

  let push t value =
    if t.capacity = 0 then ()
    else if t.count < t.capacity then (
      (* Buffer not yet full: just add the new value *)
      t.samples.(t.head) <- value;
      t.sum <- t.sum +. value;
      t.head <- (t.head + 1) mod t.capacity;
      t.count <- t.count + 1)
    else
      (* Buffer full: subtract old value, add new value *)
      let old_value = t.samples.(t.head) in
      t.samples.(t.head) <- value;
      t.sum <- t.sum -. old_value +. value;
      t.head <- (t.head + 1) mod t.capacity

  let average t = if t.count = 0 then None else Some (t.sum /. float t.count)
end

let fmt_ms value = Printf.sprintf "%.2f ms" value
let bytes_per_word = Sys.word_size / 8

let words_to_mb words =
  float_of_int words *. float bytes_per_word /. (1024. *. 1024.)

let words_to_mb_f words = words *. float bytes_per_word /. (1024. *. 1024.)
let fmt_mb value = Printf.sprintf "%.2f MB" value

let fmt_words_mb words =
  if words <= 0 then "0.00 MB" else fmt_mb (words_to_mb words)

type t = {
  corner : corner;
  panel : panel;
  render_ring : Avg_ring.t;
  interval_ring : Avg_ring.t;
  (* The last frame recorded: when it started and how long it took, and the
     time since the frame before it, 0. for the first. *)
  mutable render_ms : float;
  mutable interval_ms : float;
  mutable last_start : int option;
  (* The collector's counts at the last draw, to show what each frame
     allocated. *)
  mutable last_minor_words : float option;
  mutable last_major_words : float option;
}

let create ?(corner = `Bottom_right) ?(capacity = 120) () =
  {
    corner;
    panel = panel ();
    render_ring = Avg_ring.create capacity;
    interval_ring = Avg_ring.create capacity;
    render_ms = 0.;
    interval_ms = 0.;
    last_start = None;
    last_minor_words = None;
    last_major_words = None;
  }

let record t ~start ~ns =
  let render_ms = Float.of_int ns *. 1e-6 in
  let interval_ms =
    match t.last_start with
    | Some last when start > last -> Float.of_int (start - last) *. 1e-6
    | _ -> 0.
  in
  t.last_start <- Some start;
  t.render_ms <- render_ms;
  t.interval_ms <- interval_ms;
  Avg_ring.push t.render_ring render_ms;
  Avg_ring.push t.interval_ring interval_ms

let draw t ~(metrics : Screen.frame_metrics) ~(gc : Gc.stat) grid =
  let frame_ms = if t.interval_ms > 0. then t.interval_ms else t.render_ms in
  let fps = if frame_ms > 0. then 1000. /. frame_ms else 0. in
  let frame_rows =
    [
      ("frames", string_of_int metrics.frame_count);
      ("fps", Printf.sprintf "%.1f" fps);
      ("frame", fmt_ms frame_ms);
      ("draw", fmt_ms t.render_ms);
      ("cells", string_of_int metrics.cells);
      ("output", Printf.sprintf "%d bytes" metrics.bytes);
      ("cursor", if metrics.cursor_visible then "visible" else "hidden");
    ]
  in
  let perf_rows =
    List.concat
      [
        (match Avg_ring.average t.render_ring with
        | Some avg -> [ ("avg draw", fmt_ms avg) ]
        | None -> []);
        (match Avg_ring.average t.interval_ring with
        | Some avg_interval when avg_interval > 0. ->
            [
              ("avg frame", fmt_ms avg_interval);
              ("avg fps", Printf.sprintf "%.1f" (1000. /. avg_interval));
            ]
        | _ -> []);
      ]
  in
  let delta previous now =
    Option.map
      (fun previous -> words_to_mb_f (max 0. (now -. previous)))
      previous
  in
  let minor = delta t.last_minor_words gc.minor_words
  and major = delta t.last_major_words gc.major_words in
  t.last_minor_words <- Some gc.minor_words;
  t.last_major_words <- Some gc.major_words;
  let gc_rows =
    [
      ("heap", fmt_words_mb gc.heap_words); ("live", fmt_words_mb gc.live_words);
    ]
    @ List.map (fun mb -> ("minor Δ", fmt_mb mb)) (Option.to_list minor)
    @ List.map (fun mb -> ("major Δ", fmt_mb mb)) (Option.to_list major)
  in
  set_sections t.panel
    ([ section ~title:"frame" frame_rows ]
    @ (match perf_rows with [] -> [] | rows -> [ section ~title:"perf" rows ])
    @ [ section ~title:"gc" gc_rows ]);
  render ~corner:t.corner t.panel grid
