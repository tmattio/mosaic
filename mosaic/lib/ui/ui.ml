module Style = Style
module Border = Border
module Theme = Theme
module Attr = Attr
module Layout_snapshot = Layout_snapshot
module Key = Key
include Element

type element = Element.t

(* String utilities - defined early as needed by measure function *)
let measure_string str = Renderer.measure_string str

let truncate_string_with_ellipsis str max_width suffix =
  Renderer.truncate_string_with_ellipsis str max_width suffix

let pad_string str width = Renderer.pad_string str width

(* Default measure function for text nodes *)
let default_measure_fn known_dimensions available_space _node_id context _style
    =
  match context with
  | Some (Renderable.Text { content; tab_width; wrap; _ }) ->
      (* Use unified text measurement function *)
      Renderer.measure_text_content ~known_dimensions ~available_space
        ~tab_width ~wrap content
  | _ ->
      (* Non-text nodes don't need measuring *)
      { width = 0.0; height = 0.0 }

module Progress_bar = Progress_bar
module Panel = Panel
module Spinner = Spinner
module Table = Table
module Tree = Tree

let panel = Panel.panel
let progress_bar = Progress_bar.progress_bar
let spinner = Spinner.spinner

let table ?title ?caption ?columns ?rows ?box_style ?safe_box ?padding
    ?collapse_padding ?pad_edge ?expand ?show_header ?show_footer ?show_edge
    ?show_lines ?leading ?style ?row_styles ?header_style ?footer_style
    ?border_style ?title_style ?caption_style ?title_justify ?caption_justify
    ?width ?min_width () =
  Table.table ?title ?caption ?columns ?rows ?box_style ?safe_box ?padding
    ?collapse_padding ?pad_edge ?expand ?show_header ?show_footer ?show_edge
    ?show_lines ?leading ?style ?row_styles ?header_style ?footer_style
    ?border_style ?title_style ?caption_style ?title_justify ?caption_justify
    ?width ?min_width ()

let tree ?style ?guide_style ?guides ?hide_root ?expanded node =
  Tree.tree ?style ?guide_style ?guides ?hide_root ?expanded node

module Canvas = struct
  type t = {
    plot : x:int -> y:int -> ?style:Style.t -> string -> unit;
    draw_line :
      x1:int ->
      y1:int ->
      x2:int ->
      y2:int ->
      ?style:Style.t ->
      ?kind:[ `Line | `Braille ] ->
      unit ->
      unit;
    draw_box :
      x:int ->
      y:int ->
      width:int ->
      height:int ->
      ?style:Style.t ->
      ?border:Border.t ->
      unit ->
      unit;
  }

  let create ?width ?height ?min_width ?min_height ?max_width ?max_height
      ?padding ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border
      ?border_style draw_fn =
    Element.canvas ?width ?height ?min_width ?min_height ?max_width ?max_height
      ?padding ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border
      ?border_style (fun ~width ~height plot_fn ->
        let plot ~x ~y ?style str = plot_fn ~x ~y ?style str in

        let draw_line ~x1 ~y1 ~x2 ~y2 ?(style = Style.empty) ?(kind = `Line) ()
            =
          (* Simple line drawing using Bresenham's algorithm *)
          let dx = abs (x2 - x1) in
          let dy = abs (y2 - y1) in
          let sx = if x1 < x2 then 1 else -1 in
          let sy = if y1 < y2 then 1 else -1 in

          match kind with
          | `Line ->
              let rec draw x y err =
                (* Choose appropriate character based on direction *)
                let glyph =
                  if dx = 0 then "│" (* Vertical line *)
                  else if dy = 0 then "─" (* Horizontal line *)
                  else if (x2 - x1) * (y2 - y1) > 0 then "╲"
                    (* Diagonal down-right or up-left - U+2572 *)
                  else "╱" (* Diagonal down-left or up-right - U+2571 *)
                in
                plot_fn ~x ~y ~style glyph;
                if x = x2 && y = y2 then ()
                else
                  let e2 = 2 * err in
                  let x', err' =
                    if e2 > -dy then (x + sx, err - dy) else (x, err)
                  in
                  let y', err'' =
                    if e2 < dx then (y + sy, err' + dx) else (y, err')
                  in
                  draw x' y' err''
              in
              draw x1 y1 (dx - dy)
          | `Braille ->
              (* Braille pattern drawing *)
              (* Braille characters use a 2x4 grid of dots *)
              (* Each cell can represent 8 possible dot positions *)
              let braille_buffer = Hashtbl.create 100 in

              (* Draw line using Bresenham algorithm and store in buffer *)
              let rec draw x y err =
                (* Calculate braille cell position and bit position *)
                let cell_x = x / 2 in
                let cell_y = y / 4 in
                let bit_x = x mod 2 in
                let bit_y = y mod 4 in

                (* Braille dot mapping: *)
                (* 0 3 *)
                (* 1 4 *)
                (* 2 5 *)
                (* 6 7 *)
                let bit_pos =
                  match (bit_x, bit_y) with
                  | 0, 0 -> 0
                  | 0, 1 -> 1
                  | 0, 2 -> 2
                  | 0, 3 -> 6
                  | 1, 0 -> 3
                  | 1, 1 -> 4
                  | 1, 2 -> 5
                  | 1, 3 -> 7
                  | _ -> 0
                in

                let key = (cell_x, cell_y) in
                let current =
                  try Hashtbl.find braille_buffer key with Not_found -> 0
                in
                Hashtbl.replace braille_buffer key (current lor (1 lsl bit_pos));

                if x = x2 && y = y2 then ()
                else
                  let e2 = 2 * err in
                  let x', err' =
                    if e2 > -dy then (x + sx, err - dy) else (x, err)
                  in
                  let y', err'' =
                    if e2 < dx then (y + sy, err' + dx) else (y, err')
                  in
                  draw x' y' err''
              in
              draw x1 y1 (dx - dy);

              (* Render the braille buffer *)
              Hashtbl.iter
                (fun (x, y) bits ->
                  let braille_code = 0x2800 + bits in
                  let char = Uchar.of_int braille_code in
                  let char_str =
                    let buf = Buffer.create 4 in
                    Uutf.Buffer.add_utf_8 buf char;
                    Buffer.contents buf
                  in
                  plot_fn ~x ~y ~style char_str)
                braille_buffer
        in

        let draw_box ~x ~y ~width ~height ?(style = Style.empty) ?border () =
          match border with
          | Some _ ->
              (* Draw border box *)
              for i = x + 1 to x + width - 2 do
                plot_fn ~x:i ~y ~style "─";
                plot_fn ~x:i ~y:(y + height - 1) ~style "─"
              done;
              for j = y + 1 to y + height - 2 do
                plot_fn ~x ~y:j ~style "│";
                plot_fn ~x:(x + width - 1) ~y:j ~style "│"
              done;
              plot_fn ~x ~y ~style "┌";
              plot_fn ~x:(x + width - 1) ~y ~style "┐";
              plot_fn ~x ~y:(y + height - 1) ~style "└";
              plot_fn ~x:(x + width - 1) ~y:(y + height - 1) ~style "┘"
          | None ->
              (* Fill box - use full block character for filled effect *)
              for j = y to y + height - 1 do
                for i = x to x + width - 1 do
                  plot_fn ~x:i ~y:j ~style "█"
                done
              done
        in

        let canvas = { plot; draw_line; draw_box } in
        draw_fn ~width ~height canvas)

  let plot canvas ~x ~y ?(style = Style.empty) str =
    canvas.plot ~x ~y ~style str

  let draw_line ~x1 ~y1 ~x2 ~y2 ?(style = Style.empty) ?(kind = `Line) canvas =
    canvas.draw_line ~x1 ~y1 ~x2 ~y2 ~style ~kind ()

  let draw_box ~x ~y ~width ~height ?(style = Style.empty) ?border canvas =
    canvas.draw_box ~x ~y ~width ~height ~style ?border ()
end

let canvas ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    draw_fn =
  Element.canvas ?width ?height ?min_width ?min_height ?max_width ?max_height
    ?padding ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border
    ?border_style draw_fn

let render ?(dark = false) ?(theme = Theme.default_dark) ?calc ?snapshot screen
    ui =
  let viewport =
    Screen.Viewport.full ~rows:(Screen.rows screen) ~cols:(Screen.cols screen)
  in

  (* If a snapshot is provided, use it for recording during rendering *)
  let render_with_ctx snapshot_opt =
    let ctx =
      {
        Renderer.screen;
        dark;
        theme;
        viewport;
        snapshot = snapshot_opt;
        z_counter = 0;
        inherited_style = None;
      }
    in

    let ui_id, ui_tree = ui in
    let available_space =
      {
        Toffee.Geometry.Size.width =
          Toffee.Available_space.Definite (float_of_int (Screen.cols screen));
        height =
          Toffee.Available_space.Definite (float_of_int (Screen.rows screen));
      }
    in

    (* Compute layout with measure function for text nodes *)
    (* Note: calc support is defined in the API but not yet fully implemented in Toffee.
       The calc_resolver parameter is accepted but currently ignored. *)
    let _ = calc in
    (* Suppress unused warning until Toffee implements calc support *)
    let _ =
      Toffee.compute_layout_with_measure ui_tree ui_id available_space
        default_measure_fn
      |> Result.get_ok
    in

    Renderer.render_node ctx (ui_id, ui_tree)
  in

  (* Use with_recording if snapshot provided, otherwise render without *)
  match snapshot with
  | Some snap ->
      Layout_snapshot.with_recording snap (fun () ->
          render_with_ctx (Some snap))
  | None -> render_with_ctx None

let render_string ?(width = 80) ?height ?(dark = false)
    ?(theme = Theme.default_dark) ?calc ui =
  let measured_height =
    match height with
    | Some h -> h
    | None -> (
        let ui_id, ui_tree = ui in
        let available_space =
          {
            Toffee.Geometry.Size.width =
              Toffee.Available_space.Definite (float_of_int width);
            height = Toffee.Available_space.Definite 1000.0;
          }
        in
        let _ = calc in
        (* Suppress unused warning until Toffee implements calc support *)
        let _ =
          Toffee.compute_layout_with_measure ui_tree ui_id available_space
            default_measure_fn
          |> Result.get_ok
        in
        match Toffee.layout ui_tree ui_id with
        | Ok layout ->
            let size = Toffee.Layout.size layout in
            int_of_float size.height + 1
        | Error _ -> 50 (* fallback height *))
  in

  let screen = Screen.create ~rows:measured_height ~cols:width () in
  render ~dark ~theme ?calc screen ui;
  Screen.render_to_string screen

let print ?(width = 80) ?height ?(dark = false) ?(theme = Theme.default_dark)
    ?calc ui =
  let output = render_string ~width ?height ~dark ~theme ?calc ui in
  (* Use buffered output for better performance *)
  let out_channel = stdout in
  output_string out_channel output;
  flush out_channel
