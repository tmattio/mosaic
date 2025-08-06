module Style = Style
module Border = Border
module Theme = Theme
module Spacing = Spacing
include Element

type element = Element.t

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
      ?border_style (fun plot_fn ->
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
                plot_fn ~x ~y ~style (if dx > dy then "─" else "│");
                if x = x2 && y = y2 then ()
                else
                  let e2 = err * 2 in
                  let err', x', y' =
                    let err1 =
                      if e2 > -dy then (err - dy, x + sx, y) else (err, x, y)
                    in
                    let err2, x2, y2 = err1 in
                    if e2 < dx then (err2 + dx, x2, y2 + sy) else (err2, x2, y2)
                  in
                  draw x' y' err'
              in
              draw x1 y1 (dx - dy)
          | `Braille ->
              (* For Braille, delegate to the braille buffer logic in charts *)
              ()
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
              (* Fill box *)
              for j = y to y + height - 1 do
                for i = x to x + width - 1 do
                  plot_fn ~x:i ~y:j ~style " "
                done
              done
        in

        let canvas = { plot; draw_line; draw_box } in
        draw_fn canvas)

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

let render ?(dark = false) ?(theme = Theme.default_dark) screen ui =
  Screen.begin_frame screen;

  let viewport =
    Screen.Viewport.full ~rows:(Screen.rows screen) ~cols:(Screen.cols screen)
  in

  let ctx = { Renderer.screen; dark; theme; viewport } in

  let ui_id, ui_tree = ui in
  let available_space =
    {
      Toffee.Geometry.Size.width =
        Toffee.Available_space.Definite
          (float_of_int (Screen.cols screen));
      height =
        Toffee.Available_space.Definite
          (float_of_int (Screen.rows screen));
    }
  in

  (* Compute layout *)
  let _ = Toffee.compute_layout ui_tree ui_id available_space |> Result.get_ok in

  Renderer.render_node ctx (ui_id, ui_tree)

let render_string ?(width = 80) ?height ?(dark = false)
    ?(theme = Theme.default_dark) ui =
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
        let _ = Toffee.compute_layout ui_tree ui_id available_space |> Result.get_ok in
        match Toffee.layout ui_tree ui_id with
        | Ok layout -> 
            let size = Toffee.Layout.size layout in
            int_of_float size.height + 1
        | Error _ -> 50 (* fallback height *))
  in

  let screen = Screen.create ~rows:measured_height ~cols:width () in
  render ~dark ~theme screen ui;
  Screen.render_to_string screen

let print ?(width = 80) ?height ?(dark = false) ?(theme = Theme.default_dark) ui
    =
  let output = render_string ~width ?height ~dark ~theme ui in
  print_string output;
  flush stdout

(* String utilities *)

let measure_string str =
  (* UTF-8 aware string width calculation *)
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec count_width acc =
    match Uutf.decode decoder with
    | `Uchar _ -> count_width (acc + 1)
    | `End -> acc
    | `Malformed _ -> count_width (acc + 1)
    | `Await -> acc
  in
  count_width 0

let truncate_string_with_ellipsis str max_width suffix =
  let suffix_len = String.length suffix in
  if measure_string str <= max_width then str
  else if max_width <= suffix_len then String.sub str 0 max_width
  else
    (* UTF-8 safe truncation *)
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let buf = Buffer.create (String.length str) in
    let rec take_chars width =
      if width <= suffix_len then ()
      else
        match Uutf.decode decoder with
        | `Uchar u ->
            let char_str =
              String.init (Uchar.utf_8_byte_length u) (fun i ->
                  String.get str
                    (Uutf.decoder_byte_count decoder
                    - Uchar.utf_8_byte_length u + i))
            in
            if width - 1 >= suffix_len then (
              Buffer.add_string buf char_str;
              take_chars (width - 1))
        | `End | `Malformed _ | `Await -> ()
    in
    take_chars (max_width - suffix_len);
    Buffer.contents buf ^ suffix

let pad_string str width =
  let str_width = measure_string str in
  if str_width >= width then str else str ^ String.make (width - str_width) ' '
