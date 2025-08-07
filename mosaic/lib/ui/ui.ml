module Style = Style
module Border = Border
module Theme = Theme
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
      (* Measure text content *)
      let lines = String.split_on_char '\n' content in
      (* Expand tabs before measuring *)
      let expanded_lines =
        List.map
          (fun line -> Renderer.expand_tabs ~tab_width ~start_col:0 line)
          lines
      in

      (* Calculate width and height based on wrap mode *)
      let computed_width, computed_height =
        match wrap with
        | `Wrap -> (
            (* When wrapping, pre-wrap to available width if definite *)
            match available_space.Toffee.Geometry.Size.width with
            | Toffee.Available_space.Definite available_w ->
                let available_width = int_of_float available_w in
                (* Count wrapped lines for height calculation *)
                let wrapped_line_count =
                  List.fold_left
                    (fun acc line ->
                      let line_width = measure_string line in
                      if line_width <= available_width then acc + 1
                      else
                        (* Calculate number of wrapped lines *)
                        let lines_needed =
                          if available_width > 0 then
                            (line_width + available_width - 1) / available_width
                          else 1
                        in
                        acc + lines_needed)
                    0 expanded_lines
                in
                (* For wrapped text, width is constrained to available width *)
                let max_line_width =
                  List.fold_left
                    (fun acc line ->
                      max acc (min (measure_string line) available_width))
                    0 expanded_lines
                in
                let width =
                  match known_dimensions.Toffee.Geometry.Size.width with
                  | Some w -> w
                  | None -> min available_w (float_of_int max_line_width)
                in
                let height =
                  match known_dimensions.Toffee.Geometry.Size.height with
                  | Some h -> h
                  | None -> float_of_int wrapped_line_count
                in
                (width, height)
            | _ ->
                (* No definite width to wrap to, use natural size *)
                let max_width =
                  List.fold_left
                    (fun acc line -> max acc (measure_string line))
                    0 expanded_lines
                in
                let width =
                  match known_dimensions.Toffee.Geometry.Size.width with
                  | Some w -> w
                  | None -> float_of_int max_width
                in
                let height =
                  match known_dimensions.Toffee.Geometry.Size.height with
                  | Some h -> h
                  | None -> float_of_int (List.length lines)
                in
                (width, height))
        | `Truncate | `Clip ->
            (* Don't clamp to available size - use natural content size *)
            let max_width =
              List.fold_left
                (fun acc line -> max acc (measure_string line))
                0 expanded_lines
            in
            let width =
              match known_dimensions.Toffee.Geometry.Size.width with
              | Some w -> w
              | None -> float_of_int max_width
            in
            let height =
              match known_dimensions.Toffee.Geometry.Size.height with
              | Some h -> h
              | None -> float_of_int (List.length lines)
            in
            (width, height)
      in

      { Toffee.Geometry.Size.width = computed_width; height = computed_height }
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

let render ?(dark = false) ?(theme = Theme.default_dark) ?calc screen ui =
  Screen.begin_frame screen;

  let viewport =
    Screen.Viewport.full ~rows:(Screen.rows screen) ~cols:(Screen.cols screen)
  in

  let ctx = { Renderer.screen; dark; theme; viewport } in

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
  print_string output;
  flush stdout
