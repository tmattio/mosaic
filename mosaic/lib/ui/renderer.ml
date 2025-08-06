type render_context = {
  screen : Screen.t;
  dark : bool;
  theme : Theme.t;
  viewport : Screen.Viewport.t;
}

type bounds = { x : float; y : float; width : float; height : float }

let draw_border ctx border bounds =
  let {
    Border.tl;
    th;
    tr;
    vl;
    bl;
    bh;
    br;
    vr;
    ml = _;
    mr = _;
    mt = _;
    mb = _;
    mc = _;
  } =
    Border.get_chars (Border.line_style border)
  in
  let row = int_of_float bounds.y in
  let col = int_of_float bounds.x in
  let width = int_of_float bounds.width in
  let height = int_of_float bounds.height in

  let attrs =
    let base_style =
      match Border.color border with
      | Some color -> Style.(fg color)
      | None -> Style.empty
    in
    let final_style =
      match Border.style border with
      | Some s -> Style.merge base_style s
      | None -> base_style
    in
    Style.resolve final_style ~dark:ctx.dark ~pos:(0, 0) ~bounds:(1, 1)
  in

  if Border.top border && height > 0 then (
    Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col ~glyph:tl
      ~attrs;
    for i = 1 to width - 2 do
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col:(col + i)
        ~glyph:th ~attrs
    done;
    if width > 1 then
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
        ~col:(col + width - 1)
        ~glyph:tr ~attrs);

  if Border.bottom border && height > 1 then (
    let bottom_row = row + height - 1 in
    Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row ~col
      ~glyph:bl ~attrs;
    for i = 1 to width - 2 do
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
        ~col:(col + i) ~glyph:bh ~attrs
    done;
    if width > 1 then
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
        ~col:(col + width - 1)
        ~glyph:br ~attrs);

  if Border.left border then
    for i = 1 to height - 2 do
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:(row + i) ~col
        ~glyph:vl ~attrs
    done;

  if Border.right border && width > 0 then
    let right_col = col + width - 1 in
    for i = 1 to height - 2 do
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:(row + i)
        ~col:right_col ~glyph:vr ~attrs
    done

let fill_background ctx style bounds =
  let row = int_of_float bounds.y in
  let col = int_of_float bounds.x in
  let width = int_of_float bounds.width in
  let height = int_of_float bounds.height in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let attrs =
        Style.resolve style ~dark:ctx.dark ~pos:(x, y) ~bounds:(width, height)
      in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:(row + y)
        ~col:(col + x) ~glyph:" " ~attrs
    done
  done

let rec render_node ctx (node_id, tree) =
  (* Get layout for this node *)
  let layout =
    match Toffee.layout tree node_id with
    | Ok l -> l
    | Error _ -> failwith "Failed to get layout"
  in

  let location = Toffee.Layout.location layout in
  let size = Toffee.Layout.size layout in
  let bounds =
    {
      x = location.x;
      y = location.y;
      width = size.width;
      height = size.height;
    }
  in

  (* Render this node's renderable *)
  (match Toffee.get_node_context tree node_id with
  | Some renderable -> (
      match renderable with
      | Renderable.Empty -> ()
      | Renderable.Box { border; background } ->
          Option.iter (fun bg -> fill_background ctx bg bounds) background;
          Option.iter (fun b -> draw_border ctx b bounds) border
      | Renderable.Text { content; style; align; wrap = _; _ } ->
          let row = int_of_float bounds.y in
          let col = int_of_float bounds.x in
          let width = int_of_float bounds.width in
          let height = int_of_float bounds.height in

          let start_col =
            match align with
            | `Start -> col
            | `Center -> col + ((width - String.length content) / 2)
            | `End -> col + width - String.length content
            | `Stretch -> col
          in

          let attrs =
            Style.resolve style ~dark:ctx.dark ~pos:(0, 0)
              ~bounds:(width, height)
          in
          let _ =
            Screen.set_text ctx.screen ~viewport:ctx.viewport ~row
              ~col:start_col ~text:content ~attrs
          in
          ()
      | Renderable.Canvas { draw } ->
          let row = int_of_float bounds.y in
          let col = int_of_float bounds.x in
          let width = int_of_float bounds.width in
          let height = int_of_float bounds.height in

          let plot ~x ~y ?style glyph =
            if x >= 0 && x < width && y >= 0 && y < height then
              let attrs =
                match style with
                | Some s ->
                    Style.resolve s ~dark:ctx.dark ~pos:(x, y)
                      ~bounds:(width, height)
                | None -> Ansi.Style.default
              in
              Screen.set_grapheme ctx.screen ~viewport:ctx.viewport
                ~row:(row + y) ~col:(col + x) ~glyph ~attrs
          in
          draw plot
      | Renderable.Scroll { h_offset = _; v_offset = _ } -> ())
  | _ -> ());

  (* Render children *)
  match Toffee.children tree node_id with
  | Ok children ->
      List.iter (fun child_id -> render_node ctx (child_id, tree)) children
  | Error _ -> ()
