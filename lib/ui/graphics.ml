let expand_tabs s tab_width =
  let rec expand_line line =
    match String.index_opt line '\t' with
    | None -> line
    | Some idx ->
        let before = String.sub line 0 idx in
        let after = String.sub line (idx + 1) (String.length line - idx - 1) in
        let col = Render.measure_string before in
        let spaces_needed = tab_width - (col mod tab_width) in
        let spaces = String.make spaces_needed ' ' in
        expand_line (before ^ spaces ^ after)
  in
  String.split_on_char '\n' s |> List.map expand_line |> String.concat "\n"

let unicode_substring str max_cells =
  if max_cells <= 0 then ""
  else
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let buffer = Buffer.create (String.length str) in
    let rec loop cells =
      if cells >= max_cells then Buffer.contents buffer
      else
        match Uutf.decode decoder with
        | `Uchar u ->
            let width = max 0 (Uucp.Break.tty_width_hint u) in
            if cells + width > max_cells then Buffer.contents buffer
            else (
              Buffer.add_utf_8_uchar buffer u;
              loop (cells + width))
        | `End | `Await -> Buffer.contents buffer
        | `Malformed _ -> loop cells
    in
    loop 0

let wrap_text text width =
  if width <= 0 then [ "" ]
  else
    let rec wrap_line line =
      if line = "" then [ "" ]
      else
        let len = Render.measure_string line in
        if len <= width then [ line ]
        else
          let decoder = Uutf.decoder ~encoding:`UTF_8 (`String line) in
          let last_space_byte = ref (-1) in
          let find_wrap () =
            let current_cell = ref 0 in
            let prev_byte = ref 0 in
            let rec loop () =
              match Uutf.decode decoder with
              | `Uchar u ->
                  let w = max 1 (Uucp.Break.tty_width_hint u) in
                  let current_byte = Uutf.decoder_byte_count decoder in
                  let char_byte_size = current_byte - !prev_byte in
                  if Uchar.equal u (Uchar.of_char ' ') then
                    last_space_byte := current_byte;
                  if !current_cell + w > width && !current_cell > 0 then
                    if !last_space_byte > 0 then !last_space_byte
                    else current_byte - char_byte_size
                  else (
                    current_cell := !current_cell + w;
                    prev_byte := current_byte;
                    loop ())
              | `End -> String.length line
              | `Malformed _ ->
                  (* Skip malformed and continue *)
                  let current_byte = Uutf.decoder_byte_count decoder in
                  prev_byte := current_byte;
                  loop ()
              | `Await -> assert false
            in
            loop ()
          in
          let break_byte = find_wrap () in
          let adjusted_break_byte =
            if break_byte = !last_space_byte && break_byte > 0 then break_byte - 1
            else break_byte
          in
          if adjusted_break_byte = 0 then
            let split_at = unicode_substring line width |> String.length in
            let first = String.sub line 0 split_at in
            let rest =
              String.sub line split_at (String.length line - split_at)
            in
            first :: wrap_line rest
          else
            let first = String.sub line 0 adjusted_break_byte in
            let rest =
              String.sub line break_byte (String.length line - break_byte)
            in
            (* Only trim leading space if we broke at a space *)
            let rest =
              if
                break_byte = !last_space_byte
                && String.length rest > 0
                && rest.[0] = ' '
              then String.sub rest 1 (String.length rest - 1)
              else rest
            in
            if rest = "" then [ first ] else first :: wrap_line rest
    in
    String.split_on_char '\n' text |> List.concat_map wrap_line

let fill_rect ?(clip : Render.Clip.t option = None) ~buffer
    ~rect:(x, y, width, height) ~style () =
  let has_gradient =
    match style.Render.Style.bg with
    | Some (Render.Style.Gradient _) | Some (Render.Style.Adaptive _) -> true
    | _ -> false
  in
  if has_gradient then
    Render.fill_rect_gradient ?clip buffer x y width height style
  else Render.fill_rect ?clip buffer x y width height style

let draw_border ?(clip : Render.Clip.t option = None) ~buffer
    ~rect:(x, y, width, height) ~border () =
  if width <= 0 || height <= 0 then ()
  else
    let effective_rect =
      match clip with
      | None -> (x, y, width, height)
      | Some clip_rect ->
          let clip_x = Render.Clip.x clip_rect in
          let clip_y = Render.Clip.y clip_rect in
          let clip_w = Render.Clip.width clip_rect in
          let clip_h = Render.Clip.height clip_rect in
          let eff_x = max x clip_x in
          let eff_y = max y clip_y in
          let eff_right = min (x + width) (clip_x + clip_w) in
          let eff_bottom = min (y + height) (clip_y + clip_h) in
          let eff_w = max 0 (eff_right - eff_x) in
          let eff_h = max 0 (eff_bottom - eff_y) in
          (eff_x, eff_y, eff_w, eff_h)
    in
    let eff_x, eff_y, eff_w, eff_h = effective_rect in
    if eff_w <= 0 || eff_h <= 0 then ()
    else
      let border_chars style =
        match style with
        | Border.Solid -> ("┌", "─", "┐", "│", "└", "─", "┘", "│")
        | Border.Rounded -> ("╭", "─", "╮", "│", "╰", "─", "╯", "│")
        | Border.Double -> ("╔", "═", "╗", "║", "╚", "═", "╝", "║")
        | Border.Thick -> ("┏", "━", "┓", "┃", "┗", "━", "┛", "┃")
        | Border.ASCII -> ("+", "-", "+", "|", "+", "-", "+", "|")
      in
      let tl, t, tr, r, bl, b, br, l = border_chars (Border.style border) in
      let border_style =
        match Border.color border with
        | Some color -> Render.Style.fg color
        | None -> Render.Style.empty
      in
      let left_edge_visible = eff_x = x in
      let right_edge_visible = eff_x + eff_w = x + width in
      let top_edge_visible = eff_y = y in
      let bottom_edge_visible = eff_y + eff_h = y + height in
      let eff_right = eff_x + eff_w - 1 in

      (* Corners *)
      let left_corner_x = if left_edge_visible then x else eff_x in
      let right_corner_x = if right_edge_visible then x + width - 1 else eff_right in
      let top_corner_y = y in
      let bottom_corner_y = y + height - 1 in

      if top_edge_visible && Border.top border && Border.left border then
        Render.set_string ?clip buffer left_corner_x top_corner_y tl border_style;
      if top_edge_visible && Border.top border && Border.right border then
        Render.set_string ?clip buffer right_corner_x top_corner_y tr border_style;
      if bottom_edge_visible && Border.bottom border && Border.left border then
        Render.set_string ?clip buffer left_corner_x bottom_corner_y bl border_style;
      if bottom_edge_visible && Border.bottom border && Border.right border then
        Render.set_string ?clip buffer right_corner_x bottom_corner_y br border_style;

      (* Horizontal lines *)
      if Border.top border && top_edge_visible then (
        let h_start_x =
          if Border.left border then left_corner_x + 1 else left_corner_x
        in
        let h_end_x =
          if Border.right border then right_corner_x - 1 else right_corner_x
        in
        if h_end_x >= h_start_x then
          for i = h_start_x to h_end_x do
            Render.set_string ?clip buffer i top_corner_y t border_style
          done);
      if Border.bottom border && bottom_edge_visible then (
        let h_start_x =
          if Border.left border then left_corner_x + 1 else left_corner_x
        in
        let h_end_x =
          if Border.right border then right_corner_x - 1 else right_corner_x
        in
        if h_end_x >= h_start_x then
          for i = h_start_x to h_end_x do
            Render.set_string ?clip buffer i bottom_corner_y b border_style
          done);

      (* Vertical lines *)
      if eff_h > 0 then (
        let v_start_y =
          if Border.top border then y + 1 else y
        in
        let v_end_y =
          if Border.bottom border then y + height - 2 else y + height - 1
        in
        if v_end_y >= v_start_y then
          for i = v_start_y to v_end_y do
            if Border.left border then
              let left_v_x = if left_edge_visible then x else eff_x in
              Render.set_string ?clip buffer left_v_x i l border_style;
            if Border.right border then
              let right_v_x = if right_edge_visible then x + width - 1 else eff_right in
              Render.set_string ?clip buffer right_v_x i r border_style
          done)

let draw_text ?(clip : Render.Clip.t option = None) ~buffer ~pos:(x, y)
    ~bounds:(w, h) ~text ~style ~align ~tab_width ~wrap () =
  let expanded = expand_tabs text tab_width in
  let lines =
    if wrap then wrap_text expanded w else String.split_on_char '\n' expanded
  in
  let total_lines = List.length lines in

  let has_gradient =
    match (style.Render.Style.fg, style.Render.Style.bg) with
    | Some (Render.Style.Gradient _), _
    | _, Some (Render.Style.Gradient _)
    | Some (Render.Style.Adaptive _), _
    | _, Some (Render.Style.Adaptive _) ->
        true
    | _ -> false
  in

  let align_offset available used =
    match align with
    | `Start -> 0
    | `Center -> max 0 ((available - used) / 2)
    | `End -> max 0 (available - used)
    | `Stretch -> 0
  in

  List.iteri
    (fun i line ->
      if i < h then (
        let line_width = Render.measure_string line in
        let x_offset = align_offset w line_width in
        let available_width = w - x_offset in
        let clipped_line =
          if line_width > available_width then
            unicode_substring line available_width
          else line
        in

        if has_gradient then
          Render.set_string_gradient ?clip buffer (x + x_offset) (y + i)
            clipped_line style
            ~width:(Render.measure_string clipped_line)
            ~height:total_lines ~line_offset:i
        else
          Render.set_string ?clip buffer (x + x_offset) (y + i) clipped_line
            style;

        (* Pad remaining space on the line based on alignment *)
        let clipped_width = Render.measure_string clipped_line in
        let remaining = w - x_offset - clipped_width in
        if remaining > 0 then
          let pad_str = String.make remaining ' ' in
          match align with
          | `Start ->
              (* Pad right *)
              Render.set_string ?clip buffer
                (x + x_offset + clipped_width)
                (y + i) pad_str style
          | `End ->
              (* Already padded left by x_offset *)
              ()
          | `Center ->
              (* Pad both sides - already padded left, pad right *)
              let right_pad = remaining in
              if right_pad > 0 then
                Render.set_string ?clip buffer
                  (x + x_offset + clipped_width)
                  (y + i)
                  (String.make right_pad ' ')
                  style
          | `Stretch ->
              (* Pad right *)
              Render.set_string ?clip buffer
                (x + x_offset + clipped_width)
                (y + i) pad_str style))
    lines

let draw_rich_text ?(clip : Render.Clip.t option = None) ~buffer
    ~pos:(x_start, y) ~width ~segments () =
  let rec render_segments x segs =
    match segs with
    | [] -> ()
    | (s, style) :: rest ->
        let available = x_start + width - x in
        if available <= 0 then ()
        else
          let w = Render.measure_string s in
          let clipped_s =
            if w > available then unicode_substring s available else s
          in
          let clipped_w = Render.measure_string clipped_s in
          if clipped_w > 0 then (
            let has_gradient =
              match (style.Render.Style.fg, style.Render.Style.bg) with
              | Some (Render.Style.Gradient _), _
              | _, Some (Render.Style.Gradient _)
              | Some (Render.Style.Adaptive _), _
              | _, Some (Render.Style.Adaptive _) ->
                  true
              | _ -> false
            in
            if has_gradient then
              Render.set_string_gradient ?clip buffer x y clipped_s style
                ~width:clipped_w ~height:1 ~line_offset:0
            else Render.set_string ?clip buffer x y clipped_s style;
            render_segments (x + clipped_w) rest)
  in
  render_segments x_start segments
