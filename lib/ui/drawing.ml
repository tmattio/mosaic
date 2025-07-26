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
                  let w = max 0 (Uucp.Break.tty_width_hint u) in
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
            if break_byte = !last_space_byte && break_byte > 0 then break_byte
            else break_byte
          in
          if adjusted_break_byte = 0 then
            let split_at =
              Render.unicode_substring line width |> String.length
            in
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

(* Helper to distribute integer remainder fairly using largest remainder method *)
let distribute_fairly total_to_distribute weights =
  let total_weight = List.fold_left ( + ) 0 weights in
  if total_weight = 0 then List.map (fun _ -> 0) weights
  else
    let with_fractions =
      List.mapi
        (fun i w ->
          let base = total_to_distribute * w / total_weight in
          let frac = total_to_distribute * w mod total_weight in
          (i, base, frac))
        weights
    in
    let distributed =
      List.fold_left (fun acc (_, base, _) -> acc + base) 0 with_fractions
    in
    let remainder = total_to_distribute - distributed in
    let sorted_by_fraction =
      List.sort (fun (_, _, f1) (_, _, f2) -> compare f2 f1) with_fractions
    in
    let rec distribute_remainder n acc = function
      | [] -> acc
      | (idx, base, _) :: rest ->
          if n > 0 then
            distribute_remainder (n - 1) ((idx, base + 1) :: acc) rest
          else distribute_remainder 0 ((idx, base) :: acc) rest
    in
    let final_with_remainder =
      distribute_remainder remainder [] sorted_by_fraction
    in
    List.sort (fun (i1, _) (i2, _) -> compare i1 i2) final_with_remainder
    |> List.map snd

let compute_sizes ~defs ~mins ~available ~spacing =
  let len = List.length defs in
  if len <> List.length mins then invalid_arg "defs and mins length mismatch";
  let total_spacing = spacing * max 0 (len - 1) in
  let total_fixed = ref 0 in
  let total_min_flex = ref 0 in
  let flex_weights = ref [] in
  List.iter2
    (fun def m ->
      match def with
      | `Fixed s -> total_fixed := !total_fixed + s
      | `Flex f ->
          total_min_flex := !total_min_flex + m;
          flex_weights := f :: !flex_weights)
    defs mins;
  let flex_space =
    max 0 (available - !total_fixed - !total_min_flex - total_spacing)
  in
  let flex_amounts = distribute_fairly flex_space (List.rev !flex_weights) in
  let k = ref 0 in
  List.map2
    (fun def m ->
      match def with
      | `Fixed s -> s
      | `Flex _ ->
          let amt = List.nth flex_amounts !k in
          incr k;
          m + amt)
    defs mins

(* Convert a Style.t to Render.attr, resolving gradients at the given position *)
let style_to_attr style ~dark ~x ~y ~width ~height =
  let fg = Style.resolve_color style.Style.fg ~dark ~x ~y ~width ~height in
  let bg = Style.resolve_color style.Style.bg ~dark ~x ~y ~width ~height in
  {
    Render.fg;
    bg;
    bold = style.Style.bold;
    dim = style.Style.dim;
    italic = style.Style.italic;
    underline = style.Style.underline;
    blink = style.Style.blink;
    reverse = style.Style.reverse;
    strikethrough = style.Style.strikethrough;
    uri = style.Style.uri;
  }

let fill_rect ?(clip : Render.Clip.t option = None) ~buffer
    ~rect:(x, y, width, height) ~style ~dark () =
  let has_gradient =
    match (style.Style.fg, style.Style.bg) with
    | Some (Style.Gradient _), _
    | _, Some (Style.Gradient _)
    | Some (Style.Adaptive _), _
    | _, Some (Style.Adaptive _) ->
        true
    | _ -> false
  in
  let space_char = Uchar.of_int 0x20 in

  if has_gradient then
    (* Handle gradients by calculating color for each cell *)
    for dy = 0 to height - 1 do
      for dx = 0 to width - 1 do
        let px = x + dx in
        let py = y + dy in
        let attr = style_to_attr style ~dark ~x:dx ~y:dy ~width ~height in
        Render.set_char ?clip buffer px py space_char attr
      done
    done
  else
    (* Solid color - convert once and reuse *)
    let attr = style_to_attr style ~dark ~x:0 ~y:0 ~width:1 ~height:1 in
    for dy = 0 to height - 1 do
      for dx = 0 to width - 1 do
        let px = x + dx in
        let py = y + dy in
        Render.set_char ?clip buffer px py space_char attr
      done
    done

let draw_border ?(clip : Render.Clip.t option = None) ~buffer
    ~rect:(x, y, width, height) ~border ~dark () =
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
        | Some color -> Style.fg color
        | None -> Style.empty
      in
      let border_attr =
        style_to_attr border_style ~dark ~x:0 ~y:0 ~width:1 ~height:1
      in

      let left_edge_visible = eff_x = x in
      let right_edge_visible = eff_x + eff_w = x + width in
      let top_edge_visible = eff_y = y in
      let bottom_edge_visible = eff_y + eff_h = y + height in
      let eff_right = eff_x + eff_w - 1 in

      (* Corners *)
      let left_corner_x = if left_edge_visible then x else eff_x in
      let right_corner_x =
        if right_edge_visible then x + width - 1 else eff_right
      in
      let top_corner_y = y in
      let bottom_corner_y = y + height - 1 in

      if top_edge_visible && Border.top border && Border.left border then
        Render.set_string ?clip buffer left_corner_x top_corner_y tl border_attr;
      if top_edge_visible && Border.top border && Border.right border then
        Render.set_string ?clip buffer right_corner_x top_corner_y tr
          border_attr;
      if bottom_edge_visible && Border.bottom border && Border.left border then
        Render.set_string ?clip buffer left_corner_x bottom_corner_y bl
          border_attr;
      if bottom_edge_visible && Border.bottom border && Border.right border then
        Render.set_string ?clip buffer right_corner_x bottom_corner_y br
          border_attr;

      (* Horizontal lines *)
      (if Border.top border && top_edge_visible then
         let h_start_x =
           if Border.left border then left_corner_x + 1 else left_corner_x
         in
         let h_end_x =
           if Border.right border then right_corner_x - 1 else right_corner_x
         in
         if h_end_x >= h_start_x then
           for i = h_start_x to h_end_x do
             Render.set_string ?clip buffer i top_corner_y t border_attr
           done);
      (if Border.bottom border && bottom_edge_visible then
         let h_start_x =
           if Border.left border then left_corner_x + 1 else left_corner_x
         in
         let h_end_x =
           if Border.right border then right_corner_x - 1 else right_corner_x
         in
         if h_end_x >= h_start_x then
           for i = h_start_x to h_end_x do
             Render.set_string ?clip buffer i bottom_corner_y b border_attr
           done);

      (* Vertical lines *)
      if eff_h > 0 then
        let v_start_y = if Border.top border then y + 1 else y in
        let v_end_y =
          if Border.bottom border then y + height - 2 else y + height - 1
        in
        if v_end_y >= v_start_y then
          for i = v_start_y to v_end_y do
            (if Border.left border then
               let left_v_x = if left_edge_visible then x else eff_x in
               Render.set_string ?clip buffer left_v_x i l border_attr);
            if Border.right border then
              let right_v_x =
                if right_edge_visible then x + width - 1 else eff_right
              in
              Render.set_string ?clip buffer right_v_x i r border_attr
          done

let draw_text ?(clip : Render.Clip.t option = None) ~buffer ~pos:(x, y)
    ~bounds:(w, h) ~text ~style ~align ~tab_width ~wrap ~dark () =
  let expanded = Render.expand_tabs text tab_width in
  let lines =
    if wrap then wrap_text expanded w else String.split_on_char '\n' expanded
  in

  let has_gradient =
    match (style.Style.fg, style.Style.bg) with
    | Some (Style.Gradient _), _
    | _, Some (Style.Gradient _)
    | Some (Style.Adaptive _), _
    | _, Some (Style.Adaptive _) ->
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

  let total_lines = List.length lines in

  List.iteri
    (fun i line ->
      if i < h then (
        let line_width = Render.measure_string line in
        let x_offset = align_offset w line_width in
        let available_width = w - x_offset in
        let clipped_line =
          if line_width > available_width then
            Render.unicode_substring line available_width
          else line
        in
        let clipped_width = Render.measure_string clipped_line in

        if has_gradient then
          (* Draw string character by character with gradient *)
          let decoder = Uutf.decoder ~encoding:`UTF_8 (`String clipped_line) in
          let rec draw_chars char_x =
            match Uutf.decode decoder with
            | `Uchar u ->
                let char_width = max 0 (Uucp.Break.tty_width_hint u) in
                let rel_x = char_x - x - x_offset in
                let attr =
                  style_to_attr style ~dark ~x:rel_x ~y:i ~width:clipped_width
                    ~height:total_lines
                in
                Render.set_char ?clip buffer char_x (y + i) u attr;
                draw_chars (char_x + char_width)
            | `End -> ()
            | `Malformed _ ->
                let replacement = Uchar.of_int 0xFFFD in
                let rel_x = char_x - x - x_offset in
                let attr =
                  style_to_attr style ~dark ~x:rel_x ~y:i ~width:clipped_width
                    ~height:total_lines
                in
                Render.set_char ?clip buffer char_x (y + i) replacement attr;
                draw_chars (char_x + 1)
            | `Await -> ()
          in
          draw_chars (x + x_offset)
        else
          (* Solid style - use set_string for efficiency *)
          let attr = style_to_attr style ~dark ~x:0 ~y:0 ~width:1 ~height:1 in
          Render.set_string ?clip buffer (x + x_offset) (y + i) clipped_line
            attr;

          (* Pad remaining space on the line based on alignment *)
          let remaining = w - x_offset - clipped_width in
          if remaining > 0 then
            let pad_str = String.make remaining ' ' in
            let pad_attr =
              style_to_attr style ~dark ~x:0 ~y:0 ~width:1 ~height:1
            in
            match align with
            | `Start ->
                (* Pad right *)
                Render.set_string ?clip buffer
                  (x + x_offset + clipped_width)
                  (y + i) pad_str pad_attr
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
                    pad_attr
            | `Stretch ->
                (* Pad right *)
                Render.set_string ?clip buffer
                  (x + x_offset + clipped_width)
                  (y + i) pad_str pad_attr))
    lines

let draw_rich_text ?(clip : Render.Clip.t option = None) ~buffer
    ~pos:(x_start, y) ~width ~segments ~dark () =
  let rec render_segments x segs =
    match segs with
    | [] -> ()
    | (s, style) :: rest ->
        let available = x_start + width - x in
        if available <= 0 then ()
        else
          let w = Render.measure_string s in
          let clipped_s =
            if w > available then Render.unicode_substring s available else s
          in
          let clipped_w = Render.measure_string clipped_s in
          if clipped_w > 0 then (
            let has_gradient =
              match (style.Style.fg, style.Style.bg) with
              | Some (Style.Gradient _), _
              | _, Some (Style.Gradient _)
              | Some (Style.Adaptive _), _
              | _, Some (Style.Adaptive _) ->
                  true
              | _ -> false
            in
            if has_gradient then
              (* Draw character by character for gradients *)
              let decoder = Uutf.decoder ~encoding:`UTF_8 (`String clipped_s) in
              let rec draw_chars char_x =
                match Uutf.decode decoder with
                | `Uchar u ->
                    let char_width = max 0 (Uucp.Break.tty_width_hint u) in
                    let rel_x = char_x - x_start in
                    let attr =
                      style_to_attr style ~dark ~x:rel_x ~y:0 ~width:clipped_w
                        ~height:1
                    in
                    Render.set_char ?clip buffer char_x y u attr;
                    draw_chars (char_x + char_width)
                | `End -> ()
                | `Malformed _ ->
                    let replacement = Uchar.of_int 0xFFFD in
                    let rel_x = char_x - x_start in
                    let attr =
                      style_to_attr style ~dark ~x:rel_x ~y:0 ~width:clipped_w
                        ~height:1
                    in
                    Render.set_char ?clip buffer char_x y replacement attr;
                    draw_chars (char_x + 1)
                | `Await -> ()
              in
              draw_chars x
            else
              (* Solid style *)
              let attr =
                style_to_attr style ~dark ~x:0 ~y:0 ~width:1 ~height:1
              in
              Render.set_string ?clip buffer x y clipped_s attr;
              render_segments (x + clipped_w) rest)
  in
  render_segments x_start segments

let draw_braille_line ~buffer ~x1 ~y1 ~x2 ~y2 ~style ~dark () =
  (* Braille characters use a 2x4 grid mapped to Unicode points U+2800-U+28FF *)
  let module BrailleBuffer = struct
    type t = { _width : int; _height : int; cells : (int * int, int) Hashtbl.t }

    let create width height =
      {
        _width = (width + 1) / 2;
        (* Braille cells are 2 pixels wide *)
        _height = (height + 3) / 4;
        (* Braille cells are 4 pixels tall *)
        cells = Hashtbl.create 100;
      }

    let set_pixel buffer x y =
      if x >= 0 && y >= 0 then
        let cell_x = x / 2 in
        let cell_y = y / 4 in
        let bit_x = x mod 2 in
        let bit_y = y mod 4 in
        (* Braille dot mapping:
           bit positions in cell:
           0 3
           1 4
           2 5
           6 7 *)
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
        let current = try Hashtbl.find buffer.cells key with Not_found -> 0 in
        Hashtbl.replace buffer.cells key (current lor (1 lsl bit_pos))

    let render buffer render_buffer style dark =
      Hashtbl.iter
        (fun (x, y) bits ->
          let braille_code = 0x2800 + bits in
          let char = Uchar.of_int braille_code in
          let attr = style_to_attr style ~dark ~x ~y ~width:1 ~height:1 in
          Render.set_char render_buffer x y char attr)
        buffer.cells
  end in
  (* Create a buffer for the line *)
  let max_x = max x1 x2 in
  let max_y = max y1 y2 in
  let braille_buffer = BrailleBuffer.create (max_x + 1) (max_y + 1) in

  (* Draw line using Bresenham algorithm *)
  let dx = abs (x2 - x1) in
  let dy = abs (y2 - y1) in
  let sx = if x1 < x2 then 1 else -1 in
  let sy = if y1 < y2 then 1 else -1 in
  let err = ref (dx - dy) in
  let x = ref x1 in
  let y = ref y1 in

  let rec loop () =
    BrailleBuffer.set_pixel braille_buffer !x !y;
    if !x = x2 && !y = y2 then ()
    else
      let e2 = 2 * !err in
      if e2 > -dy then (
        err := !err - dy;
        x := !x + sx);
      if e2 < dx then (
        err := !err + dx;
        y := !y + sy);
      loop ()
  in
  loop ();
  BrailleBuffer.render braille_buffer buffer style dark

let draw_line ?(clip : Render.Clip.t option = None) ~buffer ~x1 ~y1 ~x2 ~y2
    ~style ~dark ~kind () =
  match kind with
  | `Braille -> draw_braille_line ~buffer ~x1 ~y1 ~x2 ~y2 ~style ~dark ()
  | `Line ->
      let dx = abs (x2 - x1) in
      let dy = abs (y2 - y1) in
      (* Positive dy - correct Bresenham *)
      let sx = if x1 < x2 then 1 else -1 in
      let sy = if y1 < y2 then 1 else -1 in
      let err = ref (dx - dy) in
      (* Standard Bresenham error initialization *)
      let x = ref x1 in
      let y = ref y1 in
      let char_for_slope =
        if dx > dy then "-" (* Mostly horizontal *)
        else if dy > dx then "|" (* Mostly vertical *)
        else if sx = sy then "\\"
        else "/" (* Diagonal *)
      in
      let rec loop () =
        (* Calculate per-point attribute for position-aware styles like gradients *)
        let rel_x = !x - x1 in
        let rel_y = !y - y1 in
        let attr =
          style_to_attr style ~dark ~x:rel_x ~y:rel_y ~width:(dx + 1)
            ~height:(dy + 1)
        in
        Render.set_string ?clip buffer !x !y char_for_slope attr;
        if !x = x2 && !y = y2 then ()
        else
          let e2 = 2 * !err in
          if e2 > -dy then (
            err := !err - dy;
            x := !x + sx);
          if e2 < dx then (
            err := !err + dx;
            y := !y + sy);
          loop ()
      in
      loop ()

let draw_box ?clip ~buffer ~x ~y ~width ~height ~style ~dark ?border () =
  match border with
  | Some b ->
      let rect = (x, y, width, height) in
      draw_border ?clip ~buffer ~rect ~border:b ~dark ()
  | None ->
      let rect = (x, y, width, height) in
      fill_rect ?clip ~buffer ~rect ~style ~dark ()
