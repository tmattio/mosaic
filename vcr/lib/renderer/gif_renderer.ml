(** GIF renderer for vcr - renders terminal output as animated GIF *)

let src = Logs.Src.create "vcr.renderer.gif" ~doc:"GIF renderer"

module Log = (val Logs.src_log src : Logs.LOG)

type theme = {
  bg : int * int * int;
  fg : int * int * int;
  black : int * int * int;
  red : int * int * int;
  green : int * int * int;
  yellow : int * int * int;
  blue : int * int * int;
  magenta : int * int * int;
  cyan : int * int * int;
  white : int * int * int;
  bright_black : int * int * int;
  bright_red : int * int * int;
  bright_green : int * int * int;
  bright_yellow : int * int * int;
  bright_blue : int * int * int;
  bright_magenta : int * int * int;
  bright_cyan : int * int * int;
  bright_white : int * int * int;
}

(** Default theme matching vhs colors *)
let default_theme =
  {
    bg = (23, 23, 23);
    (* #171717 - vhs Background color *)
    fg = (221, 221, 221);
    (* #dddddd - vhs Foreground color *)
    black = (40, 42, 46);
    (* #282a2e *)
    red = (215, 78, 111);
    (* #D74E6F *)
    green = (49, 187, 113);
    (* #31BB71 *)
    yellow = (211, 229, 97);
    (* #D3E561 *)
    blue = (128, 86, 255);
    (* #8056FF *)
    magenta = (237, 97, 215);
    (* #ED61D7 *)
    cyan = (4, 215, 215);
    (* #04D7D7 *)
    white = (191, 191, 191);
    (* #bfbfbf *)
    bright_black = (77, 77, 77);
    (* #4d4d4d *)
    bright_red = (254, 95, 134);
    (* #FE5F86 *)
    bright_green = (0, 215, 135);
    (* #00D787 *)
    bright_yellow = (235, 255, 113);
    (* #EBFF71 *)
    bright_blue = (155, 121, 255);
    (* #9B79FF *)
    bright_magenta = (255, 122, 234);
    (* #FF7AEA *)
    bright_cyan = (0, 254, 254);
    (* #00FEFE *)
    bright_white = (230, 230, 230);
    (* #e6e6e6 *)
  }

type config = {
  char_width : int;  (** Width of a character in pixels *)
  char_height : int;  (** Height of a character in pixels *)
  frame_delay : int;  (** Delay between frames in 1/100th of a second *)
  theme : theme;
  font_path : string option;  (** Path to font file, None for built-in font *)
  font_size : int;  (** Font size in pixels *)
  target_width : int option;  (** Desired output width in pixels *)
  target_height : int option;  (** Desired output height in pixels *)
  padding : int;  (** Padding around terminal content in pixels *)
}

type font_set = {
  regular : Freetype.t;
  bold : Freetype.t;
  italic : Freetype.t;
  bold_italic : Freetype.t;
}

type font_renderer = BuiltinFont | TrueTypeFont of font_set

type frame_data = {
  x_offset : int;
  y_offset : int;
  width : int;
  height : int;
  pixel_data : bytes;
  transparent_color : (int * int * int) option;
  delay_cs : int; (* Delay in centiseconds (1/100th of a second) *)
}

type t = {
  vte : Vte.t;
  config : config;
  font_renderer : font_renderer;
  mutable frames : frame_data list;
  mutable previous_pixels : (int * int * int) array option;
      (** Previous frame for optimization *)
  mutable working_pixels : (int * int * int) array option;
      (** Working buffer to avoid repeated allocations *)
  mutable previous_grid : Vte.cell option array array option;
      (** Copy of previous grid state for diffing *)
  mutable full_width : int;
  mutable full_height : int;
  mutable pending_delay_cs : int;
      (** Accumulated delay for frames with no changes *)
  mutable prev_cursor : int * int;
      (** Previous cursor position for diff calculation *)
}

(** Convert ANSI color to RGB *)
let rec color_of_ansi theme (c : Ansi.color) =
  match c with
  | Default -> theme.fg
  | Black -> theme.black
  | Red -> theme.red
  | Green -> theme.green
  | Yellow -> theme.yellow
  | Blue -> theme.blue
  | Magenta -> theme.magenta
  | Cyan -> theme.cyan
  | White -> theme.white
  | Bright_black -> theme.bright_black
  | Bright_red -> theme.bright_red
  | Bright_green -> theme.bright_green
  | Bright_yellow -> theme.bright_yellow
  | Bright_blue -> theme.bright_blue
  | Bright_magenta -> theme.bright_magenta
  | Bright_cyan -> theme.bright_cyan
  | Bright_white -> theme.bright_white
  | RGB (r, g, b) -> (r, g, b)
  | Index i ->
      (* Basic 256-color palette support *)
      if i < 16 then
        (* Standard and bright colors *)
        color_of_ansi theme
          (match i with
          | 0 -> Black
          | 1 -> Red
          | 2 -> Green
          | 3 -> Yellow
          | 4 -> Blue
          | 5 -> Magenta
          | 6 -> Cyan
          | 7 -> White
          | 8 -> Bright_black
          | 9 -> Bright_red
          | 10 -> Bright_green
          | 11 -> Bright_yellow
          | 12 -> Bright_blue
          | 13 -> Bright_magenta
          | 14 -> Bright_cyan
          | 15 -> Bright_white
          | _ -> Default)
      else if i < 232 then
        (* 216-color cube: 6x6x6 *)
        let i = i - 16 in
        let r = i / 36 * 51 in
        let g = i / 6 mod 6 * 51 in
        let b = i mod 6 * 51 in
        (r, g, b)
      else
        (* Grayscale *)
        let gray = 8 + ((i - 232) * 10) in
        (gray, gray, gray)

(** Simple bitmap font - 5x7 pixels per character *)
module Font = struct
  (* Minimal ASCII font data - just basic letters and symbols *)

  (** Get bitmap for a character (returns array of 7 rows, 5 bits each) *)
  let get_char_bitmap ch =
    match ch with
    | ' ' -> [| 0; 0; 0; 0; 0; 0; 0 |]
    | '!' -> [| 0x04; 0x04; 0x04; 0x04; 0x00; 0x04; 0x00 |]
    | 'A' | 'a' -> [| 0x0E; 0x11; 0x11; 0x1F; 0x11; 0x11; 0x00 |]
    | 'B' | 'b' -> [| 0x1E; 0x11; 0x1E; 0x11; 0x11; 0x1E; 0x00 |]
    | 'C' | 'c' -> [| 0x0E; 0x11; 0x10; 0x10; 0x11; 0x0E; 0x00 |]
    | 'E' | 'e' -> [| 0x1F; 0x10; 0x1E; 0x10; 0x10; 0x1F; 0x00 |]
    | 'H' | 'h' -> [| 0x11; 0x11; 0x1F; 0x11; 0x11; 0x11; 0x00 |]
    | 'L' | 'l' -> [| 0x10; 0x10; 0x10; 0x10; 0x10; 0x1F; 0x00 |]
    | 'O' | 'o' -> [| 0x0E; 0x11; 0x11; 0x11; 0x11; 0x0E; 0x00 |]
    | 'W' | 'w' -> [| 0x11; 0x11; 0x11; 0x15; 0x15; 0x0A; 0x00 |]
    | 'R' | 'r' -> [| 0x1E; 0x11; 0x11; 0x1E; 0x14; 0x12; 0x00 |]
    | 'D' | 'd' -> [| 0x1E; 0x11; 0x11; 0x11; 0x11; 0x1E; 0x00 |]
    | '0' -> [| 0x0E; 0x11; 0x13; 0x15; 0x19; 0x0E; 0x00 |]
    | '1' -> [| 0x04; 0x0C; 0x04; 0x04; 0x04; 0x0E; 0x00 |]
    | _ -> [| 0x1F; 0x11; 0x11; 0x11; 0x11; 0x1F; 0x00 |]
  (* Default box *)

  (** Render a character at position (x, y) in the pixel buffer *)
  let render_char pixels width height x y ch fg_color _bg_color char_width
      char_height =
    let bitmap = get_char_bitmap ch in
    (* Scale factor to make the font larger *)
    let scale_x = max 1 (char_width / 5) in
    let scale_y = max 1 (char_height / 7) in

    for row = 0 to 6 do
      for col = 0 to 4 do
        let bit = (bitmap.(row) lsr (4 - col)) land 1 in
        if bit = 1 then
          (* Draw scaled pixel *)
          for sy = 0 to scale_y - 1 do
            for sx = 0 to scale_x - 1 do
              let px = x + (col * scale_x) + sx in
              let py = y + (row * scale_y) + sy in
              if px >= 0 && px < width && py >= 0 && py < height then
                let idx = (py * width) + px in
                pixels.(idx) <- fg_color
            done
          done
      done
    done
end

let create vte config =
  (* Initialize font renderer based on config *)
  let font_renderer =
    match config.font_path with
    | None -> (
        (* Use embedded JetBrains Mono fonts by default *)
        try
          let regular_data =
            Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_regular
          in
          let bold_data = Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_bold in
          let italic_data =
            Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_italic
          in
          let bold_italic_data =
            Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_bold_italic
          in

          let regular =
            Freetype.create_from_memory ~font_data:regular_data
              ~pixel_size:config.font_size
          in
          let bold =
            Freetype.create_from_memory ~font_data:bold_data
              ~pixel_size:config.font_size
          in
          let italic =
            Freetype.create_from_memory ~font_data:italic_data
              ~pixel_size:config.font_size
          in
          let bold_italic =
            Freetype.create_from_memory ~font_data:bold_italic_data
              ~pixel_size:config.font_size
          in

          TrueTypeFont { regular; bold; italic; bold_italic }
        with exn ->
          Log.warn (fun m ->
              m
                "Failed to load embedded JetBrains Mono fonts: %s. Falling \
                 back to builtin font"
                (Printexc.to_string exn));
          BuiltinFont)
    | Some font_path ->
        (* Use the same font for all styles when custom font is provided *)
        let ft = Freetype.create ~font_path ~pixel_size:config.font_size in
        TrueTypeFont { regular = ft; bold = ft; italic = ft; bold_italic = ft }
  in
  {
    vte;
    config;
    font_renderer;
    frames = [];
    previous_pixels = None;
    working_pixels = None;
    previous_grid = None;
    full_width = 0;
    full_height = 0;
    pending_delay_cs = 0;
    prev_cursor = (-1, -1);
  }

(** Find bounding box of differences between two pixel arrays *)
let find_diff_bounds prev_pixels curr_pixels width height =
  let min_x = ref width in
  let min_y = ref height in
  let max_x = ref (-1) in
  let max_y = ref (-1) in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let idx = (y * width) + x in
      if prev_pixels.(idx) <> curr_pixels.(idx) then (
        min_x := min !min_x x;
        min_y := min !min_y y;
        max_x := max !max_x x;
        max_y := max !max_y y)
    done
  done;

  if !max_x >= 0 then
    Some (!min_x, !min_y, !max_x - !min_x + 1, !max_y - !min_y + 1)
  else None

let capture_frame t =
  let rows = Vte.rows t.vte in
  let cols = Vte.cols t.vte in
  let terminal_width = cols * t.config.char_width in
  let terminal_height = rows * t.config.char_height in

  (* Use target dimensions if specified, with padding included *)
  let total_width =
    match t.config.target_width with
    | Some w -> w
    | None -> terminal_width + (2 * t.config.padding)
  in
  let total_height =
    match t.config.target_height with
    | Some h -> h
    | None -> terminal_height + (2 * t.config.padding)
  in

  (* Get current grid state *)
  let current_grid =
    Array.init rows (fun r ->
        Array.init cols (fun c -> Vte.get_cell t.vte ~row:r ~col:c))
  in

  (* Get or create working buffer - reuse to avoid allocations *)
  let pixels =
    match t.working_pixels with
    | Some work when Array.length work = total_width * total_height ->
        (* Reuse existing buffer - copy previous frame data if available *)
        (match t.previous_pixels with
        | Some prev -> Array.blit prev 0 work 0 (Array.length prev)
        | None -> Array.fill work 0 (Array.length work) t.config.theme.bg);
        work
    | _ ->
        (* Need to allocate new buffer (size changed or first frame) *)
        let new_buffer =
          match t.previous_pixels with
          | Some prev when Array.length prev = total_width * total_height ->
              Array.copy prev (* Start with previous frame content *)
          | _ -> Array.make (total_width * total_height) t.config.theme.bg
        in
        t.working_pixels <- Some new_buffer;
        new_buffer
  in

  (* Calculate offsets - ensure at least the configured padding *)
  let x_offset = max t.config.padding ((total_width - terminal_width) / 2) in
  let y_offset = max t.config.padding ((total_height - terminal_height) / 2) in

  (* erase old cursor before we start painting *)
  (match t.prev_cursor with
  | -1, -1 -> ()
  | r, c ->
      let x0 = x_offset + (c * t.config.char_width) in
      let y0 = y_offset + (r * t.config.char_height) in
      for y = y0 to y0 + t.config.char_height - 1 do
        for x = x0 to x0 + t.config.char_width - 1 do
          let idx = (y * total_width) + x in
          pixels.(idx) <- t.config.theme.bg
        done
      done);

  (* Get current cursor position *)
  let cur_row, cur_col = Vte.cursor_pos t.vte in
  let pc_row, pc_col = t.prev_cursor in

  (* Use Grid_diff for efficient cell-level diffing *)
  let dirty_regions =
    match t.previous_grid with
    | None ->
        (* First frame - render everything *)
        [
          {
            Grid_diff.min_row = 0;
            max_row = rows - 1;
            min_col = 0;
            max_col = cols - 1;
          };
        ]
    | Some prev_grid ->
        (* Compute differences *)
        let _dirty_rows, region_changes =
          Grid_diff.diff prev_grid current_grid cols
        in
        let regions = List.map fst region_changes in

        (* Add cursor regions if cursor moved *)
        let cursor_regions =
          if
            (cur_row, cur_col) <> (pc_row, pc_col)
            && pc_row >= 0 && pc_row < rows && pc_col >= 0 && pc_col < cols
          then
            [
              {
                Grid_diff.min_row = pc_row;
                max_row = pc_row;
                min_col = pc_col;
                max_col = pc_col;
              };
              {
                Grid_diff.min_row = cur_row;
                max_row = cur_row;
                min_col = cur_col;
                max_col = cur_col;
              };
            ]
          else []
        in

        regions @ cursor_regions
  in

  (* Render only dirty regions *)
  List.iter
    (fun region ->
      for row = region.Grid_diff.min_row to region.max_row do
        for col = region.min_col to region.max_col do
          match Vte.get_cell t.vte ~row ~col with
          | None -> ()
          | Some cell -> (
              (* Render the cell *)
              let style = cell.style in
              let fg_ansi, bg_ansi =
                if style.reversed then (style.bg, style.fg)
                else (style.fg, style.bg)
              in
              (* For background, Default should map to theme.bg, not theme.fg *)
              let fg_color = color_of_ansi t.config.theme fg_ansi in
              let bg_color =
                match bg_ansi with
                | Ansi.Default -> t.config.theme.bg
                | _ -> color_of_ansi t.config.theme bg_ansi
              in

              (* Fill background *)
              let x_start = x_offset + (col * t.config.char_width) in
              let y_start = y_offset + (row * t.config.char_height) in
              for y = y_start to y_start + t.config.char_height - 1 do
                for x = x_start to x_start + t.config.char_width - 1 do
                  if x >= 0 && x < total_width && y >= 0 && y < total_height
                  then pixels.((y * total_width) + x) <- bg_color
                done
              done;

              (* Render character *)
              match t.font_renderer with
              | BuiltinFont ->
                  let ch =
                    try Uchar.to_char cell.char with Invalid_argument _ -> '?'
                    (* Non-ASCII character *)
                  in
                  Font.render_char pixels total_width total_height x_start
                    y_start ch fg_color bg_color t.config.char_width
                    t.config.char_height
              | TrueTypeFont fonts -> (
                  (* Select appropriate font based on style *)
                  let ft =
                    match (style.bold, style.italic) with
                    | true, true -> fonts.bold_italic
                    | true, false -> fonts.bold
                    | false, true -> fonts.italic
                    | false, false -> fonts.regular
                  in
                  (* Render using FreeType *)
                  let unicode = Uchar.to_int cell.char in
                  try
                    let bitmap_str, metrics, pitch =
                      Freetype.load_and_render_char ft unicode
                    in

                    if metrics.width > 0 && metrics.height > 0 then
                      (* Calculate baseline position *)
                      let baseline_y =
                        y_start + t.config.char_height
                        - (t.config.char_height / 4)
                      in
                      let glyph_x = x_start + metrics.bitmap_left in
                      let glyph_y = baseline_y - metrics.bitmap_top in

                      (* Render the glyph bitmap *)
                      for row = 0 to metrics.height - 1 do
                        for col = 0 to metrics.width - 1 do
                          let px = glyph_x + col in
                          let py = glyph_y + row in
                          if
                            px >= 0 && px < total_width && py >= 0
                            && py < total_height
                          then
                            let idx = (row * pitch) + col in
                            let alpha = Char.code bitmap_str.[idx] in
                            if alpha > 0 then
                              (* Only draw within the cell bounds *)
                              if
                                px >= x_start
                                && px < x_start + t.config.char_width
                                && py >= y_start
                                && py < y_start + t.config.char_height
                              then
                                let pixel_idx = (py * total_width) + px in
                                (* Blend with existing pixel based on alpha *)
                                if alpha = 255 then
                                  (* Fully opaque - just set the color *)
                                  pixels.(pixel_idx) <- fg_color
                                else
                                  (* Alpha blend *)
                                  let bg = pixels.(pixel_idx) in
                                  let a = float_of_int alpha /. 255.0 in
                                  let inv_a = 1.0 -. a in
                                  (* Extract RGB components *)
                                  let fg_r, fg_g, fg_b = fg_color in
                                  let bg_r, bg_g, bg_b = bg in
                                  (* Blend each component *)
                                  let r =
                                    int_of_float
                                      ((float_of_int fg_r *. a)
                                      +. (float_of_int bg_r *. inv_a))
                                  in
                                  let g =
                                    int_of_float
                                      ((float_of_int fg_g *. a)
                                      +. (float_of_int bg_g *. inv_a))
                                  in
                                  let b =
                                    int_of_float
                                      ((float_of_int fg_b *. a)
                                      +. (float_of_int bg_b *. inv_a))
                                  in
                                  pixels.(pixel_idx) <- (r, g, b)
                        done
                      done
                  with exn ->
                    (* Fallback to builtin font if character not found *)
                    Log.warn (fun m ->
                        m "Failed to render character U+%04X: %s" unicode
                          (Printexc.to_string exn));
                    let ch =
                      try Uchar.to_char cell.char
                      with Invalid_argument _ -> '?'
                      (* Non-ASCII character *)
                    in
                    Font.render_char pixels total_width total_height x_start
                      y_start ch fg_color bg_color t.config.char_width
                      t.config.char_height))
        done
      done)
    dirty_regions;

  (* Draw cursor if visible *)
  (if Vte.is_cursor_visible t.vte then
     let cursor_row, cursor_col = Vte.cursor_pos t.vte in
     let x_start = x_offset + (cursor_col * t.config.char_width) in
     let y_start = y_offset + (cursor_row * t.config.char_height) in
     (* Draw cursor as a solid block using foreground color *)
     for
       y = y_start to min (y_start + t.config.char_height - 1) (total_height - 1)
     do
       for
         x = x_start to min (x_start + t.config.char_width - 1) (total_width - 1)
       do
         if x >= 0 && y >= 0 then
           let idx = (y * total_width) + x in
           pixels.(idx) <- t.config.theme.fg
       done
     done);

  (* Update full dimensions if needed *)
  if t.full_width = 0 then (
    t.full_width <- total_width;
    t.full_height <- total_height);

  (* Check for differences with previous frame *)
  match t.previous_pixels with
  | None ->
      (* First frame - store full frame *)
      let pixel_bytes = Bytes.create (total_width * total_height * 3) in
      Array.iteri
        (fun i (r, g, b) ->
          Bytes.set pixel_bytes (i * 3) (Char.chr r);
          Bytes.set pixel_bytes ((i * 3) + 1) (Char.chr g);
          Bytes.set pixel_bytes ((i * 3) + 2) (Char.chr b))
        pixels;

      let frame =
        {
          x_offset = 0;
          y_offset = 0;
          width = total_width;
          height = total_height;
          pixel_data = pixel_bytes;
          transparent_color = None;
          delay_cs = 0;
          (* First frame has no delay *)
        }
      in
      t.frames <- frame :: t.frames;
      (* Copy working buffer to previous only when we have changes *)
      t.previous_pixels <- Some (Array.copy pixels);
      t.previous_grid <- Some current_grid;
      t.prev_cursor <- (cur_row, cur_col);
      (* Don't reset pending_delay_cs so second frame gets the accumulated time *)
      ()
  | Some prev_pixels -> (
      (* Find bounding box of changes *)
      match find_diff_bounds prev_pixels pixels total_width total_height with
      | None ->
          (* No visual changes, but we might have accumulated delay to preserve *)
          if t.pending_delay_cs > 0 then (
            (* Emit a 1x1 transparent frame to hold the delay *)
            let pixel_bytes = Bytes.create 3 in
            let r, g, b = t.config.theme.bg in
            Bytes.set pixel_bytes 0 (Char.chr r);
            Bytes.set pixel_bytes 1 (Char.chr g);
            Bytes.set pixel_bytes 2 (Char.chr b);

            let frame =
              {
                x_offset = 0;
                y_offset = 0;
                width = 1;
                height = 1;
                pixel_data = pixel_bytes;
                transparent_color = Some t.config.theme.bg;
                delay_cs = max 2 t.pending_delay_cs;
              }
            in
            t.frames <- frame :: t.frames;
            t.pending_delay_cs <- 0)
      | Some (x, y, w, h) ->
          (* Extract only the changed region *)
          let pixel_bytes = Bytes.create (w * h * 3) in

          for dy = 0 to h - 1 do
            for dx = 0 to w - 1 do
              let src_idx = ((y + dy) * total_width) + (x + dx) in
              let dst_idx = (dy * w) + dx in
              let r, g, b = pixels.(src_idx) in
              Bytes.set pixel_bytes (dst_idx * 3) (Char.chr r);
              Bytes.set pixel_bytes ((dst_idx * 3) + 1) (Char.chr g);
              Bytes.set pixel_bytes ((dst_idx * 3) + 2) (Char.chr b)
            done
          done;

          let frame =
            {
              x_offset = x;
              y_offset = y;
              width = w;
              height = h;
              pixel_data = pixel_bytes;
              transparent_color = None;
              delay_cs = max 2 t.pending_delay_cs;
              (* Use accumulated delay *)
            }
          in
          t.frames <- frame :: t.frames;
          (* Copy working buffer to previous since we found changes *)
          t.previous_pixels <- Some (Array.copy pixels);
          t.previous_grid <- Some current_grid;
          t.prev_cursor <- (cur_row, cur_col);
          t.pending_delay_cs <- 0)

let render t =
  let start_time = Unix.gettimeofday () in
  (* If we have accumulated delay but no final frame to hold it, capture one *)
  if t.pending_delay_cs > 0 then capture_frame t;

  match List.rev t.frames with
  | [] -> ""
  | frames ->
      Printf.eprintf "[TIMING] GIF render: %d frames\n" (List.length frames);
      (* Collect all unique colors from all frames, including transparent color *)
      let all_colors = Hashtbl.create 256 in
      let transparent_color = ref None in

      List.iter
        (fun frame ->
          (* Add transparent color if present *)
          (match frame.transparent_color with
          | Some (r, g, b) ->
              transparent_color := Some (Gif.rgb r g b);
              Hashtbl.replace all_colors (r, g, b) ()
          | None -> ());

          (* Add all pixels from frame *)
          for i = 0 to (Bytes.length frame.pixel_data / 3) - 1 do
            let r = Char.code (Bytes.get frame.pixel_data (i * 3)) in
            let g = Char.code (Bytes.get frame.pixel_data ((i * 3) + 1)) in
            let b = Char.code (Bytes.get frame.pixel_data ((i * 3) + 2)) in
            Hashtbl.replace all_colors (r, g, b) ()
          done)
        frames;

      (* Create color palette *)
      let palette_start = Unix.gettimeofday () in
      let color_tuple_array =
        Hashtbl.fold (fun color () acc -> color :: acc) all_colors []
        |> Array.of_list
      in

      let palette_result = Gif.Quantize.create_palette color_tuple_array 256 in
      let palette =
        match palette_result with
        | Ok p -> p
        | Error _ -> failwith "Failed to create palette"
      in
      Printf.eprintf "[TIMING] GIF palette creation: %.3fs\n"
        (Unix.gettimeofday () -. palette_start);

      (* Find color index in palette *)
      let find_color_index palette color =
        let palette_array = Gif.palette_to_array palette in
        let rec find idx =
          if idx >= Array.length palette_array then None
          else if palette_array.(idx) = color then Some idx
          else find (idx + 1)
        in
        find 0
      in

      (* Get background color for disposal *)
      let bg_r, bg_g, bg_b = t.config.theme.bg in
      let bg_color = Gif.rgb bg_r bg_g bg_b in

      (* Create color lookup cache for faster conversion *)
      let color_cache = Hashtbl.create 4096 in

      (* Helper to find color index with caching *)
      let find_cached_color_index (r, g, b) =
        let key = (r lsl 16) lor (g lsl 8) lor b in
        match Hashtbl.find_opt color_cache key with
        | Some idx -> idx
        | None ->
            let idx =
              match Gif.Quantize.find_nearest_color (r, g, b) palette with
              | idx -> idx
            in
            Hashtbl.add color_cache key idx;
            idx
      in

      (* Convert all frames *)
      let convert_start = Unix.gettimeofday () in
      let gif_frames =
        List.map
          (fun frame ->
            (* Convert RGB to indexed directly without intermediate array *)
            let num_pixels = frame.width * frame.height in
            let indexed_data = Bytes.create num_pixels in

            (* Process pixels directly from source bytes *)
            for i = 0 to num_pixels - 1 do
              let r = Char.code (Bytes.get frame.pixel_data (i * 3)) in
              let g = Char.code (Bytes.get frame.pixel_data ((i * 3) + 1)) in
              let b = Char.code (Bytes.get frame.pixel_data ((i * 3) + 2)) in
              let idx = find_cached_color_index (r, g, b) in
              Bytes.set indexed_data i (Char.chr idx)
            done;

            (* Create GIF frame *)
            {
              Gif.width = frame.width;
              height = frame.height;
              x_offset = frame.x_offset;
              y_offset = frame.y_offset;
              delay_cs = frame.delay_cs;
              disposal = Gif.Do_not_dispose;
              transparent_index = None;
              (* Disable transparency to avoid display issues *)
              pixels = indexed_data;
              local_palette = None;
            })
          frames
      in
      Printf.eprintf "[TIMING] GIF frame conversion: %.3fs\n"
        (Unix.gettimeofday () -. convert_start);

      (* Create GIF data structure with infinite loop *)
      (* Find the background color index in palette *)
      let bg_index = find_color_index palette bg_color in
      let background_index = Option.value bg_index ~default:0 in

      let gif =
        match
          Gif.gif_create ~width:t.full_width ~height:t.full_height ~palette
            ~background_index ~frames:gif_frames ~loop_count:0 ()
        with
        | Ok g -> g
        | Error _ -> failwith "Failed to create GIF"
      in

      (* Encode to binary *)
      let encode_start = Unix.gettimeofday () in
      let result =
        match Gif.encode gif with
        | Ok data -> data
        | Error _ -> failwith "Failed to encode GIF"
      in
      Printf.eprintf "[TIMING] GIF encoding: %.3fs\n"
        (Unix.gettimeofday () -. encode_start);
      Printf.eprintf "[TIMING] GIF render total: %.3fs\n"
        (Unix.gettimeofday () -. start_time);
      result

let add_pending_delay t delay_seconds =
  let added = int_of_float ((delay_seconds *. 100.) +. 0.5) in
  (* Round to nearest *)
  t.pending_delay_cs <- t.pending_delay_cs + added

let frame_count t = List.length t.frames

type frame_info = { width : int; height : int; x_offset : int; y_offset : int }

let get_frame_info t idx =
  match List.nth_opt t.frames idx with
  | None -> None
  | Some f ->
      Some
        {
          width = f.width;
          height = f.height;
          x_offset = f.x_offset;
          y_offset = f.y_offset;
        }

let render_streaming t out_channel =
  let start_time = Unix.gettimeofday () in
  (* If we have accumulated delay but no final frame to hold it, capture one *)
  if t.pending_delay_cs > 0 then capture_frame t;

  match List.rev t.frames with
  | [] -> ()
  | frames ->
      Printf.eprintf "[TIMING] GIF render streaming: %d frames\n"
        (List.length frames);
      (* Collect all unique colors from all frames *)
      let all_colors = Hashtbl.create 256 in
      List.iter
        (fun (fd : frame_data) ->
          (* Add all pixels from frame *)
          for i = 0 to (Bytes.length fd.pixel_data / 3) - 1 do
            let r = Char.code (Bytes.get fd.pixel_data (i * 3)) in
            let g = Char.code (Bytes.get fd.pixel_data ((i * 3) + 1)) in
            let b = Char.code (Bytes.get fd.pixel_data ((i * 3) + 2)) in
            Hashtbl.replace all_colors (r, g, b) ()
          done)
        frames;

      (* Create color palette *)
      let palette_start = Unix.gettimeofday () in
      let color_tuple_array =
        Hashtbl.fold (fun color () acc -> color :: acc) all_colors []
        |> Array.of_list
      in
      let palette_result = Gif.Quantize.create_palette color_tuple_array 256 in
      let palette =
        match palette_result with
        | Ok p -> p
        | Error _ -> failwith "Failed to create palette"
      in
      Printf.eprintf "[TIMING] GIF palette creation: %.3fs\n"
        (Unix.gettimeofday () -. palette_start);

      (* Find background color index *)
      let bg_r, bg_g, bg_b = t.config.theme.bg in
      let bg_color = Gif.rgb bg_r bg_g bg_b in
      let find_color_index palette color =
        let palette_array = Gif.palette_to_array palette in
        let rec find idx =
          if idx >= Array.length palette_array then None
          else if palette_array.(idx) = color then Some idx
          else find (idx + 1)
        in
        find 0
      in
      let bg_index = find_color_index palette bg_color in
      let background_index = Option.value bg_index ~default:0 in

      (* Create color lookup cache for faster conversion *)
      let color_cache = Hashtbl.create 4096 in
      let find_cached_color_index (r, g, b) =
        let key = (r lsl 16) lor (g lsl 8) lor b in
        match Hashtbl.find_opt color_cache key with
        | Some idx -> idx
        | None ->
            let idx = Gif.Quantize.find_nearest_color (r, g, b) palette in
            Hashtbl.add color_cache key idx;
            idx
      in

      (* Convert all frames *)
      let convert_start = Unix.gettimeofday () in
      let gif_frames =
        List.map
          (fun (fd : frame_data) ->
            (* Convert RGB to indexed *)
            let num_pixels = fd.width * fd.height in
            let indexed_data = Bytes.create num_pixels in

            for i = 0 to num_pixels - 1 do
              let r = Char.code (Bytes.get fd.pixel_data (i * 3)) in
              let g = Char.code (Bytes.get fd.pixel_data ((i * 3) + 1)) in
              let b = Char.code (Bytes.get fd.pixel_data ((i * 3) + 2)) in
              let idx = find_cached_color_index (r, g, b) in
              Bytes.set indexed_data i (Char.chr idx)
            done;

            ({
               width = fd.width;
               height = fd.height;
               x_offset = fd.x_offset;
               y_offset = fd.y_offset;
               delay_cs = fd.delay_cs;
               disposal = Gif.Do_not_dispose;
               transparent_index = None;
               pixels = indexed_data;
               local_palette = None;
             }
              : Gif.frame))
          frames
      in
      Printf.eprintf "[TIMING] GIF frame conversion: %.3fs\n"
        (Unix.gettimeofday () -. convert_start);

      (* Create GIF data structure *)
      let gif =
        match
          Gif.gif_create ~width:t.full_width ~height:t.full_height ~palette
            ~background_index ~frames:gif_frames ~loop_count:0 ()
        with
        | Ok g -> g
        | Error _ -> failwith "Failed to create GIF"
      in

      (* Encode to output channel using streaming *)
      let encode_start = Unix.gettimeofday () in
      let writer buf ofs len = output out_channel buf ofs len in
      (match Gif.encode_streaming gif ~writer with
      | Ok () -> ()
      | Error _ -> failwith "Failed to encode GIF");

      Printf.eprintf "[TIMING] GIF encoding: %.3fs\n"
        (Unix.gettimeofday () -. encode_start);
      Printf.eprintf "[TIMING] GIF render streaming total: %.3fs\n"
        (Unix.gettimeofday () -. start_time)
