(** GIF renderer for vcr - renders terminal output as animated GIF *)

let src = Logs.Src.create "vcr.renderer.gif" ~doc:"GIF renderer"

module Log = (val Logs.src_log src : Logs.LOG)

(* Global timing reference for performance tracking *)
let global_timing : Vcr_common.Timing.t option ref = ref None
let set_timing t = global_timing := t

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
  theme : theme;
  font_path : string option;  (** Path to font file, None for built-in font *)
  font_size : int;  (** Font size in pixels *)
  target_width : int option;  (** Desired output width in pixels *)
  target_height : int option;  (** Desired output height in pixels *)
  padding : int;  (** Padding around terminal content in pixels *)
}

(* Use common font types from Font_renderer module *)
open Font_renderer

type frame_data = {
  x_offset : int;
  y_offset : int;
  width : int;
  height : int;
  pixel_data : bytes;
  delay_cs : int; (* Delay in centiseconds (1/100th of a second) *)
}

type glyph_key = {
  text : string;
  fg : int * int * int;
  bg : int * int * int;
  style_hash : int;
}
[@@warning "-69"]

(** Convert ANSI color to RGB *)
let rec color_of_ansi (theme : theme) (c : Ansi.color) =
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
  | RGBA (r, g, b, _a) ->
      (* For GIF, we ignore alpha and just use RGB *)
      (r, g, b)

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
      let bits = bitmap.(row) in
      for col = 0 to 4 do
        if bits land (1 lsl (4 - col)) <> 0 then
          (* Draw scaled pixel *)
          for dy = 0 to scale_y - 1 do
            for dx = 0 to scale_x - 1 do
              let px = x + (col * scale_x) + dx in
              let py = y + (row * scale_y) + dy in
              if px >= 0 && px < width && py >= 0 && py < height then
                let idx = (py * width) + px in
                if idx >= 0 && idx < Array.length pixels then
                  pixels.(idx) <- fg_color
            done
          done
      done
    done
end

(** Find bounding box of differences between two pixel arrays *)
let find_diff_bounds prev_pixels curr_pixels width height =
  let min_x = ref width in
  let min_y = ref height in
  let max_x = ref (-1) in
  let max_y = ref (-1) in

  let prev_len = Array.length prev_pixels in
  let curr_len = Array.length curr_pixels in
  let expected_len = width * height in

  if prev_len <> expected_len || curr_len <> expected_len then
    failwith
      (Printf.sprintf
         "find_diff_bounds: array size mismatch. Expected %d, got prev=%d, \
          curr=%d (width=%d, height=%d)"
         expected_len prev_len curr_len width height);

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

(** ========== NEW PURE IMPLEMENTATION ========== *)

(** Render a cell to pixels *)
let render_cell ~config ~font_renderer ~glyph_cache ~pixels ~width ~height
    ~x_offset ~y_offset ~row ~col ~cell =
  let x_start = x_offset + (col * config.char_width) in
  let y_start = y_offset + (row * config.char_height) in

  match cell with
  | None ->
      (* Empty cell - fill with background *)
      for y = 0 to config.char_height - 1 do
        for x = 0 to config.char_width - 1 do
          if x_start + x < width && y_start + y >= 0 then
            let idx = ((y_start + y) * width) + (x_start + x) in
            if idx >= 0 && idx < Array.length pixels then
              pixels.(idx) <- config.theme.bg
        done
      done
  | Some cell -> (
      if Grid.Cell.is_empty cell then
        (* Empty cell - fill with background *)
        for y = 0 to config.char_height - 1 do
          for x = 0 to config.char_width - 1 do
            if x_start + x < width && y_start + y >= 0 then
              let idx = ((y_start + y) * width) + (x_start + x) in
              if idx >= 0 && idx < Array.length pixels then
                pixels.(idx) <- config.theme.bg
          done
        done
      else if Grid.Cell.is_continuation cell then
        (* Continuation cell - just fill background *)
        let style = Grid.Cell.get_style cell in
        let bg = color_of_ansi config.theme (Ansi.Style.bg style) in
        for y = 0 to config.char_height - 1 do
          for x = 0 to config.char_width - 1 do
            if x_start + x < width && y_start + y >= 0 then
              let idx = ((y_start + y) * width) + (x_start + x) in
              if idx >= 0 && idx < Array.length pixels then pixels.(idx) <- bg
          done
        done
      else (* Glyph *)
        let text = Grid.Cell.get_text cell in
        let style = Grid.Cell.get_style cell in
        let fg = color_of_ansi config.theme (Ansi.Style.fg style) in
        let bg = color_of_ansi config.theme (Ansi.Style.bg style) in

        (* Create cache key *)
        let key = { text; fg; bg; style_hash = Hashtbl.hash style } in

        (* Check cache first *)
        match Hashtbl.find_opt glyph_cache key with
        | Some cached ->
            (* Found in cache - copy to output pixels *)
            (* Log.debug (fun m -> m "Glyph cache hit for '%s'" cached_key.text); *)
            for y = 0 to config.char_height - 1 do
              for x = 0 to config.char_width - 1 do
                let src_idx = ((y * config.char_width) + x) * 3 in
                let dst_x = x_start + x in
                let dst_y = y_start + y in
                if dst_x >= 0 && dst_x < width && dst_y >= 0 && dst_y < height
                then
                  let dst_idx = (dst_y * width) + dst_x in
                  let r = int_of_float cached.(src_idx) in
                  let g = int_of_float cached.(src_idx + 1) in
                  let b = int_of_float cached.(src_idx + 2) in
                  pixels.(dst_idx) <- (r, g, b)
              done
            done
        | None ->
            (* Not in cache - render and cache it *)
            (* Render glyph into temporary buffer *)
            let render_glyph_to_buffer () =
              let temp_pixels =
                Array.make (config.char_width * config.char_height) bg
              in

              (if String.length text = 0 then
                 (* Empty text - render space *)
                 Font.render_char temp_pixels config.char_width
                   config.char_height 0 0 ' ' fg bg config.char_width
                   config.char_height
               else
                 (* Check if it's ASCII *)
                 let first_byte = String.get text 0 in
                 if Char.code first_byte < 128 && String.length text = 1 then
                   (* ASCII character - render directly *)
                   Font.render_char temp_pixels config.char_width
                     config.char_height 0 0 first_byte fg bg config.char_width
                     config.char_height
                 else
                   (* Non-ASCII Unicode character - render placeholder *)
                   (* TODO: Implement proper Unicode font rendering *)
                   Font.render_char temp_pixels config.char_width
                     config.char_height 0 0 '?' fg bg config.char_width
                     config.char_height);

              (* Convert to float array for caching *)
              let result =
                Array.create_float (config.char_width * config.char_height * 3)
              in
              for i = 0 to (config.char_width * config.char_height) - 1 do
                let r, g, b = temp_pixels.(i) in
                let base = i * 3 in
                result.(base) <- float_of_int r;
                result.(base + 1) <- float_of_int g;
                result.(base + 2) <- float_of_int b
              done;
              result
            in

            let glyph_pixels =
              match font_renderer with
              | Bitmap _ -> render_glyph_to_buffer ()
              | Freetype fonts -> (
                  (* Select appropriate font based on style *)
                  let style = Grid.Cell.get_style cell in
                  let ft =
                    match (Ansi.Style.bold style, Ansi.Style.italic style) with
                    | true, true -> fonts.bold_italic
                    | true, false -> fonts.bold
                    | false, true -> fonts.italic
                    | false, false -> fonts.regular
                  in
                  (* Initialize result with background color *)
                  let result =
                    Array.create_float
                      (config.char_width * config.char_height * 3)
                  in
                  let bg_r, bg_g, bg_b = bg in
                  for i = 0 to (config.char_width * config.char_height) - 1 do
                    let base = i * 3 in
                    result.(base) <- float_of_int bg_r;
                    result.(base + 1) <- float_of_int bg_g;
                    result.(base + 2) <- float_of_int bg_b
                  done;

                  (* Render using FreeType *)
                  let ch =
                    if String.length text > 0 then
                      Uchar.of_int (Char.code text.[0])
                    else Uchar.of_int 0x20 (* space *)
                  in
                  let unicode = Uchar.to_int ch in
                  try
                    let bitmap_str, metrics, pitch =
                      Freetype.load_and_render_char ft unicode
                    in

                    Log.debug (fun m ->
                        m
                          "Rendering char U+%04X '%c': metrics width=%d \
                           height=%d"
                          unicode
                          (Char.chr (min 127 unicode))
                          metrics.width metrics.height);

                    (if metrics.width > 0 && metrics.height > 0 then
                       (* Calculate baseline position relative to cell origin *)
                       let baseline_y =
                         config.char_height - (config.char_height / 4)
                       in
                       let glyph_x = metrics.bitmap_left in
                       let glyph_y = baseline_y - metrics.bitmap_top in

                       (* Render the glyph bitmap into result buffer *)
                       for row = 0 to metrics.height - 1 do
                         for col = 0 to metrics.width - 1 do
                           let px = glyph_x + col in
                           let py = glyph_y + row in
                           if
                             px >= 0 && px < config.char_width && py >= 0
                             && py < config.char_height
                           then
                             let idx = (row * pitch) + col in
                             let alpha = Char.code bitmap_str.[idx] in
                             if alpha > 0 then (
                               let pixel_idx = (py * config.char_width) + px in
                               let base_idx = pixel_idx * 3 in
                               (* Blend with existing pixel based on alpha *)
                               if alpha = 255 then (
                                 (* Fully opaque - just set the color *)
                                 let fg_r, fg_g, fg_b = fg in
                                 result.(base_idx) <- float_of_int fg_r;
                                 result.(base_idx + 1) <- float_of_int fg_g;
                                 result.(base_idx + 2) <- float_of_int fg_b)
                               else
                                 (* Alpha blend *)
                                 let a = float_of_int alpha /. 255.0 in
                                 let inv_a = 1.0 -. a in
                                 (* Extract RGB components *)
                                 let fg_r, fg_g, fg_b = fg in
                                 (* Blend each component *)
                                 result.(base_idx) <-
                                   (float_of_int fg_r *. a)
                                   +. (result.(base_idx) *. inv_a);
                                 result.(base_idx + 1) <-
                                   (float_of_int fg_g *. a)
                                   +. (result.(base_idx + 1) *. inv_a);
                                 result.(base_idx + 2) <-
                                   (float_of_int fg_b *. a)
                                   +. (result.(base_idx + 2) *. inv_a))
                         done
                       done);
                    result
                  with exn ->
                    (* Fallback to builtin font if character not found *)
                    Log.warn (fun m ->
                        m "Failed to render character U+%04X: %s" unicode
                          (Printexc.to_string exn));
                    render_glyph_to_buffer ())
            in
            (* Cache the rendered glyph *)
            Hashtbl.add glyph_cache key glyph_pixels;

            (* Copy cached glyph to output pixels *)
            for y = 0 to config.char_height - 1 do
              for x = 0 to config.char_width - 1 do
                let src_idx = ((y * config.char_width) + x) * 3 in
                let dst_x = x_start + x in
                let dst_y = y_start + y in
                if dst_x >= 0 && dst_x < width && dst_y >= 0 && dst_y < height
                then
                  let dst_idx = (dst_y * width) + dst_x in
                  let r = int_of_float glyph_pixels.(src_idx) in
                  let g = int_of_float glyph_pixels.(src_idx + 1) in
                  let b = int_of_float glyph_pixels.(src_idx + 2) in
                  pixels.(dst_idx) <- (r, g, b)
              done
            done)

(** Render a single frame to pixel array *)
let render_frame_to_pixels ~config ~font_renderer ~glyph_cache
    ~(frame : Renderer_intf.frame) ~width ~height ~x_offset ~y_offset =
  let pixels = Array.make (width * height) config.theme.bg in

  (* Render all cells *)
  let rows = Grid.rows frame.grid in
  let cols = Grid.cols frame.grid in

  (* Debug logging *)
  if rows <= 0 || cols <= 0 then
    failwith
      (Printf.sprintf "render_frame_to_pixels: Invalid grid dimensions: %dx%d"
         rows cols);

  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      match Grid.get frame.grid ~row ~col with
      | None -> ()
      | Some cell ->
          render_cell ~config ~font_renderer ~glyph_cache ~pixels ~width ~height
            ~x_offset ~y_offset ~row ~col ~cell:(Some cell)
    done
  done;

  (* Draw cursor if visible *)
  (if
     frame.cursor_visible && frame.cursor_row >= 0 && frame.cursor_row < rows
     && frame.cursor_col >= 0 && frame.cursor_col < cols
   then
     let x_start = x_offset + (frame.cursor_col * config.char_width) in
     let y_start = y_offset + (frame.cursor_row * config.char_height) in

     (* Draw cursor as inverted colors or block *)
     for y = 0 to config.char_height - 1 do
       for x = 0 to config.char_width - 1 do
         if x_start + x < width && y_start + y >= 0 then
           let idx = ((y_start + y) * width) + (x_start + x) in
           if idx >= 0 && idx < Array.length pixels then
             let r, g, b = pixels.(idx) in
             (* Invert colors for cursor *)
             pixels.(idx) <- (255 - r, 255 - g, 255 - b)
       done
     done);

  pixels

(** Convert frames to frame_data for GIF encoding *)
let frames_to_frame_data ~config ~font_renderer ~glyph_cache ~frames =
  (* Calculate dimensions *)
  let rows, cols =
    match frames with
    | [] -> (24, 80) (* Default terminal size *)
    | frame :: _ ->
        (Grid.rows frame.Renderer_intf.grid, Grid.cols frame.Renderer_intf.grid)
  in

  let terminal_width = cols * config.char_width in
  let terminal_height = rows * config.char_height in

  let total_width =
    match config.target_width with
    | Some w -> w
    | None -> terminal_width + (2 * config.padding)
  in
  let total_height =
    match config.target_height with
    | Some h -> h
    | None -> terminal_height + (2 * config.padding)
  in

  let x_offset = (total_width - terminal_width) / 2 in
  let y_offset = (total_height - terminal_height) / 2 in

  (* Render each frame *)
  let rec process_frames frames prev_pixels prev_cursor acc =
    match frames with
    | [] -> List.rev acc
    | frame :: rest -> (
        (* For first frame or when we have dirty regions, render incrementally *)
        let pixels =
          match prev_pixels with
          | None ->
              (* First frame - render everything *)
              render_frame_to_pixels ~config ~font_renderer ~glyph_cache ~frame
                ~width:total_width ~height:total_height ~x_offset ~y_offset
          | Some prev when frame.dirty_regions = [] && not frame.cursor_moved ->
              (* No changes - reuse previous pixels *)
              prev
          | Some prev ->
              (* Have previous frame - render only dirty regions *)
              let pixels = Array.copy prev in
              (* Add cursor regions if cursor moved *)
              let regions =
                if frame.cursor_moved then
                  match prev_cursor with
                  | Some (prev_row, prev_col) ->
                      (* Add regions for old and new cursor positions *)
                      let old_cursor_region =
                        {
                          Grid.min_row = prev_row;
                          max_row = prev_row;
                          min_col = prev_col;
                          max_col = prev_col;
                        }
                      in
                      let new_cursor_region =
                        {
                          Grid.min_row = frame.cursor_row;
                          max_row = frame.cursor_row;
                          min_col = frame.cursor_col;
                          max_col = frame.cursor_col;
                        }
                      in
                      old_cursor_region :: new_cursor_region
                      :: frame.dirty_regions
                  | None -> frame.dirty_regions
                else frame.dirty_regions
              in
              (* Render the dirty regions *)
              let rows = Grid.rows frame.grid in
              let cols = Grid.cols frame.grid in
              List.iter
                (fun region ->
                  for row = region.Grid.min_row to region.max_row do
                    for col = region.min_col to region.max_col do
                      if row >= 0 && row < rows && col >= 0 && col < cols then
                        match Grid.get frame.grid ~row ~col with
                        | None -> ()
                        | Some cell ->
                            render_cell ~config ~font_renderer ~glyph_cache
                              ~pixels ~width:total_width ~height:total_height
                              ~x_offset ~y_offset ~row ~col ~cell:(Some cell)
                    done
                  done)
                regions;
              (* Draw cursor if visible *)
              (if
                 frame.cursor_visible && frame.cursor_row >= 0
                 && frame.cursor_row < rows && frame.cursor_col >= 0
                 && frame.cursor_col < cols
               then
                 let x_start =
                   x_offset + (frame.cursor_col * config.char_width)
                 in
                 let y_start =
                   y_offset + (frame.cursor_row * config.char_height)
                 in
                 (* Draw cursor as inverted colors or block *)
                 for y = 0 to config.char_height - 1 do
                   for x = 0 to config.char_width - 1 do
                     if x_start + x < total_width && y_start + y >= 0 then
                       let idx =
                         ((y_start + y) * total_width) + (x_start + x)
                       in
                       if idx >= 0 && idx < Array.length pixels then
                         let r, g, b = pixels.(idx) in
                         (* Invert colors for cursor *)
                         pixels.(idx) <- (255 - r, 255 - g, 255 - b)
                   done
                 done);
              pixels
        in

        (* Calculate diff region *)
        let diff_region =
          match prev_pixels with
          | None -> Some (0, 0, total_width, total_height)
          | Some prev -> find_diff_bounds prev pixels total_width total_height
        in

        match diff_region with
        | None -> (
            (* No changes - accumulate delay to next frame if there is one *)
            match acc with
            | [] ->
                process_frames rest (Some pixels)
                  (Some (frame.cursor_row, frame.cursor_col))
                  acc
            | (hd : frame_data) :: tl ->
                let updated =
                  { hd with delay_cs = hd.delay_cs + frame.delay_cs }
                in
                process_frames rest (Some pixels)
                  (Some (frame.cursor_row, frame.cursor_col))
                  (updated :: tl))
        | Some (x, y, w, h) ->
            (* Extract pixel data for changed region *)
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

            let fd =
              {
                x_offset = x;
                y_offset = y;
                width = w;
                height = h;
                pixel_data = pixel_bytes;
                delay_cs = frame.delay_cs;
              }
            in
            process_frames rest (Some pixels)
              (Some (frame.cursor_row, frame.cursor_col))
              (fd :: acc))
  in

  let frame_data_list = process_frames frames None None [] in
  (frame_data_list, total_width, total_height)

type t = {
  rows : int;
  cols : int;
  config : config;
  total_width : int;
  total_height : int;
  font_renderer : font_renderer;
  mutable encoder : Gif.streaming_encoder option;
  mutable first_frame_buffer : Buffer.t option;
  glyph_cache : (glyph_key, float array) Hashtbl.t;
  mutable color_cache : (int, int) Hashtbl.t option;
  mutable prev_pixels : (int * int * int) array option;
  mutable prev_cursor : (int * int) option;
  mutable frame_count : int;
}
[@@warning "-69"]
(* Suppress unused field warnings *)

(** Create a comprehensive terminal color palette *)
let create_terminal_palette (theme : theme) =
  (* Build a comprehensive palette that includes:
     - Theme colors (16 standard + bright colors)
     - 216-color cube (6x6x6)
     - 24 grayscale colors
     Total: 256 colors *)
  let colors = ref [] in

  (* Add theme colors *)
  colors := theme.bg :: !colors;
  colors := theme.fg :: !colors;
  colors := theme.black :: !colors;
  colors := theme.red :: !colors;
  colors := theme.green :: !colors;
  colors := theme.yellow :: !colors;
  colors := theme.blue :: !colors;
  colors := theme.magenta :: !colors;
  colors := theme.cyan :: !colors;
  colors := theme.white :: !colors;
  colors := theme.bright_black :: !colors;
  colors := theme.bright_red :: !colors;
  colors := theme.bright_green :: !colors;
  colors := theme.bright_yellow :: !colors;
  colors := theme.bright_blue :: !colors;
  colors := theme.bright_magenta :: !colors;
  colors := theme.bright_cyan :: !colors;
  colors := theme.bright_white :: !colors;

  (* Add 216-color cube (6x6x6) *)
  for r = 0 to 5 do
    for g = 0 to 5 do
      for b = 0 to 5 do
        let r' = if r = 0 then 0 else 55 + (r * 40) in
        let g' = if g = 0 then 0 else 55 + (g * 40) in
        let b' = if b = 0 then 0 else 55 + (b * 40) in
        colors := (r', g', b') :: !colors
      done
    done
  done;

  (* Add 24 grayscale colors *)
  for i = 0 to 23 do
    let gray = 8 + (i * 10) in
    colors := (gray, gray, gray) :: !colors
  done;

  (* Convert to array and limit to 256 colors *)
  let color_list = List.rev !colors in
  let color_array_full = Array.of_list color_list in
  let len = min (Array.length color_array_full) 256 in
  let color_array =
    Array.sub color_array_full 0 len
    |> Array.map (fun (r, g, b) -> Gif.rgb r g b)
  in

  match Gif.palette_of_array color_array with
  | Ok p -> p
  | Error _ -> failwith "Failed to create terminal palette"

(** Pre-compute color lookup table for quantization *)
let create_color_lookup_table palette =
  let cache = Hashtbl.create 65536 in
  (* Pre-populate with common terminal colors *)
  let common_colors =
    [
      (0, 0, 0);
      (255, 255, 255);
      (* Black and white *)
      (23, 23, 23);
      (221, 221, 221);
      (* Default bg/fg *)
      (128, 128, 128);
      (192, 192, 192);
      (* Grays *)
    ]
  in
  List.iter
    (fun (r, g, b) ->
      let key = (r lsl 16) lor (g lsl 8) lor b in
      let idx = Gif.Quantize.find_nearest_color (r, g, b) palette in
      Hashtbl.add cache key idx)
    common_colors;
  cache

(** Find the index of a color in the palette *)
let find_color_index palette (r, g, b) =
  Gif.Quantize.find_nearest_color (r, g, b) palette

let create ~rows ~cols config =
  let font_renderer = create_font_renderer config.font_path config.font_size in

  (* Calculate total dimensions based on rows/cols *)
  let term_width, term_height =
    match (config.target_width, config.target_height) with
    | Some w, Some h ->
        let available_width = w - (2 * config.padding) in
        let available_height = h - (2 * config.padding) in
        (available_width, available_height)
    | _ -> (cols * config.char_width, rows * config.char_height)
  in
  let total_width = term_width + (2 * config.padding) in
  let total_height = term_height + (2 * config.padding) in

  {
    rows;
    cols;
    config;
    total_width;
    total_height;
    font_renderer;
    encoder = None;
    first_frame_buffer = None;
    glyph_cache = Hashtbl.create 1024;
    color_cache = None;
    prev_pixels = None;
    prev_cursor = None;
    frame_count = 0;
  }

let rec write_frame t frame ~incremental ~writer =
  (* Create encoder on first frame *)
  match t.encoder with
  | None -> (
      (* Create a comprehensive palette for terminal colors *)
      let palette = create_terminal_palette t.config.theme in

      (* Find background color index *)
      let bg_r, bg_g, bg_b = t.config.theme.bg in
      let bg_index = find_color_index palette (bg_r, bg_g, bg_b) in

      (* Create and store color cache *)
      t.color_cache <- Some (create_color_lookup_table palette);

      (* For the first frame, we need to buffer the GIF header until it's written *)
      let buffer = Buffer.create 1024 in
      t.first_frame_buffer <- Some buffer;

      match
        Gif.create_streaming_encoder ~width:t.total_width ~height:t.total_height
          ~palette ~background_index:bg_index ~loop_count:0 ()
      with
      | Error err ->
          failwith
            (Printf.sprintf "Failed to create encoder: %s"
               (Gif.string_of_error err))
      | Ok encoder ->
          t.encoder <- Some encoder;
          write_frame t frame ~incremental ~writer)
  | Some encoder ->
      (* For first frame, always render full frame *)
      let is_first_frame = t.frame_count = 0 in
      t.frame_count <- t.frame_count + 1;

      (* Compute terminal dimensions *)
      let x_offset = (t.total_width - (t.cols * t.config.char_width)) / 2 in
      let y_offset = (t.total_height - (t.rows * t.config.char_height)) / 2 in

      (* Render frame data *)
      let frame_data_list =
        match !global_timing with
        | None ->
            if is_first_frame then (
              (* First frame - must be full *)
              let pixels =
                render_frame_to_pixels ~config:t.config
                  ~font_renderer:t.font_renderer ~glyph_cache:t.glyph_cache
                  ~frame ~width:t.total_width ~height:t.total_height ~x_offset
                  ~y_offset
              in
              t.prev_pixels <- Some pixels;
              t.prev_cursor <- Some (frame.cursor_row, frame.cursor_col);
              (* Convert entire frame to bytes *)
              let pixel_bytes =
                Bytes.create (t.total_width * t.total_height * 3)
              in
              for y = 0 to t.total_height - 1 do
                for x = 0 to t.total_width - 1 do
                  let idx = (y * t.total_width) + x in
                  let r, g, b = pixels.(idx) in
                  Bytes.set pixel_bytes (idx * 3) (Char.chr r);
                  Bytes.set pixel_bytes ((idx * 3) + 1) (Char.chr g);
                  Bytes.set pixel_bytes ((idx * 3) + 2) (Char.chr b)
                done
              done;
              [
                {
                  x_offset = 0;
                  y_offset = 0;
                  width = t.total_width;
                  height = t.total_height;
                  pixel_data = pixel_bytes;
                  delay_cs = frame.delay_cs;
                };
              ])
            else
              (* Subsequent frames - compute diff *)
              frames_to_frame_data ~config:t.config
                ~font_renderer:t.font_renderer ~glyph_cache:t.glyph_cache
                ~frames:[ frame ]
              |> fun (fd_list, _, _) ->
              (* Update state *)
              (match fd_list with
              | [] -> ()
              | _ ->
                  (* Render this frame to update prev_pixels *)
                  let pixels =
                    render_frame_to_pixels ~config:t.config
                      ~font_renderer:t.font_renderer ~glyph_cache:t.glyph_cache
                      ~frame ~width:t.total_width ~height:t.total_height
                      ~x_offset ~y_offset
                  in
                  t.prev_pixels <- Some pixels;
                  t.prev_cursor <- Some (frame.cursor_row, frame.cursor_col));
              fd_list
        | Some timing ->
            Vcr_common.Timing.with_timing timing "  Render pixels" (fun () ->
                if is_first_frame then (
                  (* First frame - must be full *)
                  let pixels =
                    render_frame_to_pixels ~config:t.config
                      ~font_renderer:t.font_renderer ~glyph_cache:t.glyph_cache
                      ~frame ~width:t.total_width ~height:t.total_height
                      ~x_offset ~y_offset
                  in
                  t.prev_pixels <- Some pixels;
                  t.prev_cursor <- Some (frame.cursor_row, frame.cursor_col);
                  (* Convert entire frame to bytes *)
                  let pixel_bytes =
                    Bytes.create (t.total_width * t.total_height * 3)
                  in
                  for y = 0 to t.total_height - 1 do
                    for x = 0 to t.total_width - 1 do
                      let idx = (y * t.total_width) + x in
                      let r, g, b = pixels.(idx) in
                      Bytes.set pixel_bytes (idx * 3) (Char.chr r);
                      Bytes.set pixel_bytes ((idx * 3) + 1) (Char.chr g);
                      Bytes.set pixel_bytes ((idx * 3) + 2) (Char.chr b)
                    done
                  done;
                  [
                    {
                      x_offset = 0;
                      y_offset = 0;
                      width = t.total_width;
                      height = t.total_height;
                      pixel_data = pixel_bytes;
                      delay_cs = frame.delay_cs;
                    };
                  ])
                else
                  (* Subsequent frames - only render dirty regions *)
                  match t.prev_pixels with
                  | None -> failwith "No previous frame state"
                  | Some prev_pixels ->
                      (* Compute regions to update *)
                      let regions =
                        if frame.cursor_moved then
                          match t.prev_cursor with
                          | Some (prev_row, prev_col) ->
                              (* Add regions for old and new cursor positions *)
                              let old_cursor_region =
                                {
                                  Grid.min_row = prev_row;
                                  max_row = prev_row;
                                  min_col = prev_col;
                                  max_col = prev_col;
                                }
                              in
                              let new_cursor_region =
                                {
                                  Grid.min_row = frame.cursor_row;
                                  max_row = frame.cursor_row;
                                  min_col = frame.cursor_col;
                                  max_col = frame.cursor_col;
                                }
                              in
                              old_cursor_region :: new_cursor_region
                              :: frame.dirty_regions
                          | None -> frame.dirty_regions
                        else frame.dirty_regions
                      in

                      (* Fast path: if no regions changed, return empty *)
                      if regions = [] && not frame.cursor_visible then (
                        t.prev_cursor <-
                          Some (frame.cursor_row, frame.cursor_col);
                        [])
                      else
                        (* Calculate bounding box of all dirty regions *)
                        let min_x = ref t.total_width in
                        let min_y = ref t.total_height in
                        let max_x = ref (-1) in
                        let max_y = ref (-1) in

                        (* Update bounds for each dirty region *)
                        List.iter
                          (fun region ->
                            let x1 =
                              x_offset
                              + (region.Grid.min_col * t.config.char_width)
                            in
                            let y1 =
                              y_offset
                              + (region.Grid.min_row * t.config.char_height)
                            in
                            let x2 =
                              x_offset
                              + ((region.Grid.max_col + 1) * t.config.char_width)
                              - 1
                            in
                            let y2 =
                              y_offset
                              + (region.Grid.max_row + 1) * t.config.char_height
                              - 1
                            in
                            min_x := min !min_x x1;
                            min_y := min !min_y y1;
                            max_x := max !max_x x2;
                            max_y := max !max_y y2)
                          regions;

                        (* Include cursor if visible *)
                        if
                          frame.cursor_visible && frame.cursor_row >= 0
                          && frame.cursor_row < t.rows && frame.cursor_col >= 0
                          && frame.cursor_col < t.cols
                        then (
                          let x1 =
                            x_offset + (frame.cursor_col * t.config.char_width)
                          in
                          let y1 =
                            y_offset + (frame.cursor_row * t.config.char_height)
                          in
                          let x2 = x1 + t.config.char_width - 1 in
                          let y2 = y1 + t.config.char_height - 1 in
                          min_x := min !min_x x1;
                          min_y := min !min_y y1;
                          max_x := max !max_x x2;
                          max_y := max !max_y y2);

                        (* Ensure bounds are within frame *)
                        min_x := max 0 !min_x;
                        min_y := max 0 !min_y;
                        max_x := min (t.total_width - 1) !max_x;
                        max_y := min (t.total_height - 1) !max_y;

                        if !max_x < !min_x || !max_y < !min_y then (
                          (* No actual changes *)
                          t.prev_cursor <-
                            Some (frame.cursor_row, frame.cursor_col);
                          [])
                        else
                          (* Copy only the changed region from prev_pixels *)
                          let w = !max_x - !min_x + 1 in
                          let h = !max_y - !min_y + 1 in
                          let pixels = Array.make (w * h) t.config.theme.bg in

                          (* Copy previous pixels in the region *)
                          for dy = 0 to h - 1 do
                            for dx = 0 to w - 1 do
                              let src_idx =
                                ((!min_y + dy) * t.total_width) + (!min_x + dx)
                              in
                              let dst_idx = (dy * w) + dx in
                              pixels.(dst_idx) <- prev_pixels.(src_idx)
                            done
                          done;

                          (* Render only the dirty cells in this region *)
                          List.iter
                            (fun region ->
                              for row = region.Grid.min_row to region.max_row do
                                for col = region.min_col to region.max_col do
                                  if
                                    row >= 0 && row < t.rows && col >= 0
                                    && col < t.cols
                                  then
                                    match Grid.get frame.grid ~row ~col with
                                    | None -> ()
                                    | Some cell ->
                                        let cell_x =
                                          x_offset + (col * t.config.char_width)
                                        in
                                        let cell_y =
                                          y_offset + (row * t.config.char_height)
                                        in
                                        (* Only render if cell overlaps our region *)
                                        if
                                          cell_x < !min_x + w
                                          && cell_x + t.config.char_width
                                             > !min_x
                                          && cell_y < !min_y + h
                                          && cell_y + t.config.char_height
                                             > !min_y
                                        then
                                          render_cell ~config:t.config
                                            ~font_renderer:t.font_renderer
                                            ~glyph_cache:t.glyph_cache ~pixels
                                            ~width:w ~height:h
                                            ~x_offset:(- !min_x)
                                            ~y_offset:(- !min_y) ~row ~col
                                            ~cell:(Some cell)
                                done
                              done)
                            regions;

                          (* Draw cursor if visible and in region *)
                          (if
                             frame.cursor_visible && frame.cursor_row >= 0
                             && frame.cursor_row < t.rows
                             && frame.cursor_col >= 0
                             && frame.cursor_col < t.cols
                           then
                             let cursor_x =
                               x_offset
                               + (frame.cursor_col * t.config.char_width)
                             in
                             let cursor_y =
                               y_offset
                               + (frame.cursor_row * t.config.char_height)
                             in
                             if
                               cursor_x < !min_x + w
                               && cursor_x + t.config.char_width > !min_x
                               && cursor_y < !min_y + h
                               && cursor_y + t.config.char_height > !min_y
                             then
                               for y = 0 to t.config.char_height - 1 do
                                 for x = 0 to t.config.char_width - 1 do
                                   let px = cursor_x + x - !min_x in
                                   let py = cursor_y + y - !min_y in
                                   if px >= 0 && px < w && py >= 0 && py < h
                                   then
                                     let idx = (py * w) + px in
                                     let r, g, b = pixels.(idx) in
                                     pixels.(idx) <- (255 - r, 255 - g, 255 - b)
                                 done
                               done);

                          (* Convert to bytes *)
                          let pixel_bytes = Bytes.create (w * h * 3) in
                          for i = 0 to (w * h) - 1 do
                            let r, g, b = pixels.(i) in
                            Bytes.set pixel_bytes (i * 3) (Char.chr r);
                            Bytes.set pixel_bytes ((i * 3) + 1) (Char.chr g);
                            Bytes.set pixel_bytes ((i * 3) + 2) (Char.chr b)
                          done;

                          (* Update state - we need to apply changes back to prev_pixels *)
                          for dy = 0 to h - 1 do
                            for dx = 0 to w - 1 do
                              let src_idx = (dy * w) + dx in
                              let dst_idx =
                                ((!min_y + dy) * t.total_width) + (!min_x + dx)
                              in
                              prev_pixels.(dst_idx) <- pixels.(src_idx)
                            done
                          done;
                          t.prev_cursor <-
                            Some (frame.cursor_row, frame.cursor_col);

                          [
                            {
                              x_offset = !min_x;
                              y_offset = !min_y;
                              width = w;
                              height = h;
                              pixel_data = pixel_bytes;
                              delay_cs = frame.delay_cs;
                            };
                          ])
      in

      (* Get the palette we created for the encoder *)
      let palette = create_terminal_palette t.config.theme in

      (* Use persistent color cache *)
      let color_cache =
        match t.color_cache with
        | Some cache -> cache
        | None ->
            (* Should not happen, but create one if needed *)
            let cache = create_color_lookup_table palette in
            t.color_cache <- Some cache;
            cache
      in
      let find_cached_color r g b =
        let key = (r lsl 16) lor (g lsl 8) lor b in
        match Hashtbl.find_opt color_cache key with
        | Some idx -> idx
        | None ->
            let idx = Gif.Quantize.find_nearest_color (r, g, b) palette in
            Hashtbl.add color_cache key idx;
            idx
      in

      (* Process each frame data *)
      List.iter
        (fun (fd : frame_data) ->
          (* Convert RGB pixels to indexed color *)
          let indexed_pixels =
            match !global_timing with
            | None ->
                let indexed_pixels = Bytes.create (fd.width * fd.height) in
                for i = 0 to (Bytes.length fd.pixel_data / 3) - 1 do
                  let r = Char.code (Bytes.get fd.pixel_data (i * 3)) in
                  let g = Char.code (Bytes.get fd.pixel_data ((i * 3) + 1)) in
                  let b = Char.code (Bytes.get fd.pixel_data ((i * 3) + 2)) in
                  let idx = find_cached_color r g b in
                  Bytes.set indexed_pixels i (Char.chr idx)
                done;
                indexed_pixels
            | Some timing ->
                Vcr_common.Timing.with_timing timing "  Color quantization"
                  (fun () ->
                    let indexed_pixels = Bytes.create (fd.width * fd.height) in
                    for i = 0 to (Bytes.length fd.pixel_data / 3) - 1 do
                      let r = Char.code (Bytes.get fd.pixel_data (i * 3)) in
                      let g =
                        Char.code (Bytes.get fd.pixel_data ((i * 3) + 1))
                      in
                      let b =
                        Char.code (Bytes.get fd.pixel_data ((i * 3) + 2))
                      in
                      let idx = find_cached_color r g b in
                      Bytes.set indexed_pixels i (Char.chr idx)
                    done;
                    indexed_pixels)
          in

          (* Create GIF frame *)
          let gif_frame =
            {
              Gif.width = fd.width;
              height = fd.height;
              x_offset = fd.x_offset;
              y_offset = fd.y_offset;
              delay_cs = fd.delay_cs;
              disposal = Gif.Do_not_dispose;
              transparent_index = None;
              pixels = indexed_pixels;
              local_palette = None;
            }
          in

          (* Write frame *)
          let write_frame () =
            match Gif.write_frame_streaming encoder gif_frame ~writer with
            | Error err ->
                failwith
                  (Printf.sprintf "Failed to write frame: %s"
                     (Gif.string_of_error err))
            | Ok () -> ()
          in
          match !global_timing with
          | None -> write_frame ()
          | Some timing ->
              Vcr_common.Timing.with_timing timing "  Write GIF frame"
                write_frame)
        frame_data_list

let finalize t ~writer =
  match t.encoder with
  | None ->
      (* No frames were written *)
      ()
  | Some encoder -> (
      (* Finalize the streaming encoder *)
      match Gif.finalize_streaming_encoder encoder ~writer with
      | Error err ->
          failwith
            (Printf.sprintf "Failed to finalize GIF: %s"
               (Gif.string_of_error err))
      | Ok () -> ())
