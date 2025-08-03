(** PNG renderer for vcr - renders terminal output as PNG screenshots *)

let src = Logs.Src.create "vcr.renderer.png" ~doc:"PNG renderer"

module Log = (val Logs.src_log src : Logs.LOG)

type config = {
  char_width : int;  (** Width of a character in pixels *)
  char_height : int;  (** Height of a character in pixels *)
  theme : Gif_renderer.theme;  (** Reuse GIF renderer's theme *)
  font_path : string option;  (** Path to font file, None for built-in font *)
  font_size : int;  (** Font size in pixels *)
  target_width : int option;  (** Desired output width in pixels *)
  target_height : int option;  (** Desired output height in pixels *)
  padding : int;  (** Padding around terminal content in pixels *)
  output_dir : string;  (** Directory where PNG files will be written *)
}

type t = {
  rows : int;
  cols : int;
  config : config;
  font_renderer : Font_renderer.font_renderer;
  mutable frame_number : int;
}

(* Reuse font rendering logic from common module *)
let render_char = Font_renderer.render_char
let get_char_dimensions = Font_renderer.get_char_dimensions
let create_font_renderer = Font_renderer.create_font_renderer

(* Helper to get color from theme *)
let get_color theme color reversed is_fg =
  let actual_color =
    if reversed then if is_fg then color else color else color
  in
  match actual_color with
  | Ansi.Default ->
      if is_fg then theme.Gif_renderer.fg else theme.Gif_renderer.bg
  | Ansi.Black ->
      if not reversed then theme.Gif_renderer.black
      else theme.Gif_renderer.white
  | Ansi.Red -> theme.Gif_renderer.red
  | Ansi.Green -> theme.Gif_renderer.green
  | Ansi.Yellow -> theme.Gif_renderer.yellow
  | Ansi.Blue -> theme.Gif_renderer.blue
  | Ansi.Magenta -> theme.Gif_renderer.magenta
  | Ansi.Cyan -> theme.Gif_renderer.cyan
  | Ansi.White ->
      if not reversed then theme.Gif_renderer.white
      else theme.Gif_renderer.black
  | Ansi.Bright_black -> theme.Gif_renderer.bright_black
  | Ansi.Bright_red -> theme.Gif_renderer.bright_red
  | Ansi.Bright_green -> theme.Gif_renderer.bright_green
  | Ansi.Bright_yellow -> theme.Gif_renderer.bright_yellow
  | Ansi.Bright_blue -> theme.Gif_renderer.bright_blue
  | Ansi.Bright_magenta -> theme.Gif_renderer.bright_magenta
  | Ansi.Bright_cyan -> theme.Gif_renderer.bright_cyan
  | Ansi.Bright_white -> theme.Gif_renderer.bright_white
  | Ansi.Index _ -> theme.Gif_renderer.fg (* Default for indexed colors *)
  | Ansi.RGB (r, g, b) -> (r, g, b)
  | Ansi.RGBA (r, g, b, _a) -> (r, g, b)
(* Ignore alpha for PNG renderer *)

let create ~rows ~cols config =
  (* Ensure output directory exists *)
  (try Unix.mkdir config.output_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (* Initialize font renderer based on config *)
  let font_renderer = create_font_renderer config.font_path config.font_size in
  { rows; cols; config; font_renderer; frame_number = 0 }

let render_frame_to_png ~config ~frame ~font_renderer =
  let grid = frame.Renderer_intf.grid in
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in

  (* Use target dimensions if specified, otherwise calculate from terminal size *)
  let term_width, term_height =
    match (config.target_width, config.target_height) with
    | Some w, Some h ->
        let available_width = w - (2 * config.padding) in
        let available_height = h - (2 * config.padding) in
        (available_width, available_height)
    | _ -> (cols * config.char_width, rows * config.char_height)
  in

  let full_width = term_width + (2 * config.padding) in
  let full_height = term_height + (2 * config.padding) in

  (* Create pixel buffer *)
  let pixels = Array.make (full_width * full_height) config.theme.bg in

  (* Get character dimensions *)
  let char_width, char_height = get_char_dimensions font_renderer in

  (* Render each cell *)
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      match Grid.get grid ~row ~col with
      | None -> ()
      | Some cell ->
          let x = config.padding + (col * char_width) in
          let y = config.padding + (row * char_height) in

          (* Get colors *)
          let style = Grid.Cell.get_style cell in
          let fg_color =
            get_color config.theme (Ansi.Style.fg style)
              (Ansi.Style.reversed style)
              true
          in
          let bg_color =
            get_color config.theme (Ansi.Style.bg style)
              (Ansi.Style.reversed style)
              false
          in

          (* Draw background *)
          for dy = 0 to char_height - 1 do
            for dx = 0 to char_width - 1 do
              let px = x + dx in
              let py = y + dy in
              if px >= 0 && px < full_width && py >= 0 && py < full_height then
                let idx = (py * full_width) + px in
                pixels.(idx) <- bg_color
            done
          done;

          (* Draw character *)
          let text = Grid.Cell.get_text cell in
          let ch =
            if String.length text > 0 then Uchar.of_int (Char.code text.[0])
            else Uchar.of_int 0x20 (* space *)
          in
          render_char pixels full_width full_height x y ch fg_color bg_color
            char_width char_height
    done
  done;

  (* Draw cursor if visible *)
  let cur_row = frame.cursor_row in
  let cur_col = frame.cursor_col in
  (if
     frame.cursor_visible && cur_row >= 0 && cur_row < rows && cur_col >= 0
     && cur_col < cols
   then
     let x = config.padding + (cur_col * char_width) in
     let y = config.padding + (cur_row * char_height) in

     (* Simple block cursor using foreground color *)
     for dy = 0 to char_height - 1 do
       for dx = 0 to char_width - 1 do
         let px = x + dx in
         let py = y + dy in
         if px >= 0 && px < full_width && py >= 0 && py < full_height then
           let idx = (py * full_width) + px in
           pixels.(idx) <- config.theme.fg
       done
     done);

  (* Convert pixel array to 2D array for PNG *)
  let pixel_array =
    Array.init full_height (fun y ->
        Array.init full_width (fun x -> pixels.((y * full_width) + x)))
  in

  (* Create PNG image *)
  let img =
    Png.rgb_of_pixels ~width:full_width ~height:full_height pixel_array
  in

  (* Encode to PNG *)
  Png.bytes_of_png img

let write_frame t frame ~incremental ~writer =
  let _ = incremental in
  (* Unused for PNG - each frame is independent *)

  (* Render the frame to PNG *)
  let png_bytes =
    render_frame_to_png ~config:t.config ~frame ~font_renderer:t.font_renderer
  in

  (* For multi-file mode, write to a separate file *)
  if t.config.output_dir <> "" then (
    let filename =
      Printf.sprintf "%s/frame_%04d.png" t.config.output_dir t.frame_number
    in
    let oc = open_out_bin filename in
    output_bytes oc png_bytes;
    close_out oc;
    t.frame_number <- t.frame_number + 1)
  else
    (* Single file mode - write using the writer function *)
    writer png_bytes 0 (Bytes.length png_bytes)

let finalize _t ~writer:_ =
  (* Nothing to finalize for PNG renderer *)
  ()
