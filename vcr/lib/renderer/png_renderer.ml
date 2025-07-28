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
}

type t = {
  vte : Vte.t;
  config : config;
  font_renderer : Font_renderer.font_renderer;
  mutable captured : bool;
  mutable pixels : (int * int * int) array option;
  mutable width : int;
  mutable height : int;
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

let create vte config =
  (* Initialize font renderer based on config *)
  let font_renderer = create_font_renderer config.font_path config.font_size in
  {
    vte;
    config;
    font_renderer;
    captured = false;
    pixels = None;
    width = 0;
    height = 0;
  }

let capture_frame t =
  let rows = Vte.rows t.vte in
  let cols = Vte.cols t.vte in

  (* Use target dimensions if specified, otherwise calculate from terminal size *)
  let term_width, term_height =
    match (t.config.target_width, t.config.target_height) with
    | Some w, Some h ->
        let available_width = w - (2 * t.config.padding) in
        let available_height = h - (2 * t.config.padding) in
        (available_width, available_height)
    | _ -> (cols * t.config.char_width, rows * t.config.char_height)
  in

  let full_width = term_width + (2 * t.config.padding) in
  let full_height = term_height + (2 * t.config.padding) in

  (* Create pixel buffer *)
  let pixels = Array.make (full_width * full_height) t.config.theme.bg in

  (* Get character dimensions *)
  let char_width, char_height = get_char_dimensions t.font_renderer in

  (* Render each cell *)
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      match Vte.get_cell t.vte ~row ~col with
      | None -> ()
      | Some cell ->
          let x = t.config.padding + (col * char_width) in
          let y = t.config.padding + (row * char_height) in

          (* Get colors *)
          let fg_color =
            get_color t.config.theme cell.style.fg cell.style.reversed true
          in
          let bg_color =
            get_color t.config.theme cell.style.bg cell.style.reversed false
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
          render_char pixels full_width full_height x y cell.char fg_color
            bg_color char_width char_height
    done
  done;

  (* Draw cursor if visible *)
  let cur_row, cur_col = Vte.cursor_pos t.vte in
  (if
     Vte.is_cursor_visible t.vte
     && cur_row >= 0 && cur_row < rows && cur_col >= 0 && cur_col < cols
   then
     let x = t.config.padding + (cur_col * char_width) in
     let y = t.config.padding + (cur_row * char_height) in

     (* Simple block cursor using foreground color *)
     for dy = 0 to char_height - 1 do
       for dx = 0 to char_width - 1 do
         let px = x + dx in
         let py = y + dy in
         if px >= 0 && px < full_width && py >= 0 && py < full_height then
           let idx = (py * full_width) + px in
           pixels.(idx) <- t.config.theme.fg
       done
     done);

  t.captured <- true;
  t.pixels <- Some pixels;
  t.width <- full_width;
  t.height <- full_height

let add_pending_delay _ _delay =
  (* PNG doesn't need delays - it's a single frame *)
  ()

let render t =
  if not t.captured then capture_frame t;

  match t.pixels with
  | None -> ""
  | Some pixels ->
      (* Convert pixel array to 2D array for PNG *)
      let pixel_array =
        Array.init t.height (fun y ->
            Array.init t.width (fun x -> pixels.((y * t.width) + x)))
      in

      (* Create PNG image *)
      let img = Png.rgb_of_pixels ~width:t.width ~height:t.height pixel_array in

      (* Encode to PNG *)
      Png.bytes_of_png img |> Bytes.to_string
