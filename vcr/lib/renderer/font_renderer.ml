(** Common font rendering functionality *)

let src = Logs.Src.create "vcr.renderer.font" ~doc:"Font renderer"
module Log = (val Logs.src_log src : Logs.LOG)

type font_set = {
  regular : Freetype.t;
  bold : Freetype.t;
  italic : Freetype.t;
  bold_italic : Freetype.t;
}

type font_renderer =
  | Bitmap of {
      fonts : font_set;
      char_width : int;
      char_height : int;
    }
  | Freetype of font_set

let create_font_renderer font_path font_size =
  match font_path with
  | None ->
      (* Use built-in embedded JetBrains Mono fonts *)
      let regular_data = Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_regular in
      let bold_data = Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_bold in
      let italic_data = Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_italic in
      let bold_italic_data = Vcr_fonts.Embedded_fonts.Fonts.jetbrains_mono_bold_italic in
      
      let fonts = {
        regular = Freetype.create_from_memory ~font_data:regular_data ~pixel_size:font_size;
        bold = Freetype.create_from_memory ~font_data:bold_data ~pixel_size:font_size;
        italic = Freetype.create_from_memory ~font_data:italic_data ~pixel_size:font_size;
        bold_italic = Freetype.create_from_memory ~font_data:bold_italic_data ~pixel_size:font_size;
      } in
      (* Bitmap fonts use the config's char dimensions *)
      Bitmap { fonts; char_width = 10; char_height = 20 }
  | Some path ->
      (* Load TrueType font *)
      let regular = Freetype.create ~font_path:path ~pixel_size:font_size in
      let bold = 
        try Freetype.create ~font_path:(Filename.remove_extension path ^ "-Bold" ^ Filename.extension path) ~pixel_size:font_size
        with _ -> regular
      in
      let italic = 
        try Freetype.create ~font_path:(Filename.remove_extension path ^ "-Italic" ^ Filename.extension path) ~pixel_size:font_size
        with _ -> regular
      in
      let bold_italic = 
        try Freetype.create ~font_path:(Filename.remove_extension path ^ "-BoldItalic" ^ Filename.extension path) ~pixel_size:font_size
        with _ -> bold
      in
      Freetype { regular; bold; italic; bold_italic }

let get_char_dimensions = function
  | Bitmap { char_width; char_height; _ } -> (char_width, char_height)
  | Freetype _ -> (10, 20) (* Default dimensions for Freetype fonts *)

let select_font font_renderer style =
  let fonts = match font_renderer with
    | Bitmap { fonts; _ } -> fonts
    | Freetype fonts -> fonts
  in
  (* Access style fields directly - Vte.style = Vte.Cell.style so fields should be accessible *)
  match (style.Vte.Cell.bold, style.Vte.Cell.italic) with
  | true, true -> fonts.bold_italic
  | true, false -> fonts.bold
  | false, true -> fonts.italic
  | false, false -> fonts.regular

(* Render a character using the font renderer *)
let render_char pixels width height x y char fg_color _bg_color char_width char_height =
  (* This is a simplified version - in reality we'd use the font to render *)
  (* For now, just fill the character cell with foreground color for non-space chars *)
  if char <> Uchar.of_char ' ' then
    for dy = 0 to char_height - 1 do
      for dx = 0 to char_width - 1 do
        let px = x + dx in
        let py = y + dy in
        if px >= 0 && px < width && py >= 0 && py < height then
          let idx = (py * width) + px in
          pixels.(idx) <- fg_color
      done
    done