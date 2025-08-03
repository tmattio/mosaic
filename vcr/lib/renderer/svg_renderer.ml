open Gg
open Vg

type theme = {
  fg : color;
  bg : color;
  black : color;
  red : color;
  green : color;
  yellow : color;
  blue : color;
  magenta : color;
  cyan : color;
  white : color;
  bright_black : color;
  bright_red : color;
  bright_green : color;
  bright_yellow : color;
  bright_blue : color;
  bright_magenta : color;
  bright_cyan : color;
  bright_white : color;
}

let default_theme =
  {
    fg = Color.v_srgb 0.694 0.714 0.757;
    (* #abb2bf *)
    bg = Color.v_srgb 0.157 0.173 0.204;
    (* #282c34 *)
    black = Color.v_srgb 0.157 0.173 0.204;
    red = Color.v_srgb 0.878 0.424 0.459;
    green = Color.v_srgb 0.600 0.765 0.475;
    yellow = Color.v_srgb 0.900 0.753 0.482;
    blue = Color.v_srgb 0.380 0.686 0.937;
    magenta = Color.v_srgb 0.776 0.471 0.867;
    cyan = Color.v_srgb 0.337 0.714 0.761;
    white = Color.v_srgb 0.694 0.714 0.757;
    bright_black = Color.v_srgb 0.361 0.388 0.439;
    bright_red = Color.v_srgb 0.878 0.424 0.459;
    bright_green = Color.v_srgb 0.600 0.765 0.475;
    bright_yellow = Color.v_srgb 0.900 0.753 0.482;
    bright_blue = Color.v_srgb 0.380 0.686 0.937;
    bright_magenta = Color.v_srgb 0.776 0.471 0.867;
    bright_cyan = Color.v_srgb 0.337 0.714 0.761;
    bright_white = Color.v_srgb 1.0 1.0 1.0;
  }

type config = {
  font_family : string;
  font_size : float;
  line_height : float;
  theme : theme;
}

type t = { rows : int; cols : int; config : config }

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
  | RGB (r, g, b) ->
      Color.v_srgb (float r /. 255.) (float g /. 255.) (float b /. 255.)
  | Index i ->
      (* Fallback for 256 colors - could be improved with a color map *)
      if i < 8 then
        (* Map to standard colors *)
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
          | _ -> Default)
      else if i < 16 then
        (* Map to bright colors *)
        color_of_ansi theme
          (match i - 8 with
          | 0 -> Bright_black
          | 1 -> Bright_red
          | 2 -> Bright_green
          | 3 -> Bright_yellow
          | 4 -> Bright_blue
          | 5 -> Bright_magenta
          | 6 -> Bright_cyan
          | 7 -> Bright_white
          | _ -> Default)
      else
        (* For now, just use default color for indices 16-255 *)
        theme.fg
  | RGBA (r, g, b, _a) ->
      (* SVG doesn't support alpha in text fill, so we ignore it *)
      Color.v_srgb (float r /. 255.) (float g /. 255.) (float b /. 255.)

let create ~rows ~cols config = { rows; cols; config }

let render_single_frame ~rows ~cols config image writer =
  (* Use the view size from the config *)
  let view_width = float cols *. config.font_size *. 0.6 in
  let view_height = float rows *. config.font_size *. config.line_height in
  let size = Size2.v view_width view_height in
  let view = Box2.v P2.o size in

  (* Render to SVG *)
  let renderable = (size, view, image) in
  let buf = Buffer.create 1024 in
  let r = Vgr.create (Vgr_svg.target ()) (`Buffer buf) in
  ignore (Vgr.render r (`Image renderable));
  ignore (Vgr.render r `End);

  (* Write buffer contents using writer function *)
  let contents = Buffer.to_bytes buf in
  writer contents 0 (Bytes.length contents)

(** Render a frame to SVG image *)
let render_frame ~config ~frame =
  let rows = Grid.rows frame.Renderer_intf.grid in
  let cols = Grid.cols frame.Renderer_intf.grid in
  if rows = 0 || cols = 0 then None
  else
    let char_width = config.font_size *. 0.6 in
    (* Monospace font aspect ratio approximation *)
    let line_height = config.font_size *. config.line_height in
    let view_width = float cols *. char_width in
    let view_height = float rows *. line_height in
    (* Start with a background for the whole frame *)
    let background = I.const config.theme.bg in

    (* Create images for each cell and blend them *)
    let cell_images =
      let images = ref [] in
      for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
          match Grid.get frame.Renderer_intf.grid ~row:r ~col:c with
          | None -> ()
          | Some cell ->
              let style = Grid.Cell.get_style cell in
              let fg_ansi, bg_ansi =
                if Ansi.Style.reversed style then
                  (Ansi.Style.bg style, Ansi.Style.fg style)
                else (Ansi.Style.fg style, Ansi.Style.bg style)
              in
              let fg_color = color_of_ansi config.theme fg_ansi in
              let bg_color = color_of_ansi config.theme bg_ansi in

              (* Position of the bottom-left of the cell *)
              let x = float c *. char_width in
              let y = view_height -. (float (r + 1) *. line_height) in
              let pos = V2.v x y in

              (* Add cell background if not default *)
              (if bg_color <> config.theme.bg then
                 let bg_box = Box2.v pos (Size2.v char_width line_height) in
                 let bg_img =
                   I.const bg_color |> I.cut (P.rect bg_box P.empty)
                 in
                 images := bg_img :: !images);

              (* Create font for Vg *)
              let font =
                {
                  Font.name = config.font_family;
                  slant = (if Ansi.Style.italic style then `Italic else `Normal);
                  weight = (if Ansi.Style.bold style then `W700 else `W400);
                  size = config.font_size;
                }
              in
              (* NOTE: Vg expects glyph IDs. We simplify by using the Uchar.to_int.
               A real implementation would use a font layout library like otf-layout
               to get correct glyph IDs and advances. *)
              let text = Grid.Cell.get_text cell in
              let ch =
                if String.length text > 0 then Uchar.of_int (Char.code text.[0])
                else Uchar.of_int 0x20 (* space *)
              in
              let glyph = Uchar.to_int ch in
              let char_pos =
                V2.add pos
                  (V2.v 0. (line_height -. config.font_size) (* baseline adj *))
              in

              let char_image =
                I.const fg_color
                |> I.cut_glyphs ~text font [ glyph ]
                |> I.move char_pos
              in
              images := char_image :: !images
        done
      done;
      List.rev !images
    in

    (* Draw cursor *)
    let cursor_image =
      if frame.Renderer_intf.cursor_visible then
        let r = frame.Renderer_intf.cursor_row in
        let c = frame.Renderer_intf.cursor_col in
        let x = float c *. char_width in
        let y = view_height -. (float (r + 1) *. line_height) in
        let cursor_box = Box2.v (V2.v x y) (Size2.v char_width line_height) in
        [ I.const config.theme.fg |> I.cut (P.rect cursor_box P.empty) ]
      else []
    in

    (* Blend everything together: background -> cells -> cursor *)
    let final_image =
      List.fold_left I.blend background (cell_images @ cursor_image)
    in
    (* Crop the image to the view size *)
    let view = Box2.v P2.o (Size2.v view_width view_height) in
    let final_image = I.cut (P.rect view P.empty) final_image in
    Some final_image

let write_frame t frame ~incremental ~writer =
  (* For SVG, incremental could be used to decide whether to 
     render as animated SVG with multiple frames or single frame.
     Currently we just render single frames. *)
  let _ = incremental in
  match render_frame ~config:t.config ~frame with
  | Some img ->
      render_single_frame ~rows:t.rows ~cols:t.cols t.config img writer
  | None -> ()

let finalize _t ~writer:_ =
  (* Nothing to do for SVG - already written *)
  ()
