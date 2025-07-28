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

type t = { vte : Vte.t; config : config; mutable frames : I.t list }

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

let create vte config = { vte; config; frames = [] }

let capture_frame t =
  let rows, cols = (Vte.rows t.vte, Vte.cols t.vte) in
  if rows = 0 || cols = 0 then ()
  else
    let char_width = t.config.font_size *. 0.6 in
    (* Monospace font aspect ratio approximation *)
    let line_height = t.config.font_size *. t.config.line_height in
    let view_width = float cols *. char_width in
    let view_height = float rows *. line_height in
    (* Start with a background for the whole frame *)
    let background = I.const t.config.theme.bg in

    (* Create images for each cell and blend them *)
    let cell_images =
      let images = ref [] in
      for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
          match Vte.get_cell t.vte ~row:r ~col:c with
          | None -> ()
          | Some cell ->
              let style = cell.attrs in
              let fg_ansi, bg_ansi =
                if style.reversed then (style.bg, style.fg)
                else (style.fg, style.bg)
              in
              let fg_color = color_of_ansi t.config.theme fg_ansi in
              let bg_color = color_of_ansi t.config.theme bg_ansi in

              (* Position of the bottom-left of the cell *)
              let x = float c *. char_width in
              let y = view_height -. (float (r + 1) *. line_height) in
              let pos = V2.v x y in

              (* Add cell background if not default *)
              (if bg_color <> t.config.theme.bg then
                 let bg_box = Box2.v pos (Size2.v char_width line_height) in
                 let bg_img =
                   I.const bg_color |> I.cut (P.rect bg_box P.empty)
                 in
                 images := bg_img :: !images);

              (* Create font for Vg *)
              let font =
                {
                  Font.name = t.config.font_family;
                  slant = (if style.italic then `Italic else `Normal);
                  weight = (if style.bold then `W700 else `W400);
                  size = t.config.font_size;
                }
              in
              (* NOTE: Vg expects glyph IDs. We simplify by using the Uchar.to_int.
               A real implementation would use a font layout library like otf-layout
               to get correct glyph IDs and advances. *)
              let ch =
                if String.length cell.glyph > 0 then
                  Uchar.of_int (Char.code cell.glyph.[0])
                else Uchar.of_int 0x20 (* space *)
              in
              let glyph = Uchar.to_int ch in
              let char_pos =
                V2.add pos
                  (V2.v 0.
                     (line_height -. t.config.font_size) (* baseline adj *))
              in

              let char_image =
                I.const fg_color
                |> I.cut_glyphs ~text:cell.glyph font [ glyph ]
                |> I.move char_pos
              in
              images := char_image :: !images
        done
      done;
      List.rev !images
    in

    (* Draw cursor *)
    let cursor_image =
      if Vte.is_cursor_visible t.vte then
        let r, c = Vte.cursor_pos t.vte in
        let x = float c *. char_width in
        let y = view_height -. (float (r + 1) *. line_height) in
        let cursor_box = Box2.v (V2.v x y) (Size2.v char_width line_height) in
        [ I.const t.config.theme.fg |> I.cut (P.rect cursor_box P.empty) ]
      else []
    in

    (* Blend everything together: background -> cells -> cursor *)
    let final_image =
      List.fold_left I.blend background (cell_images @ cursor_image)
    in
    (* Crop the image to the view size *)
    let view = Box2.v P2.o (Size2.v view_width view_height) in
    let final_image = I.cut (P.rect view P.empty) final_image in
    t.frames <- final_image :: t.frames

let add_pending_delay _ _ = () (* SVG doesn't use frame delays *)

let render t =
  let frames = List.rev t.frames in
  if frames = [] then ""
  else
    let first_frame_vte_rows = Vte.rows t.vte in
    let first_frame_vte_cols = Vte.cols t.vte in

    (* Determine image size. We use mm for physical size. *)
    let char_width = t.config.font_size *. 0.6 in
    let line_height = t.config.font_size *. t.config.line_height in
    let view_width = float first_frame_vte_cols *. char_width in
    let view_height = float first_frame_vte_rows *. line_height in
    let size = Size2.v (view_width *. 0.5) (view_height *. 0.5) in
    (* example scaling *)
    let view = Box2.v P2.o (Size2.v view_width view_height) in

    (* Create an animated SVG with all frames *)
    let buffer = Buffer.create 16384 in
    (* Calculate frame duration in seconds *)
    let frame_duration = 0.04 in
    (* 25 fps default *)
    let total_duration = float_of_int (List.length frames) *. frame_duration in

    (* Write SVG header *)
    Buffer.add_string buffer
      (Printf.sprintf
         {|<svg xmlns="http://www.w3.org/2000/svg" width="%g" height="%g" viewBox="0 0 %g %g">
|}
         view_width view_height view_width view_height);

    (* Write each frame as a group with animation *)
    List.iteri
      (fun i image ->
        let start_time = float_of_int i *. frame_duration in

        (* Create a group for this frame *)
        Buffer.add_string buffer
          (Printf.sprintf
             {|  <g opacity="0">
    <animate attributeName="opacity" 
             begin="%gs" 
             dur="%gs" 
             values="0;1;1;0" 
             keyTimes="0;0.01;0.99;1" 
             repeatCount="indefinite" 
             repeatDur="%gs" />
|}
             start_time frame_duration total_duration);

        (* Render this frame into a temporary buffer *)
        let frame_buffer = Buffer.create 4096 in
        let renderable = (size, view, image) in
        let r =
          Vgr.create (Vgr_svg.target ~xml_decl:false ()) (`Buffer frame_buffer)
        in
        (match Vgr.render r (`Image renderable) with
        | `Ok -> (
            match Vgr.render r `End with
            | `Ok ->
                let frame_svg = Buffer.contents frame_buffer in
                (* Extract just the content between <svg> tags *)
                let start_idx =
                  try String.index frame_svg '>' + 1 with Not_found -> 0
                in
                let end_idx =
                  try String.rindex frame_svg '<'
                  with Not_found -> String.length frame_svg
                in
                if start_idx < end_idx then
                  Buffer.add_substring buffer frame_svg start_idx
                    (end_idx - start_idx)
            | `Partial -> failwith "Could not end SVG render")
        | `Partial -> failwith "Could not render SVG");

        Buffer.add_string buffer "  </g>\n")
      frames;

    (* Close SVG *)
    Buffer.add_string buffer "</svg>\n";
    Buffer.contents buffer
