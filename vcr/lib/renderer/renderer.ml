module type S = Renderer_intf.S

module Frame = struct
  type t = Renderer_intf.frame

  let create ~grid ~cursor_row ~cursor_col ~cursor_visible ~delay_cs
      ~dirty_regions ~cursor_moved =
    Renderer_intf.
      {
        grid;
        cursor_row;
        cursor_col;
        cursor_visible;
        delay_cs;
        dirty_regions;
        cursor_moved;
      }

  let rows t = Grid.rows t.Renderer_intf.grid
  let cols t = Grid.cols t.Renderer_intf.grid
  let get_cell t ~row ~col = Grid.get t.Renderer_intf.grid ~row ~col
end

module Ascii_renderer = Ascii_renderer
module Gif_renderer = Gif_renderer
module Png_renderer = Png_renderer
module Svg_renderer = Svg_renderer
