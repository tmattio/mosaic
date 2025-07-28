(** Re-export renderer modules *)

module type S = Renderer_intf.S

module Vg_renderer = Vg_renderer
module Gif_renderer = Gif_renderer
module Ascii_renderer = Ascii_renderer
