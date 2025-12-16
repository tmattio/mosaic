module Props = struct
  type t = {
    respect_alpha : bool;
    width_method : Glyph.width_method option;
    initial_width : int;
    initial_height : int;
  }

  let make ?(respect_alpha = false) ?width_method ?(initial_width = 1)
      ?(initial_height = 1) () =
    { respect_alpha; width_method; initial_width; initial_height }

  let default = make ()

  let equal a b =
    Bool.equal a.respect_alpha b.respect_alpha
    && Option.equal ( = ) a.width_method b.width_method
    && Int.equal a.initial_width b.initial_width
    && Int.equal a.initial_height b.initial_height
end

type t = {
  node : Renderable.t;
  surface : Grid.t;
  mutable content_width : int;
  mutable content_height : int;
  mutable intrinsic_width : int option;
  mutable intrinsic_height : int option;
  mutable draw : (t -> width:int -> height:int -> unit) option;
}

let node t = t.node
let width t = Grid.width t.surface
let height t = Grid.height t.surface
let request_render t = Renderable.request_render t.node
let transparent = Ansi.Color.of_rgba 0 0 0 0

let resize_surface_to_layout t =
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  if lw > 0 && lh > 0 then
    let sw = Grid.width t.surface and sh = Grid.height t.surface in
    if lw <> sw || lh <> sh then Grid.resize t.surface ~width:lw ~height:lh

let current_dimensions t =
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  let width = if lw > 0 then lw else width t in
  let height = if lh > 0 then lh else height t in
  (width, height)

let run_draw t ~width ~height =
  match t.draw with
  | Some draw when width > 0 && height > 0 ->
      let w = width and h = height in
      Grid.clear t.surface;
      let rect = Grid.{ x = 0; y = 0; width = w; height = h } in
      Grid.with_scissor t.surface rect (fun () -> draw t ~width:w ~height:h)
  | _ -> ()

let set_draw t draw =
  t.draw <- draw;
  match draw with
  | None -> ()
  | Some _ ->
      let width, height = current_dimensions t in
      run_draw t ~width ~height

let mark_layout_dirty t =
  match Renderable.mark_layout_dirty t.node with Ok () -> () | Error _ -> ()

let clamp_non_negative value = if value < 0 then 0 else value

let update_bounds t ~x ~y ~width ~height =
  let width = clamp_non_negative width in
  let height = clamp_non_negative height in
  let end_x = max 0 (x + width) in
  let end_y = max 0 (y + height) in
  let layout_w = Renderable.width t.node in
  let layout_h = Renderable.height t.node in
  let end_x = if layout_w > 0 then min end_x layout_w else end_x in
  let end_y = if layout_h > 0 then min end_y layout_h else end_y in
  let changed = ref false in
  if end_x > t.content_width then (
    t.content_width <- end_x;
    changed := true);
  if end_y > t.content_height then (
    t.content_height <- end_y;
    changed := true);
  if !changed then mark_layout_dirty t;
  !changed

let width_method t = Grid.width_method t.surface

let text_display_width t text =
  let width_method = width_method t in
  Glyph.measure ~width_method ~tab_width:2 text

let write_text t ~x ~y ?style text =
  if text = "" then false
  else
    let str_width = text_display_width t text in
    if str_width <= 0 then false
    else (
      ignore (update_bounds t ~x ~y ~width:str_width ~height:1);
      Grid.draw_text ?style t.surface ~x ~y ~text;
      true)

let clear ?(color = transparent) t =
  let w = width t and h = height t in
  if w > 0 && h > 0 then Grid.clear ~color t.surface;
  t.content_width <- max t.content_width w;
  t.content_height <- max t.content_height h;
  mark_layout_dirty t;
  request_render t

let plot t ~x ~y ?(style = Ansi.Style.default) text =
  if write_text t ~x ~y ~style text then request_render t

let fill_rect t ~x ~y ~width ~height ~color =
  if width <= 0 || height <= 0 then ()
  else (
    ignore (update_bounds t ~x ~y ~width ~height);
    Grid.fill_rect t.surface ~x ~y ~width ~height ~color;
    request_render t)

let draw_box t ~x ~y ~width ~height ?(border_style = Grid.Border.single)
    ?border_sides ?(border_color = Ansi.Color.default)
    ?(background = transparent) ?title ?title_alignment () =
  if width <= 0 || height <= 0 then ()
  else
    let border_chars = border_style in
    let border_style = Ansi.Style.make ~fg:border_color ~bg:background () in
    let sides =
      match border_sides with
      | Some sides -> sides
      | None -> [ `Top; `Right; `Bottom; `Left ]
    in
    ignore (update_bounds t ~x ~y ~width ~height);
    Grid.draw_box t.surface ~x ~y ~width ~height ~border_chars
      ~border_sides:sides ~border_style ~bg_color:background ~should_fill:true
      ?title ?title_alignment ();
    request_render t

let set_intrinsic_size t ~width ~height =
  t.intrinsic_width <- Some (max 1 width);
  t.intrinsic_height <- Some (max 1 height);
  mark_layout_dirty t;
  request_render t

let clear_intrinsic_size t =
  t.intrinsic_width <- None;
  t.intrinsic_height <- None;
  mark_layout_dirty t;
  request_render t

let measure t ~(known_dimensions : float option Toffee.Geometry.Size.t)
    ~(available_space : Toffee.Available_space.t Toffee.Geometry.Size.t)
    ~style:_ =
  let open Toffee.Geometry.Size in
  let resolve_dimension known available intrinsic content =
    match known with
    | Some value when value > 0. -> value
    | _ -> (
        match Toffee.Available_space.to_option available with
        | Some value when value > 0. -> value
        | _ ->
            let base =
              match intrinsic with
              | Some v when v > 0 -> v
              | _ -> if content > 0 then content else 1
            in
            float base)
  in
  {
    width =
      resolve_dimension known_dimensions.width available_space.width
        t.intrinsic_width t.content_width;
    height =
      resolve_dimension known_dimensions.height available_space.height
        t.intrinsic_height t.content_height;
  }

let draw_line t ~x1 ~y1 ~x2 ~y2 ?(style = Ansi.Style.default) ?(kind = `Line) ()
    =
  let changed = ref false in
  let draw_cell x y glyph =
    if write_text t ~x ~y ~style glyph then changed := true
  in
  let dx = abs (x2 - x1) in
  let dy = abs (y2 - y1) in
  let sx = if x1 < x2 then 1 else -1 in
  let sy = if y1 < y2 then 1 else -1 in
  let plot_basic () =
    let glyph =
      if dx = 0 then "│"
      else if dy = 0 then "─"
      else if (x2 - x1) * (y2 - y1) > 0 then "╲"
      else "╱"
    in
    let rec loop x y err =
      draw_cell x y glyph;
      if x = x2 && y = y2 then ()
      else
        let e2 = 2 * err in
        let x, err = if e2 > -dy then (x + sx, err - dy) else (x, err) in
        let y, err = if e2 < dx then (y + sy, err + dx) else (y, err) in
        loop x y err
    in
    loop x1 y1 (dx - dy)
  in
  let plot_braille () =
    let buffer = Hashtbl.create 32 in
    let set_dot x y =
      let cell_x = x / 2 in
      let cell_y = y / 4 in
      let bit_x = x mod 2 in
      let bit_y = y mod 4 in
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
      let current = Option.value (Hashtbl.find_opt buffer key) ~default:0 in
      Hashtbl.replace buffer key (current lor (1 lsl bit_pos))
    in
    let rec loop x y err =
      set_dot x y;
      if x = x2 && y = y2 then ()
      else
        let e2 = 2 * err in
        let x, err = if e2 > -dy then (x + sx, err - dy) else (x, err) in
        let y, err = if e2 < dx then (y + sy, err + dx) else (y, err) in
        loop x y err
    in
    loop x1 y1 (dx - dy);
    Hashtbl.iter
      (fun (cell_x, cell_y) bits ->
        let code = 0x2800 + bits in
        let uchar =
          match Uchar.of_int code with
          | exception Invalid_argument _ -> Uchar.of_int 0x2800
          | c -> c
        in
        let buffer = Buffer.create 4 in
        Buffer.add_utf_8_uchar buffer uchar;
        let glyph = Buffer.contents buffer in
        draw_cell cell_x cell_y glyph)
      buffer
  in
  (match kind with `Line -> plot_basic () | `Braille -> plot_braille ());
  if !changed then request_render t

let render_canvas t renderable grid ~delta:_ =
  let lx = Renderable.x renderable in
  let ly = Renderable.y renderable in
  let lw = Renderable.width renderable in
  let lh = Renderable.height renderable in
  if lw <= 0 || lh <= 0 then ()
  else (
    resize_surface_to_layout t;
    (match t.draw with None -> () | Some _ -> run_draw t ~width:lw ~height:lh);
    let src = t.surface in
    let blit_w = min lw (Grid.width src) in
    let blit_h = min lh (Grid.height src) in
    if blit_w > 0 && blit_h > 0 then
      let rect = Grid.{ x = lx; y = ly; width = blit_w; height = blit_h } in
      Grid.with_scissor grid rect (fun () ->
          Grid.blit_region ~src ~dst:grid ~src_x:0 ~src_y:0 ~width:blit_w
            ~height:blit_h ~dst_x:lx ~dst_y:ly))

let mount ?(props = Props.default) node =
  let iw = max 0 props.initial_width in
  let ih = max 0 props.initial_height in
  let surface =
    Grid.create ~width:(max 1 iw) ~height:(max 1 ih)
      ?width_method:props.width_method ~respect_alpha:props.respect_alpha ()
  in
  let canvas =
    {
      node;
      surface;
      content_width = 0;
      content_height = 0;
      intrinsic_width = (if iw > 0 then Some iw else None);
      intrinsic_height = (if ih > 0 then Some ih else None);
      draw = None;
    }
  in
  Renderable.set_render node (render_canvas canvas);
  Renderable.set_measure node (Some (measure canvas));
  Renderable.set_on_size_change node
    (Some
       (fun _ ->
         resize_surface_to_layout canvas;
         request_render canvas));
  request_render canvas;
  canvas

let apply_props t (props : Props.t) =
  (* Canvas props are effectively creation-time only for surface size and
     width_method; updating them at runtime would require reallocating the
     underlying grid. For now, we treat apply_props as a no-op beyond
     updating respect_alpha and draw callback. *)
  if not (Bool.equal props.respect_alpha (Grid.respect_alpha t.surface)) then
    Grid.set_respect_alpha t.surface props.respect_alpha
(* We intentionally do not change width_method or intrinsic dimensions here
     to avoid reallocations; callers should recreate the canvas if needed. *)
