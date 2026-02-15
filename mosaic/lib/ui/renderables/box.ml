module Props = struct
  type t = {
    background : Ansi.Color.t option;
    border : bool;
    border_sides : Grid.Border.side list;
    border_style : Grid.Border.t;
    border_color : Ansi.Color.t;
    focused_border_color : Ansi.Color.t option;
    should_fill : bool;
    custom_border_chars : Grid.Border.t option;
    title : string option;
    title_alignment : [ `Left | `Center | `Right ];
    gap : Toffee.Style.Length_percentage.t option;
    row_gap : Toffee.Style.Length_percentage.t option;
    column_gap : Toffee.Style.Length_percentage.t option;
  }

  let make ?background ?(border = false)
      ?(border_sides = [ `Top; `Bottom; `Left; `Right ])
      ?(border_style = Grid.Border.single)
      ?(border_color = Ansi.Color.of_rgb 255 255 255) ?focused_border_color
      ?(should_fill = true) ?custom_border_chars ?title
      ?(title_alignment = `Left) ?gap ?row_gap ?column_gap () =
    {
      background;
      border;
      border_sides;
      border_style;
      border_color;
      focused_border_color;
      should_fill;
      custom_border_chars;
      title;
      title_alignment;
      gap;
      row_gap;
      column_gap;
    }

  let default = make ()

  let equal a b =
    Option.equal Ansi.Color.equal a.background b.background
    && a.border = b.border
    && List.length a.border_sides = List.length b.border_sides
    && List.for_all
         (fun side -> List.exists (( = ) side) b.border_sides)
         a.border_sides
    && a.border_style = b.border_style
    && Ansi.Color.equal a.border_color b.border_color
    && Option.equal Ansi.Color.equal a.focused_border_color
         b.focused_border_color
    && a.should_fill = b.should_fill
    && a.custom_border_chars = b.custom_border_chars
    && a.title = b.title
    && a.title_alignment = b.title_alignment
    && a.gap = b.gap && a.row_gap = b.row_gap
    && a.column_gap = b.column_gap
end

type t = { node : Renderable.t; mutable props : Props.t }

let effective_border_sides box =
  if box.props.border then box.props.border_sides else []

(* Calculate the insets (in cells) on each side due to borders. *)
let calculate_insets box =
  if not box.props.border then (0, 0, 0, 0)
  else
    let rec aux (top, bottom, left, right) = function
      | [] -> (top, bottom, left, right)
      | `Top :: rest -> aux (1, bottom, left, right) rest
      | `Bottom :: rest -> aux (top, 1, left, right) rest
      | `Left :: rest -> aux (top, bottom, 1, right) rest
      | `Right :: rest -> aux (top, bottom, left, 1) rest
    in
    aux (0, 0, 0, 0) box.props.border_sides

(* Calculate the clipping rectangle for a child renderable inside the box,
   accounting for border insets. Always return a rectangle aligned to the inner
   content area, even if it has zero width/height, so that children are fully
   clipped by the renderer when there is no available space. We return a rect
   with max(0, ...) dimensions rather than omitting the scissor. *)
let child_clip box _parent =
  let x = Renderable.x box.node in
  let y = Renderable.y box.node in
  let w = Renderable.width box.node in
  let h = Renderable.height box.node in
  let top_inset, bottom_inset, left_inset, right_inset = calculate_insets box in
  let rect_width = max 0 (w - left_inset - right_inset) in
  let rect_height = max 0 (h - top_inset - bottom_inset) in
  Some
    Grid.
      {
        x = x + left_inset;
        y = y + top_inset;
        width = rect_width;
        height = rect_height;
      }

(** Apply box border styles to a given Toffee style. *)
let style_with_border box style =
  let module Lp = Toffee.Style.Length_percentage in
  (* Border width should be 1 cell, not 1%. *)
  let lp_one = Lp.length 1. in
  let rec aux rect = function
    | [] -> rect
    | `Top :: rest -> aux { rect with Toffee.Geometry.Rect.top = lp_one } rest
    | `Bottom :: rest -> aux { rect with bottom = lp_one } rest
    | `Left :: rest -> aux { rect with left = lp_one } rest
    | `Right :: rest -> aux { rect with right = lp_one } rest
  in
  let default = Toffee.Geometry.Rect.all Lp.zero in
  let rect =
    if not box.props.border then default
    else aux default (effective_border_sides box)
  in
  Toffee.Style.set_border rect style

let apply_border_style box =
  let style = Renderable.style box.node in
  let updated = style_with_border box style in
  match Renderable.set_style box.node updated with Ok () -> () | Error _ -> ()

(* --- Rendering helpers --- *)

let transparent = Ansi.Color.of_rgba 0 0 0 0

let resolve_bg_color props =
  match props.Props.background with Some c -> c | None -> transparent

let resolve_active_border_color box renderable =
  let focused = Renderable.focused renderable in
  match (focused, box.props.focused_border_color) with
  | true, Some c -> c
  | _ -> box.props.border_color

let resolve_border_chars box =
  match box.props.custom_border_chars with
  | Some cs -> cs
  | None -> box.props.border_style

let box_render box renderable grid ~delta:_ =
  let w = Renderable.width renderable in
  let h = Renderable.height renderable in
  if w <= 0 || h <= 0 then ()
  else
    let bg_color = resolve_bg_color box.props in
    let border_chars = resolve_border_chars box in
    let border_style =
      Ansi.Style.make
        ~fg:(resolve_active_border_color box renderable)
        ~bg:bg_color ()
    in
    Grid.draw_box grid ~x:(Renderable.x renderable) ~y:(Renderable.y renderable)
      ~width:w ~height:h ~border:border_chars
      ~sides:(effective_border_sides box)
      ~style:border_style
      ?fill:(if box.props.should_fill then Some bg_color else None)
      ?title:box.props.title ~title_alignment:box.props.title_alignment ()

let node t = t.node
let request_render t = Renderable.request_render t.node

(* Ensure border is enabled when border-related properties change. Returns
   [true] if it turned the border on and applied layout changes. *)
let ensure_border_enabled t =
  if not t.props.border then (
    t.props <- { t.props with border = true };
    apply_border_style t;
    true)
  else false

let set_background t color =
  if t.props.background <> color then (
    t.props <- { t.props with background = color };
    request_render t)

let set_border t flag =
  if t.props.border <> flag then (
    t.props <- { t.props with border = flag };
    apply_border_style t;
    request_render t)

let set_border_sides t sides =
  if t.props.border_sides <> sides then (
    t.props <- { t.props with border_sides = sides };
    apply_border_style t;
    request_render t)

let set_border_style t style =
  if t.props.border_style <> style then (
    t.props <- { t.props with border_style = style; custom_border_chars = None };
    ignore (ensure_border_enabled t);
    request_render t)

let set_border_color t color =
  if not (Ansi.Color.equal t.props.border_color color) then (
    t.props <- { t.props with border_color = color };
    ignore (ensure_border_enabled t);
    request_render t)

let set_focused_border_color t color =
  if t.props.focused_border_color <> color then (
    t.props <- { t.props with focused_border_color = color };
    ignore (ensure_border_enabled t);
    (* Only re-render immediately if currently focused. *)
    if Renderable.focused t.node then request_render t)

let set_should_fill t flag =
  if t.props.should_fill <> flag then (
    t.props <- { t.props with should_fill = flag };
    request_render t)

let set_custom_border_chars t chars =
  if t.props.custom_border_chars <> chars then (
    t.props <- { t.props with custom_border_chars = chars };
    (* Request a render to reflect any change; border remains unchanged. *)
    request_render t)

let set_title t title =
  if t.props.title <> title then (
    t.props <- { t.props with title };
    (* Always request render on title change. *)
    request_render t)

let set_title_alignment t alignment =
  if t.props.title_alignment <> alignment then (
    t.props <- { t.props with title_alignment = alignment };
    (* Always request render on alignment change; title presence is handled in
       render. *)
    request_render t)

let set_style t style =
  let updated_style = style_with_border t style in
  match Renderable.set_style t.node updated_style with
  | Ok () as ok ->
      Renderable.request_render t.node;
      ok
  | Error _ as err -> err

let mount ?(props = Props.default) node =
  let box = { node; props } in
  (* Initialize border if supporting properties are present. We approximate
     "presence" by checking for differences from defaults or explicit custom
     border chars. *)
  let defaults = Props.default in
  let should_init_border =
    (not box.props.border)
    &&
    match box.props.custom_border_chars with
    | Some _ -> true
    | None ->
        box.props.border_style <> defaults.border_style
        || (not (Ansi.Color.equal box.props.border_color defaults.border_color))
        || box.props.focused_border_color <> defaults.focused_border_color
  in
  if should_init_border then box.props <- { box.props with border = true };
  Renderable.set_render node (box_render box);
  Renderable.set_child_clip node (Some (child_clip box));
  (* Let flex-growing boxes shrink by default (Yoga parity) without affecting
     non-growing items like headers. Only override auto min-size when flex_grow
     > 0. *)
  let st0 = Renderable.style node in
  let st =
    let open Toffee.Style in
    let min_sz = min_size st0 in
    let needs_override =
      flex_grow st0 > 0.
      && (Dimension.is_auto min_sz.width || Dimension.is_auto min_sz.height)
    in
    if not needs_override then st0
    else
      let zero = Dimension.length 0. in
      let w = if Dimension.is_auto min_sz.width then zero else min_sz.width in
      let h = if Dimension.is_auto min_sz.height then zero else min_sz.height in
      set_min_size (Toffee.Geometry.Size.make w h) st0
  in
  ignore (Renderable.set_style node st);
  apply_border_style box;
  (* Apply initial gap props if provided: gap sets both axes first, then
     row/column refine their respective axis. *)
  (match (box.props.gap, box.props.row_gap, box.props.column_gap) with
  | None, None, None -> ()
  | _ ->
      let curr = Renderable.style box.node in
      let current_gap = Toffee.Style.gap curr in
      let base =
        match box.props.gap with
        | None -> current_gap
        | Some g -> Toffee.Geometry.Size.square g
      in
      let with_row =
        match box.props.row_gap with
        | None -> base
        | Some rg -> Toffee.Geometry.Size.make base.width rg
      in
      let with_both =
        match box.props.column_gap with
        | None -> with_row
        | Some cg -> Toffee.Geometry.Size.make cg with_row.height
      in
      ignore
        (Renderable.set_style box.node (Toffee.Style.set_gap with_both curr)));
  Renderable.request_render node;
  box

(* Gap convenience setters *)
let set_gap t v =
  let curr_style = Renderable.style t.node in
  let gap = Toffee.Geometry.Size.square v in
  match Renderable.set_style t.node (Toffee.Style.set_gap gap curr_style) with
  | Ok () -> Renderable.request_render t.node
  | Error _ -> ()

let set_row_gap t v =
  let curr_style = Renderable.style t.node in
  let current = Toffee.Style.gap curr_style in
  let updated = Toffee.Geometry.Size.make current.width v in
  match
    Renderable.set_style t.node (Toffee.Style.set_gap updated curr_style)
  with
  | Ok () -> Renderable.request_render t.node
  | Error _ -> ()

let set_column_gap t v =
  let curr_style = Renderable.style t.node in
  let current = Toffee.Style.gap curr_style in
  let updated = Toffee.Geometry.Size.make v current.height in
  match
    Renderable.set_style t.node (Toffee.Style.set_gap updated curr_style)
  with
  | Ok () -> Renderable.request_render t.node
  | Error _ -> ()

let apply_props t (props : Props.t) =
  (* Update renderable props wholesale, leaning on existing setters where
     possible to reuse their side-effects (style updates, layout dirties). *)
  if not (Props.equal t.props props) then (
    (* Background and border-related fields *)
    set_background t props.background;
    set_border t props.border;
    set_border_sides t props.border_sides;
    set_border_style t props.border_style;
    set_border_color t props.border_color;
    set_focused_border_color t props.focused_border_color;
    set_should_fill t props.should_fill;
    set_custom_border_chars t props.custom_border_chars;
    set_title t props.title;
    set_title_alignment t props.title_alignment;
    Option.iter (set_gap t) props.gap;
    Option.iter (set_row_gap t) props.row_gap;
    Option.iter (set_column_gap t) props.column_gap;
    (* Gap fields *)
    ())
