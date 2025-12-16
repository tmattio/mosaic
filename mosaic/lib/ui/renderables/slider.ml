type orientation = [ `Horizontal | `Vertical ]

module Props = struct
  type t = {
    orientation : orientation;
    min_value : float;
    max_value : float;
    value : float;
    viewport_size : float option;
    track_color : Ansi.Color.t;
    thumb_color : Ansi.Color.t;
    on_change : (float -> unit) option;
  }

  let make ~orientation ?(min = 0.) ?(max = 100.) ?(value = 0.) ?viewport_size
      ?(track_color = Ansi.Color.of_rgba 37 37 39 255)
      ?(thumb_color = Ansi.Color.of_rgba 154 158 163 255) ?on_change () =
    {
      orientation;
      min_value = min;
      max_value = max;
      value;
      viewport_size;
      track_color;
      thumb_color;
      on_change;
    }

  let equal a b =
    a.orientation = b.orientation
    && Float.equal a.min_value b.min_value
    && Float.equal a.max_value b.max_value
    && Float.equal a.value b.value
    && Option.equal Float.equal a.viewport_size b.viewport_size
    && Ansi.Color.equal a.track_color b.track_color
    && Ansi.Color.equal a.thumb_color b.thumb_color
    && Bool.equal (Option.is_some a.on_change) (Option.is_some b.on_change)

  (* Accessors for reconciler to inspect props in a controlled way *)
  let orientation (p : t) = p.orientation
  let min (p : t) = p.min_value
  let max (p : t) = p.max_value
  let value (p : t) = p.value
  let viewport_size (p : t) = p.viewport_size
  let track_color (p : t) = p.track_color
  let thumb_color (p : t) = p.thumb_color
  let on_change (p : t) = p.on_change
end

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  (* Last value provided via props; used to detect external value changes. *)
  mutable prop_value : float;
  mutable dragging : bool;
  mutable drag_offset_virtual : float;
}

let node t = t.node
let value t = t.props.value

let clamp_value t v =
  let v = max t.props.min_value (min t.props.max_value v) in
  if Float.is_nan v then t.props.min_value else v

let range t =
  let r = t.props.max_value -. t.props.min_value in
  if r <= 0. then 0. else r

let effective_viewport_size t =
  match t.props.viewport_size with
  | Some v ->
      let r = range t in
      let capped = if r > 0. then Float.min v r else v in
      max 0.01 capped
  | None ->
      let r = range t in
      let base = if r > 0. then max 1. (0.1 *. r) else 1. in
      let capped = if r > 0. then Float.min base r else base in
      max 0.01 capped

let virtual_track_size t width height =
  match t.props.orientation with
  | `Horizontal -> float_of_int (max 0 width) *. 2.
  | `Vertical -> float_of_int (max 0 height) *. 2.

let virtual_thumb_size t width height =
  let track = virtual_track_size t width height in
  if track <= 0. then 0.
  else
    let r = range t in
    if r = 0. then track
    else
      (* Use at least 1.0 logical unit for viewport in thumb sizing
         calculations. *)
      let viewport = max 1.0 (effective_viewport_size t) in
      let content = r +. viewport in
      if content <= viewport then track
      else
        let size = Float.floor (track *. (viewport /. content)) in
        max 1. (min size track)

let virtual_thumb_start t width height =
  let track = virtual_track_size t width height in
  if track <= 0. then 0.
  else
    let thumb_size = virtual_thumb_size t width height in
    let r = range t in
    if r = 0. then 0.
    else
      let ratio = (t.props.value -. t.props.min_value) /. r in
      Float.round (ratio *. (track -. thumb_size))

let request t = Renderable.request_render t.node

let notify_change t =
  match t.props.on_change with None -> () | Some f -> f t.props.value

let set_value t v =
  let clamped = clamp_value t v in
  if not (Float.equal clamped t.props.value) then (
    t.props <- { t.props with value = clamped };
    notify_change t;
    request t)

let set_viewport_size t size =
  let r = range t in
  let capped = if r > 0. then Float.min size r else size in
  let size = max 0.01 capped in
  match t.props.viewport_size with
  | Some cur when Float.equal cur size -> ()
  | _ ->
      t.props <- { t.props with viewport_size = Some size };
      request t

let set_range t ~min ~max =
  let min_v = min in
  let max_v = max in
  let min_v, max_v = if max_v < min_v then (max_v, min_v) else (min_v, max_v) in
  let changed =
    not
      (Float.equal min_v t.props.min_value
      && Float.equal max_v t.props.max_value)
  in
  if changed then (
    t.props <- { t.props with min_value = min_v; max_value = max_v };
    (* clamp value and viewport to new range *)
    t.props <- { t.props with value = clamp_value t t.props.value };
    (match t.props.viewport_size with
    | Some v -> ignore (set_viewport_size t v)
    | None -> ());
    request t)

let set_track_color t color =
  if not (Ansi.Color.equal t.props.track_color color) then (
    t.props <- { t.props with track_color = color };
    request t)

let set_thumb_color t color =
  if not (Ansi.Color.equal t.props.thumb_color color) then (
    t.props <- { t.props with thumb_color = color };
    request t)

let set_on_change t cb = t.props <- { t.props with on_change = cb }

let measure slider ~known_dimensions ~available_space ~style:_ =
  let default_width, default_height =
    match slider.props.orientation with
    | `Horizontal -> (Float.of_int 10, Float.of_int 1)
    | `Vertical -> (Float.of_int 1, Float.of_int 10)
  in
  let open Toffee.Geometry.Size in
  let available_width =
    match Toffee.Available_space.to_option available_space.width with
    | Some w when w > 0. -> Some w
    | _ -> None
  in
  let available_height =
    match Toffee.Available_space.to_option available_space.height with
    | Some h when h > 0. -> Some h
    | _ -> None
  in
  let clamp_positive value =
    let v = if Float.is_nan value then 0. else value in
    Float.max 0. v
  in
  let width =
    match known_dimensions.width with
    | Some w when w > 0. -> w
    | _ -> (
        match slider.props.orientation with
        | `Horizontal -> (
            match available_width with
            | Some w when w > 0. -> Float.max default_width w
            | _ -> default_width)
        | `Vertical -> (
            match available_width with
            | Some w when w > 0. -> Float.min default_width w
            | _ -> default_width))
  in
  let height =
    match known_dimensions.height with
    | Some h when h > 0. -> h
    | _ -> (
        match slider.props.orientation with
        | `Vertical -> (
            match available_height with
            | Some h when h > 0. -> Float.max default_height h
            | _ -> default_height)
        | `Horizontal -> (
            match available_height with
            | Some h when h > 0. -> Float.min default_height h
            | _ -> default_height))
  in
  { width = clamp_positive width; height = clamp_positive height }

let draw_horizontal slider grid ~x ~y ~width ~height =
  if width <= 0 || height <= 0 then ()
  else
    let render_height = max 1 height in
    Grid.fill_rect grid ~x ~y ~width ~height:render_height
      ~color:slider.props.track_color;
    let virtual_thumb_size = virtual_thumb_size slider width height in
    let virtual_thumb_start = virtual_thumb_start slider width height in
    let virtual_thumb_end = virtual_thumb_start +. virtual_thumb_size in
    let real_start_cell =
      int_of_float (Float.floor (virtual_thumb_start /. 2.))
    in
    let real_end_cell =
      int_of_float (Float.ceil (virtual_thumb_end /. 2.) -. 1.)
    in
    let start_x = max 0 real_start_cell in
    let end_x = min (width - 1) real_end_cell in
    for real_x = start_x to end_x do
      let virtual_cell_start = float_of_int (real_x * 2) in
      let virtual_cell_end = virtual_cell_start +. 2. in
      let thumb_start_in_cell =
        Float.max virtual_thumb_start virtual_cell_start
      in
      let thumb_end_in_cell = Float.min virtual_thumb_end virtual_cell_end in
      let coverage = thumb_end_in_cell -. thumb_start_in_cell in
      let code =
        if coverage >= 2. then 0x2588
        else if coverage <= 0. then 0x20
        else
          let is_left_half =
            Float.equal thumb_start_in_cell virtual_cell_start
          in
          if is_left_half then 0x258C else 0x2590
      in
      for y0 = 0 to render_height - 1 do
        Grid.set_cell_alpha grid ~x:(x + real_x) ~y:(y + y0) ~code
          ~fg:slider.props.thumb_color ~bg:slider.props.track_color
          ~attrs:Ansi.Attr.empty ()
      done
    done

let draw_vertical slider grid ~x ~y ~width ~height =
  if width <= 0 || height <= 0 then ()
  else
    let render_width = max 1 width in
    Grid.fill_rect grid ~x ~y ~width:render_width ~height
      ~color:slider.props.track_color;
    let virtual_thumb_size = virtual_thumb_size slider width height in
    let virtual_thumb_start = virtual_thumb_start slider width height in
    let virtual_thumb_end = virtual_thumb_start +. virtual_thumb_size in
    let real_start_cell =
      int_of_float (Float.floor (virtual_thumb_start /. 2.))
    in
    let real_end_cell =
      int_of_float (Float.ceil (virtual_thumb_end /. 2.) -. 1.)
    in
    let start_y = max 0 real_start_cell in
    let end_y = min (height - 1) real_end_cell in
    for real_y = start_y to end_y do
      let virtual_cell_start = float_of_int (real_y * 2) in
      let virtual_cell_end = virtual_cell_start +. 2. in
      let thumb_start_in_cell =
        Float.max virtual_thumb_start virtual_cell_start
      in
      let thumb_end_in_cell = Float.min virtual_thumb_end virtual_cell_end in
      let coverage = thumb_end_in_cell -. thumb_start_in_cell in
      let code =
        if coverage >= 2. then 0x2588
        else if coverage <= 0. then 0x20
        else
          let is_upper_half =
            Float.equal thumb_start_in_cell virtual_cell_start
          in
          if is_upper_half then 0x2580 else 0x2584
      in
      for x0 = 0 to render_width - 1 do
        Grid.set_cell_alpha grid ~x:(x + x0) ~y:(y + real_y) ~code
          ~fg:slider.props.thumb_color ~bg:slider.props.track_color
          ~attrs:Ansi.Attr.empty ()
      done
    done

let render slider renderable grid ~delta:_ =
  let lx = Renderable.x renderable in
  let ly = Renderable.y renderable in
  let lw = Renderable.width renderable in
  let lh = Renderable.height renderable in
  match slider.props.orientation with
  | `Horizontal -> draw_horizontal slider grid ~x:lx ~y:ly ~width:lw ~height:lh
  | `Vertical -> draw_vertical slider grid ~x:lx ~y:ly ~width:lw ~height:lh

let update_value_from_ratio slider ratio =
  let r = range slider in
  if r = 0. then slider.props.min_value
  else slider.props.min_value +. (ratio *. r)

let update_from_mouse slider x y ~lx ~ly ~lw ~lh =
  let track_start, track_size, mouse_pos =
    match slider.props.orientation with
    | `Horizontal -> (lx, lw, x)
    | `Vertical -> (ly, lh, y)
  in
  if track_size <= 0 then ()
  else
    let relative = float_of_int (mouse_pos - track_start) in
    let clamped = max 0. (min (float_of_int track_size) relative) in
    let ratio =
      if track_size = 0 then 0. else clamped /. float_of_int track_size
    in
    set_value slider (update_value_from_ratio slider ratio)

let calculate_drag_offset slider x y ~lx ~ly ~lw ~lh =
  let track_start, track_size, mouse =
    match slider.props.orientation with
    | `Horizontal -> (lx, lw, x)
    | `Vertical -> (ly, lh, y)
  in
  let clamped =
    max 0. (min (float_of_int track_size) (float_of_int (mouse - track_start)))
    *. 2.
  in
  let thumb_start = virtual_thumb_start slider lw lh in
  let thumb_size = virtual_thumb_size slider lw lh in
  let offset = clamped -. thumb_start in
  max 0. (min thumb_size offset)

let update_from_mouse_with_offset slider x y ~lx ~ly ~lw ~lh offset =
  let track_start, track_size, mouse =
    match slider.props.orientation with
    | `Horizontal -> (lx, lw, x)
    | `Vertical -> (ly, lh, y)
  in
  if track_size <= 0 then ()
  else
    let virtual_track = float_of_int track_size *. 2. in
    let clamped =
      max 0.
        (min (float_of_int track_size) (float_of_int (mouse - track_start)))
    in
    let virtual_mouse = clamped *. 2. in
    let thumb_size = virtual_thumb_size slider lw lh in
    let max_thumb_start = max 0. (virtual_track -. thumb_size) in
    let desired = max 0. (min max_thumb_start (virtual_mouse -. offset)) in
    let ratio =
      if max_thumb_start = 0. then 0. else desired /. max_thumb_start
    in
    set_value slider (update_value_from_ratio slider ratio)

let handle_mouse slider event renderable =
  let lx = Renderable.x renderable in
  let ly = Renderable.y renderable in
  let lw = Renderable.width renderable in
  let lh = Renderable.height renderable in
  let x = Event.Mouse.x event in
  let y = Event.Mouse.y event in
  match Event.Mouse.kind event with
  | Down -> (
      match Event.Mouse.button event with
      | Some Input.Mouse.Left ->
          let thumb_rect =
            let virtual_start = virtual_thumb_start slider lw lh in
            let virtual_size = virtual_thumb_size slider lw lh in
            let real_start = int_of_float (Float.floor (virtual_start /. 2.)) in
            let real_size =
              int_of_float
                (Float.ceil ((virtual_start +. virtual_size) /. 2.)
                -. float real_start)
            in
            match slider.props.orientation with
            | `Horizontal -> (lx + real_start, ly, max 1 real_size, lh)
            | `Vertical -> (lx, ly + real_start, lw, max 1 real_size)
          in
          let tx, ty, tw, th = thumb_rect in
          let inside = x >= tx && x < tx + tw && y >= ty && y < ty + th in
          if inside then (
            slider.dragging <- true;
            slider.drag_offset_virtual <-
              calculate_drag_offset slider x y ~lx ~ly ~lw ~lh;
            Event.Mouse.prevent_default event;
            Event.Mouse.stop_propagation event)
          else (
            update_from_mouse slider x y ~lx ~ly ~lw ~lh;
            slider.dragging <- true;
            slider.drag_offset_virtual <-
              calculate_drag_offset slider x y ~lx ~ly ~lw ~lh;
            Event.Mouse.prevent_default event;
            Event.Mouse.stop_propagation event)
      | _ -> ())
  | Drag ->
      if slider.dragging then (
        update_from_mouse_with_offset slider x y ~lx ~ly ~lw ~lh
          slider.drag_offset_virtual;
        Event.Mouse.stop_propagation event)
  | Up -> (
      match Event.Mouse.button event with
      | Some Input.Mouse.Left ->
          if slider.dragging then (
            update_from_mouse_with_offset slider x y ~lx ~ly ~lw ~lh
              slider.drag_offset_virtual;
            slider.dragging <- false;
            Event.Mouse.stop_propagation event)
      | _ -> ())
  | _ -> ()

let mount ?props node =
  let default_props =
    (* Default viewport_size intentionally left unset: compute ~10% of range
       with minimum 1 when not provided. *)
    Props.make ~orientation:`Horizontal ~min:0. ~max:100. ~value:0. ()
  in
  let props = Option.value props ~default:default_props in
  let slider =
    { node; props; prop_value = props.value; dragging = false; drag_offset_virtual = 0. }
  in
  let clamped_initial = clamp_value slider slider.props.value in
  if not (Float.equal clamped_initial slider.props.value) then
    slider.props <- { slider.props with value = clamped_initial };
  Renderable.set_render node (render slider);
  Renderable.set_measure node (Some (measure slider));
  Renderable.on_mouse node (fun event ->
      match Event.Mouse.kind event with
      | Down | Drag | Up -> handle_mouse slider event node
      | _ -> ());
  (* Ensure slider does not shrink in flex layouts. *)
  let style = Renderable.style node in
  let style = Toffee.Style.set_flex_shrink 0. style in
  ignore (Renderable.set_style node style : (unit, Renderable.error) result);
  request slider;
  slider

let apply_props t (props : Props.t) =
  (* Orientation is creation-time only for now; changing it would require
     reinterpreting layout, so we leave it as-is. *)
  if
    (not (Float.equal t.props.min_value props.min_value))
    || not (Float.equal t.props.max_value props.max_value)
  then set_range t ~min:props.min_value ~max:props.max_value;
  if not (Float.equal props.value t.prop_value) then (
    t.prop_value <- props.value;
    set_value t props.value);
  (match props.viewport_size with
  | Some v -> set_viewport_size t v
  | None -> ());
  set_track_color t props.track_color;
  set_thumb_color t props.thumb_color;
  set_on_change t props.on_change
