type orientation = [ `Vertical | `Horizontal ]
type scroll_unit = [ `Absolute | `Viewport | `Content | `Step ]
type arrow_chars = { up : string; down : string; left : string; right : string }

type arrow_style = {
  foreground : Ansi.Color.t option;
  background : Ansi.Color.t option;
  attributes : Ansi.Attr.flag list option;
  chars : arrow_chars option;
}

type track_style = {
  track_color : Ansi.Color.t option;
  thumb_color : Ansi.Color.t option;
}

module Props = struct
  type t = {
    orientation : orientation;
    show_arrows : bool;
    arrow_style : arrow_style option;
    track_style : track_style option;
    track_viewport_size : int option;
    on_change : (int -> unit) option;
    autofocus : bool;
  }

  let make ~orientation ?(show_arrows = false) ?arrow_style ?track_style
      ?track_viewport_size ?on_change ?(autofocus = false) () =
    {
      orientation;
      show_arrows;
      arrow_style;
      track_style;
      track_viewport_size;
      on_change;
      autofocus;
    }

  let default_vertical = make ~orientation:`Vertical ()
  let default_horizontal = make ~orientation:`Horizontal ()

  let equal a b =
    let arrow_style_equal (a : arrow_style) (b : arrow_style) =
      let rec list_eq eq xs ys =
        match (xs, ys) with
        | [], [] -> true
        | x :: xs, y :: ys -> eq x y && list_eq eq xs ys
        | _ -> false
      in
      Option.equal Ansi.Color.equal a.foreground b.foreground
      && Option.equal Ansi.Color.equal a.background b.background
      && Option.equal (list_eq ( = )) a.attributes b.attributes
      && Option.equal
           (fun ac bc ->
             String.equal ac.up bc.up
             && String.equal ac.down bc.down
             && String.equal ac.left bc.left
             && String.equal ac.right bc.right)
           a.chars b.chars
    in
    let track_style_equal (a : track_style) (b : track_style) =
      Option.equal Ansi.Color.equal a.track_color b.track_color
      && Option.equal Ansi.Color.equal a.thumb_color b.thumb_color
    in
    a.orientation = b.orientation
    && Bool.equal a.show_arrows b.show_arrows
    && Option.equal arrow_style_equal a.arrow_style b.arrow_style
    && Option.equal track_style_equal a.track_style b.track_style
    && Option.equal Int.equal a.track_viewport_size b.track_viewport_size
    && Bool.equal (Option.is_some a.on_change) (Option.is_some b.on_change)
    && Bool.equal a.autofocus b.autofocus
end

let default_arrow_chars = { up = "▲"; down = "▼"; left = "◀"; right = "▶" }

let style_of_arrow_style = function
  | None -> Ansi.Style.default
  | Some { foreground; background; attributes; _ } ->
      let style = Ansi.Style.default in
      let resolved_fg =
        match (foreground, background) with
        | Some fg, _ -> Some fg
        | None, Some bg -> Some bg
        | _ -> None
      in
      let style =
        match resolved_fg with
        | Some fg -> Ansi.Style.fg fg style
        | None -> style
      in
      let style =
        match background with
        | Some bg -> Ansi.Style.bg bg style
        | None -> style
      in
      let style =
        match attributes with
        | Some attrs ->
            List.fold_left
              (fun acc flag -> Ansi.Style.add_attr flag acc)
              style attrs
        | None -> style
      in
      style

let clamp v ~min ~max = if v < min then min else if v > max then max else v

type t = {
  node : Renderable.t;
  start_arrow : Renderable.t;
  end_arrow : Renderable.t;
  slider : Slider.t;
  mutable props : Props.t;
  mutable on_change : (int -> unit) option;
  mutable scroll_position : int;
  mutable scroll_size : int;
  mutable viewport_size : int;
  mutable manual_visibility : bool;
  mutable arrow_style : arrow_style option;
  mutable arrow_chars : arrow_chars;
  mutable track_viewport_size : int option;
  mutable scroll_step : int option;
  mutable repeat_active : bool;
  mutable repeat_is_end : bool;
  mutable repeat_big_step_pending : bool;
  mutable repeat_delay : float;
  mutable repeat_interval : float;
  mutable repeat_elapsed : float;
  mutable syncing_from_slider : bool;
}

let node t = t.node

let arrow_char t = function
  | `Up -> t.arrow_chars.up
  | `Down -> t.arrow_chars.down
  | `Left -> t.arrow_chars.left
  | `Right -> t.arrow_chars.right

let arrow_direction_of_orientation orientation ~is_end =
  match (orientation, is_end) with
  | `Vertical, false -> `Up
  | `Vertical, true -> `Down
  | `Horizontal, false -> `Left
  | `Horizontal, true -> `Right

let measure_arrow t dir ~known_dimensions:_ ~available_space:_ ~style:_ =
  let text = arrow_char t dir in
  let width =
    float_of_int
      (max 1 (Glyph.measure ~width_method:`Unicode ~tab_width:2 text))
  in
  let height = 1. in
  { Toffee.Geometry.Size.width; height }

let render_arrow t dir _renderable grid ~delta:_ =
  let arrow =
    match dir with
    | `Up | `Left -> t.start_arrow
    | `Down | `Right -> t.end_arrow
  in
  let lx = Renderable.x arrow in
  let ly = Renderable.y arrow in
  let text = arrow_char t dir in
  Grid.draw_text
    ~style:(style_of_arrow_style t.arrow_style)
    grid ~x:lx ~y:ly ~text

let configure_arrow_node t dir node =
  Renderable.set_render node (render_arrow t dir);
  Renderable.set_measure node (Some (measure_arrow t dir));
  let style = Renderable.style node in
  let style =
    Toffee.Style.set_align_self (Some Toffee.Style.Align_items.Center) style
  in
  let size = Toffee.Style.size style in
  let size = { size with height = Toffee.Style.Dimension.length 1. } in
  let style = Toffee.Style.set_size size style in
  ignore (Renderable.set_style node style : (unit, Renderable.error) result)

let range t = max 0 (t.scroll_size - t.viewport_size)

let update_slider_viewport t =
  let viewport =
    if t.viewport_size > 0 then t.viewport_size
    else Option.value t.track_viewport_size ~default:1
  in
  Slider.set_viewport_size t.slider (float_of_int viewport)

let update_slider_from_scroll_state t =
  let scroll_range = range t in
  Slider.set_range t.slider ~min:0. ~max:(float_of_int scroll_range);
  update_slider_viewport t;
  if not t.syncing_from_slider then (
    t.syncing_from_slider <- true;
    Slider.set_value t.slider
      (float_of_int (min t.scroll_position scroll_range));
    t.syncing_from_slider <- false)

let recalc_visibility t =
  if not t.manual_visibility then
    let range = max 0 (t.scroll_size - t.viewport_size) in
    Renderable.set_visible t.node (range > 0)

let request t = Renderable.request_render t.node

let set_scroll_position ?(emit = true) t value =
  let clamped = clamp value ~min:0 ~max:(range t) in
  if clamped <> t.scroll_position then (
    t.scroll_position <- clamped;
    if not t.syncing_from_slider then (
      t.syncing_from_slider <- true;
      Slider.set_value t.slider (float_of_int clamped);
      t.syncing_from_slider <- false);
    if emit then Option.iter (fun f -> f clamped) t.on_change;
    request t)

let handle_slider_change t v =
  if not t.syncing_from_slider then (
    t.syncing_from_slider <- true;
    set_scroll_position t (int_of_float (Float.round v));
    t.syncing_from_slider <- false)

let scroll_by t delta ~unit =
  let multiplier =
    match unit with
    | `Absolute -> 1.
    | `Viewport -> float (max 1 t.viewport_size)
    | `Content -> float (max 1 t.scroll_size)
    | `Step ->
        float
          (match t.scroll_step with Some step when step > 0 -> step | _ -> 1)
  in
  let target =
    Float.round (float t.scroll_position +. (delta *. multiplier))
    |> int_of_float
  in
  set_scroll_position t target

let stop_repeat t =
  t.repeat_active <- false;
  t.repeat_big_step_pending <- false;
  Renderable.set_live t.node false

let start_arrow_repeat t ~is_end =
  t.repeat_active <- true;
  t.repeat_is_end <- is_end;
  t.repeat_delay <- 0.5;
  t.repeat_interval <- 0.2;
  t.repeat_elapsed <- 0.;
  t.repeat_big_step_pending <- true;
  Renderable.set_live t.node true

let on_arrow_mouse t is_end (event : Event.mouse) =
  match Event.Mouse.kind event with
  | Down -> (
      match Event.Mouse.button event with
      | Some Input.Mouse.Left ->
          let delta = if is_end then 0.5 else -0.5 in
          scroll_by t delta ~unit:`Viewport;
          start_arrow_repeat t ~is_end;
          Event.Mouse.prevent_default event;
          Event.Mouse.stop_propagation event
      | _ -> ())
  | Up ->
      if t.repeat_active then (
        stop_repeat t;
        Event.Mouse.stop_propagation event)
  | _ -> ()

let handle_key t (event : Event.key) =
  let kev = Event.Key.data event in
  match kev.key with
  | Left when t.props.orientation = `Horizontal ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Right when t.props.orientation = `Horizontal ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Up when t.props.orientation = `Vertical ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Down when t.props.orientation = `Vertical ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Horizontal && Uchar.equal c (Uchar.of_char 'h')
    ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Horizontal && Uchar.equal c (Uchar.of_char 'l')
    ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Vertical && Uchar.equal c (Uchar.of_char 'k') ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Vertical && Uchar.equal c (Uchar.of_char 'j') ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Page_up ->
      scroll_by t (-0.5) ~unit:`Viewport;
      true
  | Page_down ->
      scroll_by t 0.5 ~unit:`Viewport;
      true
  | Home ->
      scroll_by t (-1.) ~unit:`Content;
      true
  | End ->
      scroll_by t 1. ~unit:`Content;
      true
  | _ -> false

let scroll_position t = t.scroll_position
let scroll_size t = t.scroll_size
let viewport_size t = t.viewport_size
let set_on_change t cb = t.on_change <- cb

let set_scroll_size t size =
  let size = max 0 size in
  if size <> t.scroll_size then (
    t.scroll_size <- size;
    recalc_visibility t;
    update_slider_from_scroll_state t;
    set_scroll_position ~emit:false t t.scroll_position)

let set_viewport_size t size =
  let size = max 0 size in
  if size <> t.viewport_size then (
    t.viewport_size <- size;
    recalc_visibility t;
    update_slider_from_scroll_state t;
    set_scroll_position ~emit:false t t.scroll_position)

let set_show_arrows t v =
  if t.props.show_arrows <> v then (
    t.props <- { t.props with show_arrows = v };
    Renderable.set_visible t.start_arrow v;
    Renderable.set_visible t.end_arrow v;
    request t)

let update_arrow_style t (style : arrow_style) =
  t.arrow_style <- Some style;
  (match style.chars with Some c -> t.arrow_chars <- c | None -> ());
  request t

let update_track_style t ({ track_color; thumb_color } : track_style) =
  Option.iter (Slider.set_track_color t.slider) track_color;
  Option.iter (Slider.set_thumb_color t.slider) thumb_color;
  request t

let set_track_viewport_size t vps =
  t.track_viewport_size <-
    (match vps with Some n when n > 0 -> Some n | _ -> None);
  update_slider_viewport t;
  request t

let set_scroll_step t step = t.scroll_step <- step

let reset_visibility_control t =
  t.manual_visibility <- false;
  recalc_visibility t

let set_visible_override t vis =
  t.manual_visibility <- true;
  Renderable.set_visible t.node vis

let apply_props t (props : Props.t) =
  (* Orientation controls layout and is treated as creation-time only. *)
  if not (Props.equal t.props props) then (
    (* Keep the stored props snapshot mostly in sync while preserving
       orientation to avoid reconfiguring layout at runtime. *)
    let updated_props =
      {
        t.props with
        show_arrows = props.show_arrows;
        arrow_style = props.arrow_style;
        track_style = props.track_style;
        track_viewport_size = props.track_viewport_size;
        on_change = props.on_change;
        autofocus = props.autofocus;
      }
    in
    t.props <- updated_props;
    set_show_arrows t props.show_arrows;
    (match props.arrow_style with
    | Some style -> update_arrow_style t style
    | None -> ());
    (match props.track_style with
    | Some style -> update_track_style t style
    | None -> ());
    set_track_viewport_size t props.track_viewport_size;
    set_on_change t props.on_change)

let mount ?(props = Props.default_vertical) node =
  let initial_track_color =
    Option.value
      (Option.bind props.track_style (fun s -> s.track_color))
      ~default:(Ansi.Color.of_rgba 37 37 39 255)
  in
  let initial_thumb_color =
    Option.value
      (Option.bind props.track_style (fun s -> s.thumb_color))
      ~default:(Ansi.Color.of_rgba 154 158 163 255)
  in
  let arrow_chars =
    match props.arrow_style with
    | Some { chars = Some c; _ } -> c
    | _ -> default_arrow_chars
  in
  let layout_style = Renderable.style node in
  let layout_style =
    let open Toffee.Style in
    let layout_style =
      match props.orientation with
      | `Vertical -> set_flex_direction Flex_direction.Column layout_style
      | `Horizontal -> set_flex_direction Flex_direction.Row layout_style
    in
    let layout_style =
      set_align_items (Some Align_items.Stretch) layout_style
    in
    let layout_style = set_align_self (Some Align_items.Stretch) layout_style in
    set_flex_shrink 0. layout_style
  in
  ignore
    (Renderable.set_style node layout_style : (unit, Renderable.error) result);
  Renderable.set_render node (fun _ _ ~delta:_ -> ());
  Renderable.set_focusable node true;
  (match props.autofocus with
  | true -> ignore (Renderable.focus node)
  | false -> ());

  let create_child () =
    match Renderable.create_child ~parent:node () with
    | Ok child -> child
    | Error _ -> failwith "failed to create child node"
  in
  let start_arrow = create_child () in
  let end_arrow = create_child () in
  let slider_node = create_child () in

  ignore (Renderable.append_child ~parent:node ~child:start_arrow);
  ignore (Renderable.append_child ~parent:node ~child:slider_node);
  ignore (Renderable.append_child ~parent:node ~child:end_arrow);

  let slider_style =
    let open Toffee.Style in
    let style = Renderable.style slider_node in
    let style = set_flex_grow 1. style in
    let style = set_flex_shrink 1. style in
    set_align_self (Some Align_items.Stretch) style
  in
  ignore
    (Renderable.set_style slider_node slider_style
      : (unit, Renderable.error) result);

  let slider_props =
    let viewport =
      match props.track_viewport_size with
      | Some v when v > 0 -> Some (float_of_int v)
      | _ -> None
    in
    Slider.Props.make
      ~orientation:(props.orientation :> Slider.orientation)
      ~min:0. ~max:0. ~value:0. ?viewport_size:viewport
      ~track_color:initial_track_color ~thumb_color:initial_thumb_color ()
  in
  let slider = Slider.mount ~props:slider_props slider_node in

  let t =
    {
      node;
      start_arrow;
      end_arrow;
      slider;
      props;
      on_change = props.on_change;
      scroll_position = 0;
      scroll_size = 0;
      viewport_size = 0;
      manual_visibility = false;
      arrow_style = props.arrow_style;
      arrow_chars;
      track_viewport_size = props.track_viewport_size;
      scroll_step = None;
      repeat_active = false;
      repeat_is_end = false;
      repeat_big_step_pending = false;
      repeat_delay = 0.5;
      repeat_interval = 0.2;
      repeat_elapsed = 0.;
      syncing_from_slider = false;
    }
  in

  configure_arrow_node t
    (arrow_direction_of_orientation props.orientation ~is_end:false)
    start_arrow;
  configure_arrow_node t
    (arrow_direction_of_orientation props.orientation ~is_end:true)
    end_arrow;
  Renderable.set_visible start_arrow props.show_arrows;
  Renderable.set_visible end_arrow props.show_arrows;
  Renderable.on_mouse start_arrow (on_arrow_mouse t false);
  Renderable.on_mouse end_arrow (on_arrow_mouse t true);

  Renderable.on_mouse node (fun ev ->
      match Event.Mouse.kind ev with
      | Up when t.repeat_active ->
          stop_repeat t;
          Event.Mouse.stop_propagation ev
      | _ -> ());

  Renderable.set_on_frame node
    (Some
       (fun _ ~delta ->
         if t.repeat_active then
           let d = if Float.is_nan delta then 0. else max 0. delta in
           if t.repeat_delay > 0. then t.repeat_delay <- t.repeat_delay -. d
           else if t.repeat_big_step_pending then (
             t.repeat_big_step_pending <- false;
             let amt = if t.repeat_is_end then 0.5 else -0.5 in
             scroll_by t amt ~unit:`Viewport;
             t.repeat_elapsed <- 0.)
           else (
             t.repeat_elapsed <- t.repeat_elapsed +. d;
             if t.repeat_elapsed >= t.repeat_interval then (
               t.repeat_elapsed <- 0.;
               let amt = if t.repeat_is_end then 0.2 else -0.2 in
               scroll_by t amt ~unit:`Viewport))));

  Renderable.on_key_down node (fun ev -> ignore (handle_key t ev));
  Slider.set_on_change t.slider (Some (handle_slider_change t));

  update_slider_from_scroll_state t;
  recalc_visibility t;
  t
