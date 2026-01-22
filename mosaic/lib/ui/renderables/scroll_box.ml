module Props = struct
  type t = {
    background : Ansi.Color.t option;
    scroll_x : bool;
    scroll_y : bool;
    scroll_acceleration : [ `Linear | `MacOS ];
    sticky_scroll : bool;
    sticky_start : [ `Top | `Bottom | `Left | `Right ] option;
    viewport_culling : bool;
    autofocus : bool;
  }

  let make ?background ?(scroll_x = false) ?(scroll_y = true)
      ?(scroll_acceleration = `Linear) ?(sticky_scroll = false) ?sticky_start
      ?(viewport_culling = true) ?(autofocus = false) () =
    {
      background;
      scroll_x;
      scroll_y;
      scroll_acceleration;
      sticky_scroll;
      sticky_start;
      viewport_culling;
      autofocus;
    }

  let default = make ()

  let equal a b =
    Option.equal Ansi.Color.equal a.background b.background
    && Bool.equal a.scroll_x b.scroll_x
    && Bool.equal a.scroll_y b.scroll_y
    && a.scroll_acceleration = b.scroll_acceleration
    && Bool.equal a.sticky_scroll b.sticky_scroll
    && Option.equal ( = ) a.sticky_start b.sticky_start
    && Bool.equal a.viewport_culling b.viewport_culling
    && Bool.equal a.autofocus b.autofocus
end

type t = {
  node : Renderable.t;
  wrapper : Renderable.t;
  viewport : Renderable.t;
  content : Renderable.t;
  renderer : Renderer.t option;
  mutable props : Props.t;
  mutable scroll_x : int;
  mutable scroll_y : int;
  mutable max_scroll_x : int;
  mutable max_scroll_y : int;
  mutable content_w : int;
  mutable content_h : int;
  mutable last_viewport_w : int;
  mutable last_viewport_h : int;
  mutable has_manual_scroll : bool;
  mutable listeners : (x:int -> y:int -> unit) list;
  mutable hbars : Scroll_bar.t list;
  mutable vbars : Scroll_bar.t list;
  internal_hbar : Scroll_bar.t option;
  internal_vbar : Scroll_bar.t option;
  (* Sticky edge state *)
  mutable sticky_top : bool;
  mutable sticky_bottom : bool;
  mutable sticky_left : bool;
  mutable sticky_right : bool;
  (* Auto-scroll during drag selection *)
  mutable auto_mouse_x : int;
  mutable auto_mouse_y : int;
  mutable is_auto_scrolling : bool;
  mutable auto_acc_x : float;
  mutable auto_acc_y : float;
  (* Simple scroll acceleration for wheel/keys *)
  mutable accel_last_ms : int;
  mutable accel_history : int list;
  accel_min_interval_ms : int;
  accel_streak_timeout_ms : int;
  accel_ref_interval_ms : int;
  mutable accel_A : float;
  mutable accel_tau : float;
  mutable accel_max_multiplier : float;
  (* Accumulators for scaled wheel deltas to preserve fractional parts *)
  mutable wheel_acc_x : float;
  mutable wheel_acc_y : float;
  mutable live_claimed : bool;
}

let clamp v ~min ~max = if v < min then min else if v > max then max else v
let notify listeners ~x ~y = List.iter (fun f -> f ~x ~y) listeners
let node t = t.node
let wrapper t = t.wrapper
let content t = t.content

let set_child_offsets t =
  Renderable.set_render_offset t.content ~x:(-t.scroll_x) ~y:(-t.scroll_y)

let content_visible_children t (content : Renderable.t) : int list =
  let viewport_bounds = Renderable.bounds t.viewport in
  let visible =
    Renderable.Internal.children_in_viewport ~parent:content
      ~viewport:viewport_bounds ~padding:0
  in
  List.map Renderable.Internal.number visible

let update_visible_children_selector t =
  if t.props.viewport_culling then
    Renderable.set_visible_children_selector t.content
      (Some (fun content -> content_visible_children t content))
  else Renderable.set_visible_children_selector t.content None

let request t = Renderable.request_render t.node

type axis = Horizontal | Vertical

let axis_has_range t = function
  | Horizontal -> t.max_scroll_x > 1
  | Vertical -> t.max_scroll_y > 1

let is_at_sticky_position t =
  match t.props.sticky_start with
  | Some `Top -> t.scroll_y <= 0
  | Some `Bottom -> t.scroll_y >= t.max_scroll_y
  | Some `Left -> t.scroll_x <= 0
  | Some `Right -> t.scroll_x >= t.max_scroll_x
  | None -> false

let mark_manual_scroll t axis =
  if
    t.props.sticky_scroll && axis_has_range t axis
    && not (is_at_sticky_position t)
  then t.has_manual_scroll <- true

let update_sticky_state t =
  if t.props.sticky_scroll then (
    (* Vertical *)
    if t.scroll_y <= 0 then (
      t.sticky_top <- true;
      t.sticky_bottom <- false)
    else if t.scroll_y >= t.max_scroll_y then (
      t.sticky_top <- false;
      t.sticky_bottom <- true)
    else (
      t.sticky_top <- false;
      t.sticky_bottom <- false);
    (* Horizontal *)
    if t.scroll_x <= 0 then (
      t.sticky_left <- true;
      t.sticky_right <- false)
    else if t.scroll_x >= t.max_scroll_x then (
      t.sticky_left <- false;
      t.sticky_right <- true)
    else (
      t.sticky_left <- false;
      t.sticky_right <- false))

let recalc_content_extents t =
  (* Scroll metrics derive from content vs. viewport dimensions. *)
  t.content_w <- Renderable.width t.content;
  t.content_h <- Renderable.height t.content

let apply_metrics t ~viewport_w ~viewport_h =
  t.max_scroll_x <-
    (if t.props.scroll_x then max 0 (t.content_w - viewport_w) else 0);
  t.max_scroll_y <-
    (if t.props.scroll_y then max 0 (t.content_h - viewport_h) else 0);
  t.scroll_x <- clamp t.scroll_x ~min:0 ~max:t.max_scroll_x;
  t.scroll_y <- clamp t.scroll_y ~min:0 ~max:t.max_scroll_y;
  List.iter
    (fun bar ->
      Scroll_bar.set_scroll_size bar t.content_w;
      Scroll_bar.set_viewport_size bar viewport_w;
      Scroll_bar.set_scroll_position ~emit:false bar t.scroll_x)
    t.hbars;
  List.iter
    (fun bar ->
      Scroll_bar.set_scroll_size bar t.content_h;
      Scroll_bar.set_viewport_size bar viewport_h;
      Scroll_bar.set_scroll_position ~emit:false bar t.scroll_y)
    t.vbars;
  (* Sticky behavior on size/content changes *)
  if t.props.sticky_scroll then
    match t.props.sticky_start with
    | Some _ when not t.has_manual_scroll ->
        (* Apply initial sticky preference if not manually scrolled *)
        let apply () =
          match t.props.sticky_start with
          | Some `Top ->
              t.sticky_top <- true;
              t.sticky_bottom <- false;
              t.scroll_y <- 0
          | Some `Bottom ->
              t.sticky_top <- false;
              t.sticky_bottom <- true;
              t.scroll_y <- t.max_scroll_y
          | Some `Left ->
              t.sticky_left <- true;
              t.sticky_right <- false;
              t.scroll_x <- 0
          | Some `Right ->
              t.sticky_left <- false;
              t.sticky_right <- true;
              t.scroll_x <- t.max_scroll_x
          | None -> ()
        in
        apply ();
        set_child_offsets t
    | _ ->
        if t.sticky_top then t.scroll_y <- 0
        else if t.sticky_bottom then t.scroll_y <- t.max_scroll_y;
        if t.sticky_left then t.scroll_x <- 0
        else if t.sticky_right then t.scroll_x <- t.max_scroll_x;
        set_child_offsets t

let get_auto_dir_x t mouse_x : int =
  let rel_x = mouse_x - Renderable.x t.node in
  let dist_right = Renderable.width t.node - rel_x in
  if rel_x <= 3 then if t.scroll_x > 0 then -1 else 0
  else if dist_right <= 3 then if t.scroll_x < t.max_scroll_x then 1 else 0
  else 0

let get_auto_dir_y t mouse_y : int =
  let rel_y = mouse_y - Renderable.y t.node in
  let dist_bottom = Renderable.height t.node - rel_y in
  if rel_y <= 3 then if t.scroll_y > 0 then -1 else 0
  else if dist_bottom <= 3 then if t.scroll_y < t.max_scroll_y then 1 else 0
  else 0

let get_auto_speed t mouse_x mouse_y : float =
  let rel_x = mouse_x - Renderable.x t.node in
  let rel_y = mouse_y - Renderable.y t.node in
  let dist_right = Renderable.width t.node - rel_x in
  let dist_bottom = Renderable.height t.node - rel_y in
  let min_d = min rel_x (min dist_right (min rel_y dist_bottom)) in
  if min_d <= 1 then 72. else if min_d <= 2 then 36. else 6.

let stop_auto_scroll t =
  if t.is_auto_scrolling then (
    t.is_auto_scrolling <- false;
    t.auto_acc_x <- 0.;
    t.auto_acc_y <- 0.;
    if t.live_claimed then (
      t.live_claimed <- false;
      Renderable.set_live t.node false))

let begin_or_update_auto_scroll t ~x ~y =
  let dir_x = get_auto_dir_x t x in
  let dir_y = get_auto_dir_y t y in
  if dir_x = 0 && dir_y = 0 then stop_auto_scroll t
  else (
    t.auto_mouse_x <- x;
    t.auto_mouse_y <- y;
    if not t.is_auto_scrolling then (
      t.is_auto_scrolling <- true;
      if not (Renderable.live t.node) then (
        t.live_claimed <- true;
        Renderable.set_live t.node true));
    request t)

let handle_auto_scroll t ~delta =
  if t.is_auto_scrolling then (
    let dir_x = get_auto_dir_x t t.auto_mouse_x in
    let dir_y = get_auto_dir_y t t.auto_mouse_y in
    let speed = get_auto_speed t t.auto_mouse_x t.auto_mouse_y in
    let amount = speed *. delta in
    let moved = ref false in
    let trunc_to_int f =
      int_of_float (if f >= 0. then Float.floor f else Float.ceil f)
    in
    if dir_x <> 0 then (
      t.auto_acc_x <- t.auto_acc_x +. (float_of_int dir_x *. amount);
      let step = trunc_to_int t.auto_acc_x in
      if step <> 0 then (
        t.auto_acc_x <- t.auto_acc_x -. float_of_int step;
        let nx = clamp (t.scroll_x + step) ~min:0 ~max:t.max_scroll_x in
        if nx <> t.scroll_x then (
          t.scroll_x <- nx;
          moved := true;
          mark_manual_scroll t Horizontal)))
    else t.auto_acc_x <- 0.;
    if dir_y <> 0 then (
      t.auto_acc_y <- t.auto_acc_y +. (float_of_int dir_y *. amount);
      let step = trunc_to_int t.auto_acc_y in
      if step <> 0 then (
        t.auto_acc_y <- t.auto_acc_y -. float_of_int step;
        let ny = clamp (t.scroll_y + step) ~min:0 ~max:t.max_scroll_y in
        if ny <> t.scroll_y then (
          t.scroll_y <- ny;
          moved := true;
          mark_manual_scroll t Vertical)))
    else t.auto_acc_y <- 0.;
    if !moved then (
      update_sticky_state t;
      set_child_offsets t;
      notify t.listeners ~x:t.scroll_x ~y:t.scroll_y;
      (match t.renderer with
      | Some renderer -> Renderer.request_selection_update renderer
      | None -> ());
      request t)
    else if dir_x = 0 && dir_y = 0 then stop_auto_scroll t)

let render_scrollbox t _renderable grid ~delta =
  let lx = Renderable.x t.node in
  let ly = Renderable.y t.node in
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  if lw <= 0 || lh <= 0 then ()
  else (
    (match t.props.background with
    | None -> ()
    | Some c -> Grid.fill_rect grid ~x:lx ~y:ly ~width:lw ~height:lh ~color:c);
    handle_auto_scroll t ~delta;
    let vx = Renderable.x t.viewport and vy = Renderable.y t.viewport in
    let vw = Renderable.width t.viewport in
    let vh = Renderable.height t.viewport in
    let visible_vw = min vw (max 0 (Grid.width grid - vx)) in
    let visible_vh = min vh (max 0 (Grid.height grid - vy)) in
    let viewport_changed =
      visible_vw <> t.last_viewport_w || visible_vh <> t.last_viewport_h
    in
    (* Always recompute content extents to keep scroll metrics in sync with
       layout *)
    recalc_content_extents t;
    (* Update scroll metrics every frame to reflect latest content/viewport
       sizes *)
    apply_metrics t ~viewport_w:visible_vw ~viewport_h:visible_vh;
    if viewport_changed then (
      t.last_viewport_w <- visible_vw;
      t.last_viewport_h <- visible_vh);
    set_child_offsets t)

let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)

let accel_tick t : float =
  match t.props.scroll_acceleration with
  | `Linear -> 1.0
  | `MacOS ->
      let now = now_ms () in
      let dt =
        if t.accel_last_ms = 0 then Int.max_int
        else max 0 (now - t.accel_last_ms)
      in
      if dt = Int.max_int || dt > t.accel_streak_timeout_ms then (
        t.accel_last_ms <- now;
        t.accel_history <- [];
        1.0)
      else if dt < t.accel_min_interval_ms then 1.0
      else (
        t.accel_last_ms <- now;
        let history = dt :: t.accel_history in
        let history =
          if List.length history > 3 then List.rev (List.tl (List.rev history))
          else history
        in
        t.accel_history <- history;
        let avg_interval =
          match history with
          | [] -> float_of_int dt
          | _ ->
              let sum = List.fold_left ( + ) 0 history in
              float_of_int sum /. float_of_int (List.length history)
        in
        let velocity = float_of_int t.accel_ref_interval_ms /. avg_interval in
        let x = velocity /. t.accel_tau in
        let mult = 1.0 +. (t.accel_A *. (exp x -. 1.0)) in
        min t.accel_max_multiplier mult)

let accel_reset t =
  t.accel_last_ms <- 0;
  t.accel_history <- []

let reset_wheel_accumulators t =
  t.wheel_acc_x <- 0.;
  t.wheel_acc_y <- 0.

let scroll_to ?x ?y ?(manual = true) t =
  let prev_x = t.scroll_x and prev_y = t.scroll_y in
  let tx = Option.value ~default:t.scroll_x x in
  let ty = Option.value ~default:t.scroll_y y in
  let nx = clamp tx ~min:0 ~max:t.max_scroll_x in
  let ny = clamp ty ~min:0 ~max:t.max_scroll_y in
  if nx <> t.scroll_x || ny <> t.scroll_y then (
    t.scroll_x <- nx;
    t.scroll_y <- ny;
    update_sticky_state t;
    set_child_offsets t;
    notify t.listeners ~x:nx ~y:ny;
    request t);
  if manual then (
    if nx <> prev_x then mark_manual_scroll t Horizontal;
    if ny <> prev_y then mark_manual_scroll t Vertical)

let scroll_by ?x ?y ?(manual = true) t =
  let dx = Option.value ~default:0 x and dy = Option.value ~default:0 y in
  scroll_to ~x:(t.scroll_x + dx) ~y:(t.scroll_y + dy) ~manual t

let remap_scroll_direction direction shift =
  if not shift then direction
  else
    match direction with
    | Event.Mouse.Scroll_up -> Event.Mouse.Scroll_left
    | Event.Mouse.Scroll_down -> Event.Mouse.Scroll_right
    | Event.Mouse.Scroll_left -> Event.Mouse.Scroll_down
    | Event.Mouse.Scroll_right -> Event.Mouse.Scroll_up

let on_mouse t (event : Event.mouse) =
  match Event.Mouse.kind event with
  | Scroll -> (
      match Event.Mouse.scroll_delta event with
      | None -> ()
      | Some (dir, delta) ->
          let direction =
            remap_scroll_direction dir (Event.Mouse.modifiers event).shift
          in
          let dx, dy =
            match direction with
            | Event.Mouse.Scroll_up -> (0, -delta)
            | Event.Mouse.Scroll_down -> (0, delta)
            | Event.Mouse.Scroll_left -> (-delta, 0)
            | Event.Mouse.Scroll_right -> (delta, 0)
          in
          let dx = if t.props.scroll_x then dx else 0 in
          let dy = if t.props.scroll_y then dy else 0 in
          let mult = accel_tick t in
          (* Accumulate fractional parts for smoother scaling *)
          let fx = float_of_int dx *. mult in
          let fy = float_of_int dy *. mult in
          t.wheel_acc_x <- t.wheel_acc_x +. fx;
          t.wheel_acc_y <- t.wheel_acc_y +. fy;
          let stepx = int_of_float t.wheel_acc_x in
          let stepy = int_of_float t.wheel_acc_y in
          if stepx <> 0 then
            t.wheel_acc_x <- t.wheel_acc_x -. float_of_int stepx;
          if stepy <> 0 then
            t.wheel_acc_y <- t.wheel_acc_y -. float_of_int stepy;
          if stepx <> 0 || stepy <> 0 then
            let prev_x, prev_y = (t.scroll_x, t.scroll_y) in
            scroll_to ~x:(prev_x + stepx) ~y:(prev_y + stepy) t)
  | Drag ->
      let x = Event.Mouse.x event in
      let y = Event.Mouse.y event in
      let is_selecting = Event.Mouse.is_selecting event in
      if is_selecting then begin_or_update_auto_scroll t ~x ~y
      else stop_auto_scroll t
  | Up | Drag_end -> stop_auto_scroll t
  | _ -> ()

let scroll_by_units
    ?(x : (float * [ `Absolute | `Viewport | `Content | `Step ]) option)
    ?(y : (float * [ `Absolute | `Viewport | `Content | `Step ]) option)
    ?(manual = true) t =
  let mul_x =
    match x with
    | None -> 0
    | Some (d, `Absolute) -> int_of_float d
    | Some (d, `Viewport) ->
        int_of_float (d *. float_of_int (Renderable.width t.viewport))
    | Some (d, `Content) -> int_of_float (d *. float_of_int t.content_w)
    | Some (d, `Step) ->
        (* No public getter on Scroll_bar; match default semantics where missing
           step=1 *)
        int_of_float (d *. 1.)
  in
  let mul_y =
    match y with
    | None -> 0
    | Some (d, `Absolute) -> int_of_float d
    | Some (d, `Viewport) ->
        int_of_float (d *. float_of_int (Renderable.height t.viewport))
    | Some (d, `Content) -> int_of_float (d *. float_of_int t.content_h)
    | Some (d, `Step) -> int_of_float (d *. 1.)
  in
  scroll_by ~x:mul_x ~y:mul_y ~manual t

(* Accessors to ensure fields are legitimately read within this module. *)
let sticky_start t = t.props.sticky_start
let has_manual_scroll t = t.has_manual_scroll
let scroll_position t = (t.scroll_x, t.scroll_y)
let scroll_limits t = (t.max_scroll_x, t.max_scroll_y)
let scroll_width t = t.content_w
let scroll_height t = t.content_h
let viewport_size t = (Renderable.width t.viewport, Renderable.height t.viewport)
let sticky_scroll_enabled t = t.props.sticky_scroll
let horizontal_bar t = t.internal_hbar
let vertical_bar t = t.internal_vbar

let set_sticky_scroll t v =
  if t.props.sticky_scroll <> v then (
    t.props <- { t.props with sticky_scroll = v };
    update_sticky_state t;
    request t)

let set_sticky_start t edge =
  if t.props.sticky_start <> edge then (
    t.props <- { t.props with sticky_start = edge };
    update_sticky_state t;
    request t)

let reset_sticky t =
  t.has_manual_scroll <- false;
  if t.props.sticky_scroll then (
    (match t.props.sticky_start with
    | Some `Top -> t.scroll_y <- 0
    | Some `Bottom -> t.scroll_y <- t.max_scroll_y
    | Some `Left -> t.scroll_x <- 0
    | Some `Right -> t.scroll_x <- t.max_scroll_x
    | None -> ());
    set_child_offsets t;
    request t)

let handle_key t (event : Event.key) =
  let consumed_vertical =
    List.exists (fun bar -> Scroll_bar.handle_key bar event) t.vbars
  in
  let consumed =
    consumed_vertical
    || List.exists (fun bar -> Scroll_bar.handle_key bar event) t.hbars
  in
  if consumed then (
    t.has_manual_scroll <- true;
    accel_reset t;
    reset_wheel_accumulators t;
    true)
  else false

let on_scroll t cb = t.listeners <- cb :: t.listeners

let set_on_scroll t cb_opt =
  t.listeners <- (match cb_opt with None -> [] | Some cb -> [ cb ])

let set_background t color =
  if t.props.background <> color then (
    t.props <- { t.props with background = color };
    request t)

let append_child t child =
  let res = Renderable.append_child ~parent:t.content ~child in
  (match res with Ok () -> request t | Error _ -> ());
  res

let insert_child t ~index child =
  let res = Renderable.insert_child ~parent:t.content ~index ~child in
  (match res with Ok () -> request t | Error _ -> ());
  res

let remove_child t child =
  match Renderable.parent child with
  | Some p
    when Renderable.Internal.number p = Renderable.Internal.number t.content ->
      let _ = Renderable.detach child in
      request t;
      Ok ()
  | _ -> Ok ()

let bind_scroll_bars t ?horizontal ?vertical () =
  let add_unique lst bar =
    if List.exists (fun b -> b == bar) lst then lst else bar :: lst
  in
  Option.iter
    (fun bar ->
      Scroll_bar.set_on_change bar (Some (fun pos -> scroll_to ~x:pos t));
      t.hbars <- add_unique t.hbars bar)
    horizontal;
  Option.iter
    (fun bar ->
      Scroll_bar.set_on_change bar (Some (fun pos -> scroll_to ~y:pos t));
      t.vbars <- add_unique t.vbars bar)
    vertical;
  request t

let set_scroll_acceleration t mode =
  if t.props.scroll_acceleration <> mode then (
    t.props <- { t.props with scroll_acceleration = mode };
    ())

let set_scroll_acceleration_params t ~a ~tau ~max_multiplier =
  t.accel_A <- a;
  t.accel_tau <- tau;
  t.accel_max_multiplier <- max_multiplier

let set_viewport_culling t value =
  if Bool.equal t.props.viewport_culling value then ()
  else (
    t.props <- { t.props with viewport_culling = value };
    update_visible_children_selector t;
    request t)

let apply_props t (props : Props.t) =
  (* Background color *)
  set_background t props.background;
  (* Scroll acceleration mode *)
  set_scroll_acceleration t props.scroll_acceleration;
  (* Sticky scrolling behaviour *)
  set_sticky_scroll t props.sticky_scroll;
  set_sticky_start t props.sticky_start;
  (* Viewport culling *)
  set_viewport_culling t props.viewport_culling;
  t.props <- { t.props with autofocus = props.autofocus }

let mount ?(props = Props.default) ?renderer node =
  (* Root: flex row with align-items stretch. Preserve any layout properties
     already applied by the vnode layer (size, padding, etc.). *)
  let root_style = Renderable.style node in
  let root_style =
    let open Toffee.Style in
    let st = set_flex_direction Flex_direction.Row root_style in
    set_align_items (Some Align_items.Stretch) st
  in
  ignore
    (Renderable.set_style node root_style : (unit, Renderable.error) result);

  let base_id = Renderable.id node in

  (* Internal wrapper: flex column, flexGrow=1, fills the root. *)
  let wrapper =
    let id = base_id ^ "-wrapper" in
    let props_r = Renderable.Props.make ~id () in
    match Renderable.create_child ~parent:node ~props:props_r () with
    | Ok w -> w
    | Error _ -> failwith "Scroll_box: failed to create wrapper node"
  in
  let wrapper_style =
    let open Toffee.Style in
    let st = Renderable.style wrapper in
    let st = set_flex_direction Flex_direction.Column st in
    let st = set_flex_grow 1.0 st in
    (* Allow the wrapper to shrink below its contents; Toffee's default
       [min_size=auto] otherwise clamps to content width/height. *)
    let zero = Dimension.length 0. in
    let min_sz = Toffee.Geometry.Size.square zero in
    set_min_size min_sz st
  in
  ignore
    (Renderable.set_style wrapper wrapper_style
      : (unit, Renderable.error) result);
  ignore
    (Renderable.append_child ~parent:node ~child:wrapper
      : (unit, Renderable.error) result);

  (* Viewport: flex column, flexGrow=1, overflow hidden – owns clipping. *)
  let viewport =
    let id = base_id ^ "-viewport" in
    let props_r = Renderable.Props.make ~id () in
    match Renderable.create_child ~parent:wrapper ~props:props_r () with
    | Ok v -> v
    | Error _ -> failwith "Scroll_box: failed to create viewport node"
  in
  let viewport_style =
    let open Toffee in
    let st = Renderable.style viewport in
    let st = Style.set_flex_direction Style.Flex_direction.Column st in
    let st = Style.set_flex_grow 1.0 st in
    (* Allow the viewport to contract to the parent's size instead of its
       content's min-content size. *)
    let zero = Style.Dimension.length 0. in
    let min_sz = Geometry.Size.square zero in
    let st = Style.set_min_size min_sz st in
    let hidden =
      Geometry.Point.{ x = Style.Overflow.Hidden; y = Style.Overflow.Hidden }
    in
    let st = Style.set_overflow hidden st in
    st
  in
  ignore
    (Renderable.set_style viewport viewport_style
      : (unit, Renderable.error) result);
  ignore
    (Renderable.append_child ~parent:wrapper ~child:viewport
      : (unit, Renderable.error) result);

  (* Content: holds user children; alignSelf flex-start, flexShrink 0. When
     scrolling is enabled on an axis, content is unconstrained; otherwise it
     stretches to fill the viewport. *)
  let content =
    let id = base_id ^ "-content" in
    let props_r = Renderable.Props.make ~id () in
    match Renderable.create_child ~parent:viewport ~props:props_r () with
    | Ok c -> c
    | Error _ -> failwith "Scroll_box: failed to create content node"
  in
  let content_style =
    let open Toffee in
    let st0 = Renderable.style content in
    let st1 = Style.set_flex_direction Style.Flex_direction.Column st0 in
    let st1 = Style.set_align_self (Some Style.Align_items.Flex_start) st1 in
    let st1 = Style.set_flex_shrink 0.0 st1 in
    let min_sz = Style.min_size st1 in
    let max_sz = Style.max_size st1 in
    let min_sz = { min_sz with width = Style.Dimension.percent 1.0 } in
    let max_sz =
      if props.scroll_x then max_sz
      else { max_sz with width = Style.Dimension.percent 1.0 }
    in
    let min_sz = { min_sz with height = Style.Dimension.percent 1.0 } in
    let max_sz =
      if props.scroll_y then max_sz
      else { max_sz with height = Style.Dimension.percent 1.0 }
    in
    let st1 = Style.set_min_size min_sz st1 in
    let st1 = Style.set_max_size max_sz st1 in
    st1
  in
  ignore
    (Renderable.set_style content content_style
      : (unit, Renderable.error) result);
  ignore
    (Renderable.append_child ~parent:viewport ~child:content
      : (unit, Renderable.error) result);

  (* Clip the scroll box subtree to the viewport's bounds so that scrollable
     content is physically constrained to the visible region, regardless of how
     child layout extends beyond the viewport. The viewport node owns overflow
     and scissoring for its subtree. *)
  let viewport_clip _viewport =
    let b = Renderable.bounds viewport in
    Some Grid.{ x = b.x; y = b.y; width = b.width; height = b.height }
  in
  Renderable.set_child_clip viewport (Some viewport_clip);

  (* Scrollbars: created and owned by the scroll box. The vertical bar is a
     sibling of the wrapper on the root; the horizontal bar is a sibling of the
     viewport inside the wrapper. *)
  let vbar_node =
    let id = base_id ^ "-vertical-scrollbar" in
    let props_r = Renderable.Props.make ~id () in
    match Renderable.create_child ~parent:node ~props:props_r () with
    | Ok b -> b
    | Error _ -> failwith "Scroll_box: failed to create vertical scrollbar node"
  in
  ignore
    (Renderable.append_child ~parent:node ~child:vbar_node
      : (unit, Renderable.error) result);
  let vbar =
    Scroll_bar.mount ~props:Scroll_bar.Props.default_vertical vbar_node
  in

  let hbar_node =
    let id = base_id ^ "-horizontal-scrollbar" in
    let props_r = Renderable.Props.make ~id () in
    match Renderable.create_child ~parent:wrapper ~props:props_r () with
    | Ok b -> b
    | Error _ ->
        failwith "Scroll_box: failed to create horizontal scrollbar node"
  in
  ignore
    (Renderable.append_child ~parent:wrapper ~child:hbar_node
      : (unit, Renderable.error) result);
  let hbar =
    Scroll_bar.mount ~props:Scroll_bar.Props.default_horizontal hbar_node
  in

  let t =
    {
      node;
      wrapper;
      viewport;
      content;
      renderer;
      props;
      scroll_x = 0;
      scroll_y = 0;
      max_scroll_x = 0;
      max_scroll_y = 0;
      content_w = 0;
      content_h = 0;
      last_viewport_w = -1;
      last_viewport_h = -1;
      has_manual_scroll = false;
      listeners = [];
      hbars = [ hbar ];
      vbars = [ vbar ];
      internal_hbar = Some hbar;
      internal_vbar = Some vbar;
      sticky_top = false;
      sticky_bottom = false;
      sticky_left = false;
      sticky_right = false;
      auto_mouse_x = 0;
      auto_mouse_y = 0;
      is_auto_scrolling = false;
      auto_acc_x = 0.;
      auto_acc_y = 0.;
      accel_last_ms = 0;
      accel_history = [];
      accel_min_interval_ms = 6;
      accel_streak_timeout_ms = 150;
      accel_ref_interval_ms = 100;
      accel_A = 0.8;
      accel_tau = 3.0;
      accel_max_multiplier = 6.0;
      wheel_acc_x = 0.;
      wheel_acc_y = 0.;
      live_claimed = false;
    }
  in
  bind_scroll_bars t ~horizontal:hbar ~vertical:vbar ();
  (match renderer with
  | Some r ->
      Renderer.on_selection r
        (fun ~anchor_x:_ ~anchor_y:_ ~focus_x:_ ~focus_y:_ ->
          stop_auto_scroll t)
  | None -> ());
  Renderable.on_mouse node (on_mouse t);
  Renderable.on_key_down node (fun ev -> ignore (handle_key t ev));
  Renderable.set_focusable node true;
  (match props.autofocus with
  | true -> ignore (Renderable.focus node)
  | false -> ());
  (* Route future child mutations and reconciliation to the internal content
     container so user children live under it. *)
  Renderable.set_child_sink node
    (Some (fun ~child:_ ~index -> (content, index)));
  Renderable.set_reconcile_parent node content;
  (* Recompute on size changes of the viewport/content nodes and root. *)
  Renderable.set_on_size_change node (Some (fun _ -> request t));
  Renderable.set_on_size_change viewport (Some (fun _ -> request t));
  Renderable.set_on_size_change content (Some (fun _ -> request t));
  Renderable.set_render node (render_scrollbox t);
  (* Initial sticky start application if requested *)
  (match (props.sticky_scroll, props.sticky_start) with
  | true, Some `Top -> t.sticky_top <- true
  | true, Some `Bottom -> t.sticky_bottom <- true
  | true, Some `Left -> t.sticky_left <- true
  | true, Some `Right -> t.sticky_right <- true
  | _ -> ());
  update_visible_children_selector t;
  set_child_offsets t;
  request t;
  t
