(* ───── Scroll Acceleration ───── *)

module Scroll_accel = struct
  type strategy =
    | Linear
    | MacOS of { a : float; tau : float; max_multiplier : float }

  type t = {
    strategy : strategy;
    mutable last_now : float;
    mutable history : float list;
  }

  let min_interval = 6.
  let streak_timeout = 150.
  let ref_interval = 100.
  let max_history = 3
  let linear () = { strategy = Linear; last_now = 0.; history = [] }

  let macos ?(a = 0.8) ?(tau = 3.0) ?(max_multiplier = 6.0) () =
    { strategy = MacOS { a; tau; max_multiplier }; last_now = 0.; history = [] }

  let tick t ~now =
    match t.strategy with
    | Linear -> 1.0
    | MacOS { a; tau; max_multiplier } ->
        let dt =
          if t.last_now = 0. then Float.infinity
          else Float.max 0. (now -. t.last_now)
        in
        if dt = Float.infinity || dt > streak_timeout then (
          t.last_now <- now;
          t.history <- [];
          1.0)
        else if dt < min_interval then 1.0
        else (
          t.last_now <- now;
          let history = dt :: t.history in
          let history =
            if List.length history > max_history then
              List.filteri (fun i _ -> i < max_history) history
            else history
          in
          t.history <- history;
          let avg_interval =
            match history with
            | [] -> dt
            | _ ->
                let sum = List.fold_left ( +. ) 0. history in
                sum /. float_of_int (List.length history)
          in
          let velocity = ref_interval /. avg_interval in
          let x = velocity /. tau in
          let mult = 1.0 +. (a *. (exp x -. 1.0)) in
          Float.min max_multiplier mult)

  let reset t =
    t.last_now <- 0.;
    t.history <- []
end

(* ───── Props ───── *)

type reveal_align = [ `Start | `Center | `End | `Nearest ]

type reveal = {
  key : string;
  x : int option;
  y : int option;
  align_x : reveal_align;
  align_y : reveal_align;
  margin : int;
}

type scroll_by = {
  key : string;
  x : float option;
  y : float option;
  unit : Scroll_bar.scroll_unit;
}

let validate_scroll_by (request : scroll_by option) =
  match request with
  | None -> ()
  | Some { x = None; y = None; _ } ->
      invalid_arg "Scroll_box: scroll_by requires an x or y delta"
  | Some request ->
      let finite = function None -> true | Some v -> Float.is_finite v in
      if not (finite request.x && finite request.y) then
        invalid_arg "Scroll_box: scroll_by deltas must be finite"

let equal_reveal_align a b =
  match (a, b) with
  | `Start, `Start | `Center, `Center | `End, `End | `Nearest, `Nearest -> true
  | _ -> false

let equal_reveal (a : reveal) (b : reveal) =
  String.equal a.key b.key
  && Option.equal Int.equal a.x b.x
  && Option.equal Int.equal a.y b.y
  && equal_reveal_align a.align_x b.align_x
  && equal_reveal_align a.align_y b.align_y
  && Int.equal a.margin b.margin

let equal_scroll_by (a : scroll_by) (b : scroll_by) =
  String.equal a.key b.key
  && Option.equal Float.equal a.x b.x
  && Option.equal Float.equal a.y b.y
  && a.unit = b.unit

module Props = struct
  type t = {
    scroll_x : bool;
    scroll_y : bool;
    sticky_scroll : bool;
    sticky_start : [ `Top | `Bottom | `Left | `Right ] option;
    background : Ansi.Color.t option;
    show_scrollbars : bool;
    scrollbar_props : Scroll_bar.Props.t option;
    vertical_bar_props : Scroll_bar.Props.t option;
    horizontal_bar_props : Scroll_bar.Props.t option;
    reveal : reveal option;
    scroll_by : scroll_by option;
    reset_sticky : string option;
  }

  let make ?(scroll_x = false) ?(scroll_y = true) ?(sticky_scroll = false)
      ?sticky_start ?background ?(show_scrollbars = true) ?scrollbar_props
      ?vertical_bar_props ?horizontal_bar_props ?reveal ?scroll_by ?reset_sticky
      () =
    validate_scroll_by scroll_by;
    {
      scroll_x;
      scroll_y;
      sticky_scroll;
      sticky_start;
      background;
      show_scrollbars;
      scrollbar_props;
      vertical_bar_props;
      horizontal_bar_props;
      reveal;
      scroll_by;
      reset_sticky;
    }

  let default = make ()

  let equal a b =
    a.scroll_x = b.scroll_x && a.scroll_y = b.scroll_y
    && a.sticky_scroll = b.sticky_scroll
    && a.sticky_start = b.sticky_start
    && Option.equal Ansi.Color.equal a.background b.background
    && a.show_scrollbars = b.show_scrollbars
    && Option.equal Scroll_bar.Props.equal a.scrollbar_props b.scrollbar_props
    && Option.equal Scroll_bar.Props.equal a.vertical_bar_props
         b.vertical_bar_props
    && Option.equal Scroll_bar.Props.equal a.horizontal_bar_props
         b.horizontal_bar_props
    && Option.equal equal_reveal a.reveal b.reveal
    && Option.equal equal_scroll_by a.scroll_by b.scroll_by
    && Option.equal String.equal a.reset_sticky b.reset_sticky
end

(* ───── Scroll Box ───── *)

type t = {
  node : Renderable.t;
  viewport : Renderable.t;
  content : Renderable.t;
  mutable props : Props.t;
  vertical_bar : Scroll_bar.t;
  horizontal_bar : Scroll_bar.t;
  mutable scroll_x : int;
  mutable scroll_y : int;
  mutable max_scroll_x : int;
  mutable max_scroll_y : int;
  mutable content_w : int;
  mutable content_h : int;
  mutable on_scroll : (x:int -> y:int -> unit) option;
  mutable on_scroll_by_applied : (key:string -> unit) option;
  mutable on_reset_sticky_applied : (key:string -> unit) option;
  mutable scroll_accel : Scroll_accel.t;
  (* Milliseconds accumulated from frame deltas; feeds Scroll_accel.tick. *)
  mutable frame_clock : float;
  mutable has_manual_scroll : bool;
  mutable sticky_top : bool;
  mutable sticky_bottom : bool;
  mutable sticky_left : bool;
  mutable sticky_right : bool;
  mutable wheel_acc_x : float;
  mutable wheel_acc_y : float;
  mutable auto_scroll_mouse_x : int;
  mutable auto_scroll_mouse_y : int;
  mutable cached_auto_scroll_speed : float;
  mutable auto_scroll_acc_x : float;
  mutable auto_scroll_acc_y : float;
  mutable auto_scrolling : bool;
  mutable is_applying_sticky : bool;
  mutable pending_reveal : reveal option;
  mutable applied_reveal_key : string option;
  mutable pending_scroll_by : scroll_by option;
  mutable applied_scroll_by_key : string option;
  mutable pending_reset_sticky : string option;
  mutable applied_reset_sticky_key : string option;
}

let node t = t.node
let content t = t.content
let viewport t = t.viewport
let vertical_bar t = t.vertical_bar
let horizontal_bar t = t.horizontal_bar
let scroll_top t = t.scroll_y
let scroll_left t = t.scroll_x
let scroll_width t = t.content_w
let scroll_height t = t.content_h
let viewport_width t = Renderable.width t.viewport
let viewport_height t = Renderable.height t.viewport
let clamp v ~lo ~hi = max lo (min hi v)
let auto_scroll_threshold_vertical = 3
let auto_scroll_threshold_horizontal = 3

(* Selection auto-scroll speeds, in cells per second. *)
let auto_scroll_speed_slow = 6.
let auto_scroll_speed_medium = 36.
let auto_scroll_speed_fast = 72.

let reveal_axis ~align ~margin ~viewport ~current coordinate =
  let margin = max 0 margin in
  match align with
  | `Start -> coordinate - margin
  | `Center -> coordinate - (viewport / 2)
  | `End -> coordinate - viewport + 1 + margin
  | `Nearest ->
      if coordinate - margin < current then coordinate - margin
      else if coordinate + margin >= current + viewport then
        coordinate + margin - viewport + 1
      else current

(* ───── Content Translation ───── *)

let set_child_offsets t =
  Renderable.set_translate t.content ~x:(-t.scroll_x) ~y:(-t.scroll_y)

(* ───── Sticky Scroll ───── *)

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
    let max_y = t.max_scroll_y in
    let max_x = t.max_scroll_x in
    if t.scroll_y <= 0 then (
      t.sticky_top <- true;
      t.sticky_bottom <- false;
      match t.props.sticky_start with
      | Some `Top -> t.has_manual_scroll <- false
      | Some `Bottom when max_y = 0 -> t.has_manual_scroll <- false
      | _ -> ())
    else if t.scroll_y >= max_y then (
      t.sticky_top <- false;
      t.sticky_bottom <- true;
      match t.props.sticky_start with
      | Some `Bottom -> t.has_manual_scroll <- false
      | _ -> ())
    else (
      t.sticky_top <- false;
      t.sticky_bottom <- false);
    if t.scroll_x <= 0 then (
      t.sticky_left <- true;
      t.sticky_right <- false;
      match t.props.sticky_start with
      | Some `Left -> t.has_manual_scroll <- false
      | Some `Right when max_x = 0 -> t.has_manual_scroll <- false
      | _ -> ())
    else if t.scroll_x >= max_x then (
      t.sticky_left <- false;
      t.sticky_right <- true;
      match t.props.sticky_start with
      | Some `Right -> t.has_manual_scroll <- false
      | _ -> ())
    else (
      t.sticky_left <- false;
      t.sticky_right <- false))

(* ───── Notification ───── *)

let notify t =
  match t.on_scroll with Some f -> f ~x:t.scroll_x ~y:t.scroll_y | None -> ()

(* ───── Scroll Position ───── *)

let scroll_to_internal t ?(manual = true) ?x ?y () =
  let prev_x = t.scroll_x and prev_y = t.scroll_y in
  let tx = Option.value ~default:t.scroll_x x in
  let ty = Option.value ~default:t.scroll_y y in
  let nx = clamp tx ~lo:0 ~hi:t.max_scroll_x in
  let ny = clamp ty ~lo:0 ~hi:t.max_scroll_y in
  if nx <> t.scroll_x || ny <> t.scroll_y then (
    t.scroll_x <- nx;
    t.scroll_y <- ny;
    update_sticky_state t;
    set_child_offsets t;
    Scroll_bar.set_scroll_position t.vertical_bar ny;
    Scroll_bar.set_scroll_position t.horizontal_bar nx;
    notify t;
    Renderable.request_render t.node);
  if manual && not t.is_applying_sticky then (
    if nx <> prev_x then mark_manual_scroll t Horizontal;
    if ny <> prev_y then mark_manual_scroll t Vertical)

let set_scroll_top t v = scroll_to_internal t ~y:v ()
let set_scroll_left t v = scroll_to_internal t ~x:v ()
let scroll_to t ?x ?y () = scroll_to_internal t ?x ?y ()

let scroll_by t ?x ?y () =
  let nx = Option.map (fun dx -> t.scroll_x + dx) x in
  let ny = Option.map (fun dy -> t.scroll_y + dy) y in
  scroll_to_internal t ?x:nx ?y:ny ()

let scroll_by_unit t ?x ?y ~(unit : Scroll_bar.scroll_unit) () =
  Option.iter (fun dx -> Scroll_bar.scroll_by t.horizontal_bar dx ~unit) x;
  Option.iter (fun dy -> Scroll_bar.scroll_by t.vertical_bar dy ~unit) y

let apply_reveal t (reveal : reveal) =
  let prev_x = t.scroll_x in
  let prev_y = t.scroll_y in
  let x =
    Option.map
      (reveal_axis ~align:reveal.align_x ~margin:reveal.margin
         ~viewport:(viewport_width t) ~current:t.scroll_x)
      reveal.x
  in
  let y =
    Option.map
      (reveal_axis ~align:reveal.align_y ~margin:reveal.margin
         ~viewport:(viewport_height t) ~current:t.scroll_y)
      reveal.y
  in
  scroll_to_internal t ~manual:false ?x ?y ();
  if
    (t.scroll_x <> prev_x || t.scroll_y <> prev_y)
    && t.props.sticky_scroll
    && not (is_at_sticky_position t)
  then t.has_manual_scroll <- true;
  t.applied_reveal_key <- Some reveal.key

let apply_pending_reveal t =
  match t.pending_reveal with
  | None -> ()
  | Some reveal ->
      t.pending_reveal <- None;
      if not (Option.equal String.equal t.applied_reveal_key (Some reveal.key))
      then apply_reveal t reveal

let apply_scroll_by t (request : scroll_by) =
  scroll_by_unit t ?x:request.x ?y:request.y ~unit:request.unit ();
  t.applied_scroll_by_key <- Some request.key

let apply_pending_scroll_by t =
  match t.pending_scroll_by with
  | None -> ()
  | Some request ->
      t.pending_scroll_by <- None;
      if
        not
          (Option.equal String.equal t.applied_scroll_by_key (Some request.key))
      then apply_scroll_by t request;
      Option.iter
        (fun callback -> callback ~key:request.key)
        t.on_scroll_by_applied

let reset_sticky t =
  t.has_manual_scroll <- false;
  let prev = t.is_applying_sticky in
  t.is_applying_sticky <- true;
  (match t.props.sticky_start with
  | Some `Top when t.props.sticky_scroll ->
      scroll_to_internal t ~manual:false ~y:0 ()
  | Some `Bottom when t.props.sticky_scroll ->
      scroll_to_internal t ~manual:false ~y:t.max_scroll_y ()
  | Some `Left when t.props.sticky_scroll ->
      scroll_to_internal t ~manual:false ~x:0 ()
  | Some `Right when t.props.sticky_scroll ->
      scroll_to_internal t ~manual:false ~x:t.max_scroll_x ()
  | None | Some _ -> ());
  t.is_applying_sticky <- prev;
  Renderable.request_render t.node

let apply_pending_reset_sticky t =
  match t.pending_reset_sticky with
  | None -> ()
  | Some key ->
      t.pending_reset_sticky <- None;
      if not (Option.equal String.equal t.applied_reset_sticky_key (Some key))
      then (
        reset_sticky t;
        t.applied_reset_sticky_key <- Some key);
      Option.iter (fun callback -> callback ~key) t.on_reset_sticky_applied

(* ───── Selection Auto-scroll ───── *)

let auto_scroll_direction_x t mouse_x =
  let relative_x = mouse_x - Renderable.x t.node in
  let dist_to_left = relative_x in
  let dist_to_right = Renderable.width t.node - relative_x in
  if dist_to_left <= auto_scroll_threshold_horizontal then
    if t.scroll_x > 0 then -1 else 0
  else if dist_to_right <= auto_scroll_threshold_horizontal then
    if t.scroll_x < t.max_scroll_x then 1 else 0
  else 0

let auto_scroll_direction_y t mouse_y =
  let relative_y = mouse_y - Renderable.y t.node in
  let dist_to_top = relative_y in
  let dist_to_bottom = Renderable.height t.node - relative_y in
  if dist_to_top <= auto_scroll_threshold_vertical then
    if t.scroll_y > 0 then -1 else 0
  else if dist_to_bottom <= auto_scroll_threshold_vertical then
    if t.scroll_y < t.max_scroll_y then 1 else 0
  else 0

let auto_scroll_speed t mouse_x mouse_y =
  let relative_x = mouse_x - Renderable.x t.node in
  let relative_y = mouse_y - Renderable.y t.node in
  let dist_to_left = relative_x in
  let dist_to_right = Renderable.width t.node - relative_x in
  let dist_to_top = relative_y in
  let dist_to_bottom = Renderable.height t.node - relative_y in
  let min_distance =
    min (min dist_to_left dist_to_right) (min dist_to_top dist_to_bottom)
  in
  if min_distance <= 1 then auto_scroll_speed_fast
  else if min_distance <= 2 then auto_scroll_speed_medium
  else auto_scroll_speed_slow

let stop_auto_scroll t =
  let was_auto_scrolling = t.auto_scrolling in
  t.auto_scrolling <- false;
  t.auto_scroll_acc_x <- 0.;
  t.auto_scroll_acc_y <- 0.;
  if was_auto_scrolling then Renderable.set_live t.node false

let start_auto_scroll t mouse_x mouse_y =
  stop_auto_scroll t;
  t.auto_scroll_mouse_x <- mouse_x;
  t.auto_scroll_mouse_y <- mouse_y;
  t.cached_auto_scroll_speed <- auto_scroll_speed t mouse_x mouse_y;
  t.auto_scrolling <- true;
  Renderable.set_live t.node true

let update_auto_scroll t mouse_x mouse_y =
  t.auto_scroll_mouse_x <- mouse_x;
  t.auto_scroll_mouse_y <- mouse_y;
  t.cached_auto_scroll_speed <- auto_scroll_speed t mouse_x mouse_y;
  let scroll_x = auto_scroll_direction_x t mouse_x in
  let scroll_y = auto_scroll_direction_y t mouse_y in
  if scroll_x = 0 && scroll_y = 0 then stop_auto_scroll t
  else if not t.auto_scrolling then start_auto_scroll t mouse_x mouse_y

let apply_auto_scroll_axis ~acc ~direction ~amount ~scroll =
  if direction = 0 then (acc, false)
  else
    let acc = acc +. (float_of_int direction *. amount) in
    let cells = int_of_float acc in
    if cells = 0 then (acc, false)
    else (
      scroll cells;
      (acc -. float_of_int cells, true))

let handle_auto_scroll t delta =
  if t.auto_scrolling then
    match Renderable.Private.get_selection t.node with
    | Some sel when Selection.is_dragging sel ->
        let scroll_x = auto_scroll_direction_x t t.auto_scroll_mouse_x in
        let scroll_y = auto_scroll_direction_y t t.auto_scroll_mouse_y in
        (* Speeds are cells per second; [delta] is milliseconds. *)
        let scroll_amount =
          t.cached_auto_scroll_speed *. (Float.max 0. delta /. 1000.)
        in
        let acc_x, scrolled_x =
          apply_auto_scroll_axis ~acc:t.auto_scroll_acc_x ~direction:scroll_x
            ~amount:scroll_amount ~scroll:(fun cells ->
              scroll_to_internal t ~x:(t.scroll_x + cells) ())
        in
        let acc_y, scrolled_y =
          apply_auto_scroll_axis ~acc:t.auto_scroll_acc_y ~direction:scroll_y
            ~amount:scroll_amount ~scroll:(fun cells ->
              scroll_to_internal t ~y:(t.scroll_y + cells) ())
        in
        t.auto_scroll_acc_x <- acc_x;
        t.auto_scroll_acc_y <- acc_y;
        if scrolled_x || scrolled_y then
          Renderable.Private.request_selection_update t.node;
        if scroll_x = 0 && scroll_y = 0 then stop_auto_scroll t
    | _ -> stop_auto_scroll t

(* ───── Metrics Recalculation ───── *)

let recalc_metrics t =
  t.content_w <- Renderable.width t.content;
  t.content_h <- Renderable.height t.content;
  let vw = Renderable.width t.viewport in
  let vh = Renderable.height t.viewport in
  t.max_scroll_x <- (if t.props.scroll_x then max 0 (t.content_w - vw) else 0);
  t.max_scroll_y <- (if t.props.scroll_y then max 0 (t.content_h - vh) else 0);
  t.scroll_x <- clamp t.scroll_x ~lo:0 ~hi:t.max_scroll_x;
  t.scroll_y <- clamp t.scroll_y ~lo:0 ~hi:t.max_scroll_y;
  Scroll_bar.set_scroll_size t.vertical_bar t.content_h;
  Scroll_bar.set_viewport_size t.vertical_bar vh;
  Scroll_bar.set_scroll_size t.horizontal_bar t.content_w;
  Scroll_bar.set_viewport_size t.horizontal_bar vw

(* Reflow reengage (opentui ScrollBox.recalculateBarProps): when a reflow
   moves an edge, a latched box sitting within one row/column of that edge is
   following in every way that matters — clear the latch so [apply_sticky]
   re-pins it to the live edge. The one-row tolerance absorbs reflow rounding
   without capturing a reader parked further up, and the changed-max gate
   keeps steady-state manual scrolls parked: a one-row wheel-up with no
   reflow stays where the user put it. *)
let reengage_on_reflow t ~old_max_x ~old_max_y =
  if t.props.sticky_scroll && t.has_manual_scroll then
    match t.props.sticky_start with
    | Some `Bottom
      when t.max_scroll_y <> old_max_y
           && t.max_scroll_y > 0
           && t.scroll_y >= t.max_scroll_y - 1 ->
        t.has_manual_scroll <- false
    | Some `Top when t.max_scroll_y <> old_max_y && t.scroll_y <= 1 ->
        t.has_manual_scroll <- false
    | Some `Right
      when t.max_scroll_x <> old_max_x
           && t.max_scroll_x > 0
           && t.scroll_x >= t.max_scroll_x - 1 ->
        t.has_manual_scroll <- false
    | Some `Left when t.max_scroll_x <> old_max_x && t.scroll_x <= 1 ->
        t.has_manual_scroll <- false
    | _ -> ()

let apply_sticky t =
  if (not t.props.sticky_scroll) || t.has_manual_scroll then ()
  else
    let prev = t.is_applying_sticky in
    t.is_applying_sticky <- true;
    (match t.props.sticky_start with
    | Some `Bottom -> scroll_to_internal t ~manual:false ~y:t.max_scroll_y ()
    | Some `Top -> scroll_to_internal t ~manual:false ~y:0 ()
    | Some `Right -> scroll_to_internal t ~manual:false ~x:t.max_scroll_x ()
    | Some `Left -> scroll_to_internal t ~manual:false ~x:0 ()
    | None ->
        let y =
          if t.sticky_bottom then Some t.max_scroll_y
          else if t.sticky_top then Some 0
          else None
        in
        let x =
          if t.sticky_right then Some t.max_scroll_x
          else if t.sticky_left then Some 0
          else None
        in
        scroll_to_internal t ~manual:false ?x ?y ());
    t.is_applying_sticky <- prev

(* ───── Update ───── *)

(* Scroll maintenance runs in the frame traversal (opentui ScrollBox's
   onUpdate/recalculateBarProps phase), before the content's children are
   culled and rendered: sticky, reveal, and scroll_by must settle the content
   offset first so viewport culling selects the children the final offset
   makes visible. The hook lives on the content node — the traversal reaches
   it after refreshing the node/wrapper/viewport/content layouts, so metrics
   read current extents, and immediately before culling its children. *)
let update_scroll_box t ~delta =
  let d = if Float.is_nan delta then 0. else Float.max 0. delta in
  t.frame_clock <- t.frame_clock +. d;
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  if lw > 0 && lh > 0 then (
    let was_applying = t.is_applying_sticky in
    t.is_applying_sticky <- true;
    let old_max_x = t.max_scroll_x and old_max_y = t.max_scroll_y in
    recalc_metrics t;
    reengage_on_reflow t ~old_max_x ~old_max_y;
    apply_sticky t;
    apply_pending_reveal t;
    apply_pending_reset_sticky t;
    t.is_applying_sticky <- was_applying;
    apply_pending_scroll_by t;
    set_child_offsets t)

(* ───── Render ───── *)

let render_scroll_box t _self grid ~delta:_ =
  match t.props.background with
  | None -> ()
  | Some c ->
      let lx = Renderable.x t.node in
      let ly = Renderable.y t.node in
      let lw = Renderable.width t.node in
      let lh = Renderable.height t.node in
      if lw > 0 && lh > 0 then
        Grid.fill_rect grid ~x:lx ~y:ly ~width:lw ~height:lh ~color:c

(* ───── Mouse Wheel ───── *)

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
  | Scroll { direction; delta } ->
      let direction =
        remap_scroll_direction direction (Event.Mouse.modifiers event).shift
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
      if dx <> 0 || dy <> 0 then begin
        let mult = Scroll_accel.tick t.scroll_accel ~now:t.frame_clock in
        let fx = float_of_int dx *. mult in
        let fy = float_of_int dy *. mult in
        t.wheel_acc_x <- t.wheel_acc_x +. fx;
        t.wheel_acc_y <- t.wheel_acc_y +. fy;
        let stepx = int_of_float t.wheel_acc_x in
        let stepy = int_of_float t.wheel_acc_y in
        if stepx <> 0 then t.wheel_acc_x <- t.wheel_acc_x -. float_of_int stepx;
        if stepy <> 0 then t.wheel_acc_y <- t.wheel_acc_y -. float_of_int stepy;
        if stepx <> 0 || stepy <> 0 then begin
          let previous_x = t.scroll_x and previous_y = t.scroll_y in
          scroll_to_internal t ~x:(t.scroll_x + stepx) ~y:(t.scroll_y + stepy)
            ();
          if t.scroll_x <> previous_x || t.scroll_y <> previous_y then
            Event.Mouse.stop_propagation event
        end
      end
  | Drag { button = Left; is_dragging = true } ->
      update_auto_scroll t (Event.Mouse.x event) (Event.Mouse.y event)
  | Up { button = Left; _ } -> stop_auto_scroll t
  | _ -> ()

(* ───── Keyboard ───── *)

let handle_key t (event : Event.key) =
  let consumed = Scroll_bar.handle_key t.vertical_bar event in
  let consumed = consumed || Scroll_bar.handle_key t.horizontal_bar event in
  if consumed then (
    t.has_manual_scroll <- true;
    Scroll_accel.reset t.scroll_accel;
    t.wheel_acc_x <- 0.;
    t.wheel_acc_y <- 0.;
    true)
  else false

(* ───── Setters ───── *)

let set_sticky_scroll t v =
  if t.props.sticky_scroll <> v then (
    t.props <- { t.props with sticky_scroll = v };
    update_sticky_state t;
    Renderable.request_render t.node)

let set_sticky_start t edge =
  if t.props.sticky_start <> edge then (
    t.props <- { t.props with sticky_start = edge };
    update_sticky_state t;
    Renderable.request_render t.node)

let set_background t color =
  if not (Option.equal Ansi.Color.equal t.props.background color) then (
    t.props <- { t.props with background = color };
    Renderable.request_render t.node)

let set_reveal t reveal =
  if not (Option.equal equal_reveal t.props.reveal reveal) then (
    t.props <- { t.props with reveal };
    t.pending_reveal <- reveal;
    if Option.is_none reveal then t.applied_reveal_key <- None;
    Renderable.request_render t.node)

let set_scroll_by t scroll_by =
  validate_scroll_by scroll_by;
  if not (Option.equal equal_scroll_by t.props.scroll_by scroll_by) then (
    t.props <- { t.props with scroll_by };
    t.pending_scroll_by <- scroll_by;
    Renderable.request_render t.node)

let set_reset_sticky t reset_sticky =
  if not (Option.equal String.equal t.props.reset_sticky reset_sticky) then (
    t.props <- { t.props with reset_sticky };
    t.pending_reset_sticky <- reset_sticky;
    Renderable.request_render t.node)

let set_on_scroll t cb = t.on_scroll <- cb
let set_on_scroll_by_applied t cb = t.on_scroll_by_applied <- cb
let set_on_reset_sticky_applied t cb = t.on_reset_sticky_applied <- cb
let set_scroll_accel t accel = t.scroll_accel <- accel

(* ───── Apply Props ───── *)

let resolve_bar_props shared specific =
  match (shared, specific) with
  | None, None -> None
  | Some p, None -> Some p
  | None, Some p -> Some p
  | Some _, Some specific -> Some specific

(* Hidden bars use the manual-visibility override so overflow recalculation
   does not bring them back; showing again restores automatic visibility. *)
let set_show_scrollbars t v =
  if t.props.show_scrollbars <> v then begin
    t.props <- { t.props with show_scrollbars = v };
    if v then begin
      Scroll_bar.reset_visibility_control t.vertical_bar;
      Scroll_bar.reset_visibility_control t.horizontal_bar
    end
    else begin
      Scroll_bar.set_visible_override t.vertical_bar false;
      Scroll_bar.set_visible_override t.horizontal_bar false
    end
  end

let apply_props t (props : Props.t) =
  set_background t props.background;
  set_sticky_scroll t props.sticky_scroll;
  set_sticky_start t props.sticky_start;
  set_reveal t props.reveal;
  set_scroll_by t props.scroll_by;
  set_reset_sticky t props.reset_sticky;
  set_show_scrollbars t props.show_scrollbars;
  (match resolve_bar_props props.scrollbar_props props.vertical_bar_props with
  | Some p -> Scroll_bar.apply_props t.vertical_bar p
  | None -> ());
  match resolve_bar_props props.scrollbar_props props.horizontal_bar_props with
  | Some p -> Scroll_bar.apply_props t.horizontal_bar p
  | None -> ()

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    ?(scroll_x = false) ?(scroll_y = true) ?(sticky_scroll = false)
    ?sticky_start ?background ?(scroll_accel = Scroll_accel.linear ())
    ?(show_scrollbars = true) ?scrollbar_props ?vertical_bar_props
    ?horizontal_bar_props ?reveal ?scroll_by ?reset_sticky ?on_scroll
    ?on_scroll_by_applied ?on_reset_sticky_applied () =
  validate_scroll_by scroll_by;
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  (* Root: flex-row, align-items stretch *)
  let zero = Toffee.Style.Dimension.length 0. in
  let root_style =
    Renderable.style node
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row
    |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Stretch)
    |> Toffee.Style.set_min_size (Toffee.Geometry.Size.square zero)
  in
  Renderable.set_style node root_style;
  (* Wrapper: flex-column, flex-grow 1 *)
  let wrapper = Renderable.create ~parent:node () in
  let wrapper_style =
    Renderable.style wrapper
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_flex_grow 1.
    |> Toffee.Style.set_min_size (Toffee.Geometry.Size.square zero)
  in
  Renderable.set_style wrapper wrapper_style;
  (* Viewport: flex-column, flex-grow 1, overflow hidden *)
  let viewport_node = Renderable.create ~parent:wrapper () in
  let hidden =
    Toffee.Geometry.Point.
      { x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
  in
  let viewport_style =
    Renderable.style viewport_node
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_flex_grow 1.
    |> Toffee.Style.set_min_size (Toffee.Geometry.Size.square zero)
    |> Toffee.Style.set_overflow hidden
  in
  Renderable.set_style viewport_node viewport_style;
  (* Content: flex-column, flex-shrink 0, min 100% on both axes *)
  let content_node = Renderable.create ~parent:viewport_node () in
  let pct100 = Toffee.Style.Dimension.percent 1.0 in
  let content_style =
    let s = Renderable.style content_node in
    let s =
      Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column s
    in
    let s =
      Toffee.Style.set_align_self (Some Toffee.Style.Align_items.Flex_start) s
    in
    let s = Toffee.Style.set_flex_shrink 0. s in
    let min_sz = Toffee.Style.min_size s in
    let max_sz = Toffee.Style.max_size s in
    let min_sz = { min_sz with width = pct100 } in
    let max_sz = if scroll_x then max_sz else { max_sz with width = pct100 } in
    let min_sz = { min_sz with height = pct100 } in
    let max_sz = if scroll_y then max_sz else { max_sz with height = pct100 } in
    let s = Toffee.Style.set_min_size min_sz s in
    Toffee.Style.set_max_size max_sz s
  in
  Renderable.set_style content_node content_style;
  (* Viewport culling (opentui ScrollBox viewportCulling, default on): only
     the children intersecting the viewport are laid out, updated, and
     rendered each frame, so a long transcript pays O(viewport) rather than
     O(content) per frame. *)
  Renderable.Private.set_viewport_cull content_node true;
  (* Horizontal scroll bar: inside wrapper, below viewport *)
  let horizontal_bar =
    let bar = Scroll_bar.create ~parent:wrapper ~orientation:`Horizontal () in
    (match resolve_bar_props scrollbar_props horizontal_bar_props with
    | Some p -> Scroll_bar.apply_props bar p
    | None -> ());
    bar
  in
  (* Vertical scroll bar: inside root, right of wrapper *)
  let vertical_bar =
    let bar = Scroll_bar.create ~parent:node ~orientation:`Vertical () in
    (match resolve_bar_props scrollbar_props vertical_bar_props with
    | Some p -> Scroll_bar.apply_props bar p
    | None -> ());
    bar
  in
  if not show_scrollbars then begin
    Scroll_bar.set_visible_override vertical_bar false;
    Scroll_bar.set_visible_override horizontal_bar false
  end;
  (* Route user children to content after internal structure is built. *)
  Renderable.set_child_target node (Some content_node);
  let props =
    {
      Props.scroll_x;
      scroll_y;
      sticky_scroll;
      sticky_start;
      background;
      show_scrollbars;
      scrollbar_props;
      vertical_bar_props;
      horizontal_bar_props;
      reveal;
      scroll_by;
      reset_sticky;
    }
  in
  let t =
    {
      node;
      viewport = viewport_node;
      content = content_node;
      props;
      vertical_bar;
      horizontal_bar;
      scroll_x = 0;
      scroll_y = 0;
      max_scroll_x = 0;
      max_scroll_y = 0;
      content_w = 0;
      content_h = 0;
      on_scroll;
      on_scroll_by_applied;
      on_reset_sticky_applied;
      scroll_accel;
      frame_clock = 0.;
      has_manual_scroll = false;
      sticky_top = false;
      sticky_bottom = false;
      sticky_left = false;
      sticky_right = false;
      wheel_acc_x = 0.;
      wheel_acc_y = 0.;
      auto_scroll_mouse_x = 0;
      auto_scroll_mouse_y = 0;
      cached_auto_scroll_speed = auto_scroll_speed_slow;
      auto_scroll_acc_x = 0.;
      auto_scroll_acc_y = 0.;
      auto_scrolling = false;
      is_applying_sticky = false;
      pending_reveal = reveal;
      applied_reveal_key = None;
      pending_scroll_by = scroll_by;
      applied_scroll_by_key = None;
      pending_reset_sticky = reset_sticky;
      applied_reset_sticky_key = None;
    }
  in
  (* Wire scroll bar change → scroll position *)
  Scroll_bar.set_on_change t.vertical_bar
    (Some
       (fun pos ->
         scroll_to_internal t ~manual:(not t.is_applying_sticky) ~y:pos ()));
  Scroll_bar.set_on_change t.horizontal_bar
    (Some
       (fun pos ->
         scroll_to_internal t ~manual:(not t.is_applying_sticky) ~x:pos ()));
  (* Wire mouse handler *)
  Renderable.on_mouse node (on_mouse t);
  (* Wire keyboard handler *)
  Renderable.set_default_key_handler node
    (Some (fun ev -> if handle_key t ev then Event.Key.prevent_default ev));
  Renderable.set_focusable node true;
  (* Wire update and render callbacks. The update hook lives on the content
     node so the offset settles right before its children are culled. *)
  Renderable.set_on_update content_node
    (Some (fun _ ~delta -> update_scroll_box t ~delta));
  Renderable.set_render node (render_scroll_box t);
  Renderable.set_on_frame node
    (Some (fun _ ~delta -> handle_auto_scroll t delta));
  (* Wire resize notifications *)
  Renderable.set_on_resize viewport_node
    (Some (fun _ -> Renderable.request_render node));
  Renderable.set_on_resize content_node
    (Some (fun _ -> Renderable.request_render node));
  (* Initial sticky state *)
  (match (sticky_scroll, sticky_start) with
  | true, Some `Top -> t.sticky_top <- true
  | true, Some `Bottom -> t.sticky_bottom <- true
  | true, Some `Left -> t.sticky_left <- true
  | true, Some `Right -> t.sticky_right <- true
  | _ -> ());
  set_child_offsets t;
  Renderable.request_render node;
  t

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "ScrollBox(%s, x=%d, y=%d, cw=%d, ch=%d, vw=%d, vh=%d)"
    (Renderable.id t.node) t.scroll_x t.scroll_y t.content_w t.content_h
    (viewport_width t) (viewport_height t)
