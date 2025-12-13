type preset = Dots | Line | Circle | Bounce | Bar | Arrow

let frames_of_preset = function
  | Dots -> [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
  | Line -> [| "-"; "\\"; "|"; "/" |]
  | Circle -> [| "◐"; "◓"; "◑"; "◒" |]
  | Bounce -> [| "⠁"; "⠂"; "⠄"; "⠂" |]
  | Bar ->
      [|
        "[    ]";
        "[=   ]";
        "[==  ]";
        "[=== ]";
        "[====]";
        "[ ===]";
        "[  ==]";
        "[   =]";
      |]
  | Arrow -> [| "←"; "↖"; "↑"; "↗"; "→"; "↘"; "↓"; "↙" |]

let interval_of_preset = function
  | Dots -> 0.08
  | Line -> 0.1
  | Circle -> 0.1
  | Bounce -> 0.12
  | Bar -> 0.1
  | Arrow -> 0.1

module Props = struct
  type t = {
    preset : preset;
    frames : string array option;
    interval : float;
    autoplay : bool;
    color : Ansi.Color.t;
    background : Ansi.Color.t;
  }

  let make ?(preset = Dots) ?frames ?(interval = 0.08) ?(autoplay = true)
      ?(color = Ansi.Color.white) ?(background = Ansi.Color.of_rgba 0 0 0 0) ()
      =
    { preset; frames; interval; autoplay; color; background }

  let default = make ()

  let equal (a : t) (b : t) =
    let frames_equal fa fb =
      match (fa, fb) with
      | None, None -> true
      | Some xa, Some xb ->
          Array.length xa = Array.length xb && Array.for_all2 String.equal xa xb
      | _ -> false
    in
    a.preset = b.preset
    && frames_equal a.frames b.frames
    && Float.equal a.interval b.interval
    && Bool.equal a.autoplay b.autoplay
    && Ansi.Color.equal a.color b.color
    && Ansi.Color.equal a.background b.background
end

type t = {
  node : Renderable.t;
  mutable frames : string array;
  mutable interval : float;
  mutable color : Ansi.Color.t;
  mutable background : Ansi.Color.t;
  mutable current_frame : int;
  mutable running : bool;
  mutable elapsed : float;
  mutable max_width : int;
}

let node t = t.node

let compute_max_width frames =
  Array.fold_left
    (fun acc frame ->
      max acc (Glyph.measure ~width_method:`Unicode ~tab_width:2 frame))
    0 frames

let request_render t = Renderable.request_render t.node

let measure t ~known_dimensions:_ ~available_space:_ ~style:_ =
  Toffee.Geometry.Size.{ width = float t.max_width; height = 1. }

let render t _renderable grid ~delta:_ =
  let lx = Renderable.x t.node in
  let ly = Renderable.y t.node in
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  if lw <= 0 || lh <= 0 then ()
  else
    let frame_idx = t.current_frame mod Array.length t.frames in
    let frame = t.frames.(frame_idx) in
    let style = Ansi.Style.make ~fg:t.color ~bg:t.background () in
    Grid.draw_text ~style grid ~x:lx ~y:ly ~text:frame

let on_frame t ~delta =
  if not t.running then ()
  else (
    t.elapsed <- t.elapsed +. delta;
    if t.elapsed >= t.interval then (
      t.elapsed <- t.elapsed -. t.interval;
      t.current_frame <- (t.current_frame + 1) mod Array.length t.frames;
      request_render t))

let start t =
  if not t.running then (
    t.running <- true;
    Renderable.set_live t.node true;
    request_render t)

let stop t =
  if t.running then (
    t.running <- false;
    Renderable.set_live t.node false)

let reset t =
  stop t;
  t.current_frame <- 0;
  t.elapsed <- 0.;
  request_render t

let is_running t = t.running

let set_frames t frames =
  if Array.length frames = 0 then ()
  else (
    t.frames <- frames;
    t.max_width <- compute_max_width frames;
    t.current_frame <- 0;
    t.elapsed <- 0.;
    ignore (Renderable.mark_layout_dirty t.node);
    request_render t)

let set_preset t preset =
  let frames = frames_of_preset preset in
  let interval = interval_of_preset preset in
  t.frames <- frames;
  t.interval <- interval;
  t.max_width <- compute_max_width frames;
  t.current_frame <- 0;
  t.elapsed <- 0.;
  ignore (Renderable.mark_layout_dirty t.node);
  request_render t

let set_interval t interval =
  t.interval <- max 0.001 interval;
  t.elapsed <- 0.

let set_color t color =
  if not (Ansi.Color.equal t.color color) then (
    t.color <- color;
    request_render t)

let set_background t color =
  if not (Ansi.Color.equal t.background color) then (
    t.background <- color;
    request_render t)

let current_frame t = t.current_frame
let frame_count t = Array.length t.frames

let apply_props t (props : Props.t) =
  (match props.frames with
  | Some frames when Array.length frames > 0 -> set_frames t frames
  | _ -> set_preset t props.preset);
  let interval =
    if props.interval > 0. then props.interval
    else interval_of_preset props.preset
  in
  set_interval t interval;
  set_color t props.color;
  set_background t props.background;
  if props.autoplay then start t else stop t

let mount ?(props = Props.default) node =
  let frames =
    match props.frames with
    | Some f when Array.length f > 0 -> f
    | _ -> frames_of_preset props.preset
  in
  let interval =
    if props.interval > 0. then props.interval
    else interval_of_preset props.preset
  in
  let t =
    {
      node;
      frames;
      interval;
      color = props.color;
      background = props.background;
      current_frame = 0;
      running = false;
      elapsed = 0.;
      max_width = compute_max_width frames;
    }
  in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_on_frame node (Some (fun _n ~delta -> on_frame t ~delta));
  if props.autoplay then start t;
  request_render t;
  t
