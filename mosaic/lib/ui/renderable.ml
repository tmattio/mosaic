module Style = Toffee.Style
module Size = Toffee.Geometry.Size
module Available_space = Toffee.Available_space

let ( let* ) = Result.bind

module Props = struct
  type buffer_mode = [ `None | `Self ]

  type t = {
    id : string;
    style : Toffee.Style.t;
    visible : bool;
    z_index : int;
    buffer : buffer_mode;
    live : bool;
  }

  let make ~id ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
      ?(buffer = `None) ?(live = false) () =
    { id; style; visible; z_index; buffer; live }

  let id t = t.id
  let visible t = t.visible
  let z_index t = t.z_index
  let buffer t = t.buffer
  let live t = t.live
end

module Error = struct
  type t = Layout_error of Toffee.Error.t | Tree_mismatch

  let map_toffee_error r = Result.map_error (fun e -> Layout_error e) r
end

type error = Error.t = Layout_error of Toffee.Error.t | Tree_mismatch

module Cached_layout = struct
  type t = { x : float; y : float; width : float; height : float; valid : bool }

  let zero = { x = 0.; y = 0.; width = 0.; height = 0.; valid = false }
  let create ~x ~y ~width ~height = { x; y; width; height; valid = true }
end

type 'a event_handler = 'a -> unit
type visible_children = [ `All | `Subset of int list ]

type measure =
  known_dimensions:float option Toffee.Geometry.Size.t ->
  available_space:Toffee.Available_space.t Toffee.Geometry.Size.t ->
  style:Toffee.Style.t ->
  float Toffee.Geometry.Size.t

type cursor_style = [ `Block | `Line | `Underline ]

type hardware_cursor = {
  x : int;
  y : int;
  color : Ansi.Color.t;
  style : cursor_style;
  blinking : bool;
}

module Select = struct
  type capability = {
    should_start : x:int -> y:int -> bool;
    on_change : Selection.t option -> bool;
    clear : unit -> unit;
    get_text : unit -> string;
  }
end

type node = {
  tree : unit Toffee.tree;
  node : Toffee.Node_id.t;
  mutable props : Props.t;
  mutable original_display : Toffee.Style.display;
  num : int;
  mutable parent : node option;
  mutable children : node option array;
  mutable child_count : int;
  mutable render : render;
  mutable layout_dirty : bool;
  mutable glyph_pool : Glyph.Pool.t option;
  mutable mouse_handlers : Event.mouse event_handler list;
  mutable key_handlers : Event.key event_handler list;
  mutable default_key_handler : (Event.key -> unit) option;
  mutable paste_handler : (Event.paste -> unit) option;
  mutable on_frame : (node -> delta:float -> unit) option;
  mutable on_size_change : (node -> unit) option;
  mutable last_width : int;
  mutable last_height : int;
  mutable focusable : bool;
  mutable focused : bool;
  mutable measure : measure option;
  mutable cached_layout : Cached_layout.t;
  mutable sorted_children_cache : node array;
  mutable sorted_children_len : int;
  mutable sorted_children_dirty : bool;
  mutable primary_sorted_children_cache : node array;
  mutable primary_sorted_children_len : int;
  mutable primary_sorted_dirty : bool;
  mutable primary_axis_cache : [ `Row | `Column ];
  mutable buffer : Grid.t option;
  mutable render_before : render option;
  mutable render_after : render option;
  mutable selection : Select.capability option;
  mutable self_live : int;
  mutable live_count : int;
  mutable child_clip : (node -> Grid.clip_rect option) option;
  mutable visible_children : (node -> int list) option;
  mutable render_offset_x : int;
  mutable render_offset_y : int;
  mutable schedule : unit -> unit;
  mutable register : node -> unit;
  mutable alloc_num : unit -> int;
  mutable focus_delegate : (node -> bool) option;
  mutable blur_delegate : (node -> unit) option;
  mutable hardware_cursor : (node -> hardware_cursor option) option;
  mutable lifecycle_pass : (node -> unit) option;
  mutable lifecycle_register : node -> unit;
  mutable lifecycle_unregister : node -> unit;
  mutable lifecycle_registered : bool;
  mutable is_root : bool;
  (* Optional redirection target for child mutations (append/insert). *)
  mutable child_sink :
    (child:node -> index:int option -> node * int option) option;
  (* Logical parent to use for reconciliation of user-provided children. *)
  mutable reconcile_parent : node option;
  mutable live_count_change : (node -> unit) option;
}

and render = node -> Grid.t -> delta:float -> unit

type t = node

let invalidate_cache node =
  node.cached_layout <- { Cached_layout.zero with valid = false }

let render_noop _ _ ~delta:_ = ()
let next_inner_num = ref (-1)

let alloc_inner_num () =
  let n = !next_inner_num in
  decr next_inner_num;
  n

let rec adjust_live_count node delta =
  if delta <> 0 then (
    node.live_count <- node.live_count + delta;
    (match node.live_count_change with Some f -> f node | None -> ());
    match node.parent with Some p -> adjust_live_count p delta | None -> ())

let compute_self_live node =
  if node.props.live && node.props.visible then 1 else 0

let update_self_live node =
  let new_self = compute_self_live node in
  let delta = new_self - node.self_live in
  if delta <> 0 then (
    node.self_live <- new_self;
    adjust_live_count node delta;
    node.schedule ())

let is_attached node = node.is_root || Option.is_some node.parent

let register_lifecycle_if_needed node =
  if not node.lifecycle_registered then (
    node.lifecycle_registered <- true;
    node.lifecycle_register node)

let unregister_lifecycle_if_needed node =
  if node.lifecycle_registered then (
    node.lifecycle_registered <- false;
    node.lifecycle_unregister node)

let update_lifecycle_registration node =
  match node.lifecycle_pass with
  | Some _ when is_attached node -> register_lifecycle_if_needed node
  | _ -> unregister_lifecycle_if_needed node

let create ~layout ?id ?props ?on_frame ?on_size_change ?(render = render_noop)
    ?render_before ?render_after ?glyph_pool ~num () =
  let id = Option.value id ~default:(Printf.sprintf "renderable-%d" num) in
  let props = Option.value props ~default:(Props.make ~id ()) in
  let* node = Error.map_toffee_error (Toffee.new_leaf layout props.style) in
  let self_live = if props.live && props.visible then 1 else 0 in
  let t =
    {
      tree = layout;
      node;
      props;
      original_display = Toffee.Style.display props.style;
      num;
      parent = None;
      children = [||];
      child_count = 0;
      render;
      layout_dirty = true;
      glyph_pool;
      mouse_handlers = [];
      key_handlers = [];
      default_key_handler = None;
      paste_handler = None;
      on_frame;
      on_size_change;
      last_width = 0;
      last_height = 0;
      focusable = false;
      focused = false;
      measure = None;
      cached_layout = Cached_layout.zero;
      sorted_children_cache = [||];
      sorted_children_len = 0;
      sorted_children_dirty = false;
      primary_sorted_children_cache = [||];
      primary_sorted_children_len = 0;
      primary_sorted_dirty = false;
      primary_axis_cache = `Row;
      buffer = None;
      render_before;
      render_after;
      selection = None;
      self_live;
      live_count = self_live;
      child_clip = None;
      visible_children = None;
      render_offset_x = 0;
      render_offset_y = 0;
      schedule = (fun () -> ());
      register = (fun _ -> ());
      alloc_num = alloc_inner_num;
      focus_delegate = None;
      blur_delegate = None;
      hardware_cursor = None;
      lifecycle_pass = None;
      lifecycle_register = (fun _ -> ());
      lifecycle_unregister = (fun _ -> ());
      lifecycle_registered = false;
      is_root = false;
      child_sink = None;
      reconcile_parent = None;
      live_count_change = None;
    }
  in
  (if not props.visible then
     let hidden =
       Toffee.Style.set_display Toffee.Style.Display.None t.props.style
     in
     match Toffee.set_style t.tree t.node hidden with
     | Ok () ->
         t.props <- { t.props with visible = false };
         t.layout_dirty <- true;
         invalidate_cache t;
         update_self_live t
     | Error _ -> ());
  Ok t

let set_render t fn =
  t.render <- fn;
  t.schedule ()

let create_child ~parent ?id ?props ?on_frame ?on_size_change ?render () =
  let layout = parent.tree in
  let num = parent.alloc_num () in
  match
    create ~layout ?id ?props ?on_frame ?on_size_change ?render
      ?glyph_pool:parent.glyph_pool ~num ()
  with
  | Error e -> Error e
  | Ok child ->
      child.schedule <- parent.schedule;
      child.focus_delegate <- parent.focus_delegate;
      child.blur_delegate <- parent.blur_delegate;
      child.lifecycle_register <- parent.lifecycle_register;
      child.lifecycle_unregister <- parent.lifecycle_unregister;
      child.lifecycle_registered <- false;
      child.register <- parent.register;
      child.alloc_num <- parent.alloc_num;
      child.register child;
      Ok child

let focusable t = t.focusable
let set_focusable t value = t.focusable <- value
let focused t = t.focused

let focus_direct t =
  if not t.focusable then false
  else if t.focused then true
  else (
    t.focused <- true;
    t.schedule ();
    true)

let blur_direct t =
  if t.focused then (
    t.focused <- false;
    t.schedule ())

let focus t =
  if not t.focusable then false
  else match t.focus_delegate with Some f -> f t | None -> focus_direct t

let blur t =
  if not t.focused then ()
  else match t.blur_delegate with Some f -> f t | None -> blur_direct t

let ignore_result = function Ok () -> () | Error _ -> ()
let request_render t = t.schedule ()
let mark_rendered _t = ()
let set_glyph_pool t pool = t.glyph_pool <- Some pool
let glyph_pool t = t.glyph_pool

let ensure_children_capacity node needed =
  let current = Array.length node.children in
  if needed <= current then ()
  else
    let new_capacity =
      let rec grow cap =
        if cap >= needed then cap else grow (max 1 (cap * 2))
      in
      if current = 0 then grow 4 else grow (current * 2)
    in
    let new_children = Array.make new_capacity None in
    Array.blit node.children 0 new_children 0 current;
    node.children <- new_children

let insert_child_at parent idx child =
  let idx =
    if idx < 0 then 0
    else if idx > parent.child_count then parent.child_count
    else idx
  in
  ensure_children_capacity parent (parent.child_count + 1);
  for i = parent.child_count downto idx + 1 do
    parent.children.(i) <- parent.children.(i - 1)
  done;
  parent.children.(idx) <- Some child;
  parent.child_count <- parent.child_count + 1;
  parent.sorted_children_dirty <- true;
  parent.primary_sorted_dirty <- true;
  adjust_live_count parent child.live_count;
  request_render parent;
  idx

let rec find_child_index parent child idx =
  if idx >= parent.child_count then None
  else
    match parent.children.(idx) with
    | Some c when c == child -> Some idx
    | _ -> find_child_index parent child (idx + 1)

let remove_child_reference parent child =
  match find_child_index parent child 0 with
  | None -> ()
  | Some idx ->
      for i = idx to parent.child_count - 2 do
        parent.children.(i) <- parent.children.(i + 1)
      done;
      if parent.child_count > 0 then
        parent.children.(parent.child_count - 1) <- None;
      parent.child_count <- max 0 (parent.child_count - 1);
      parent.sorted_children_dirty <- true;
      parent.primary_sorted_dirty <- true;
      adjust_live_count parent (-child.live_count);
      request_render parent

let attach_child parent child add_fn =
  if not (parent.tree == child.tree) then Error Error.Tree_mismatch
  else
    let detach_child child =
      match child.parent with
      | None -> Ok ()
      | Some prev ->
          remove_child_reference prev child;
          child.parent <- None;
          Error.map_toffee_error
            (Toffee.remove_child prev.tree prev.node child.node
            |> Result.map (fun _ -> ()))
    in
    let* () = detach_child child in
    let* () = Error.map_toffee_error (add_fn parent.node child.node) in
    child.parent <- Some parent;
    child.is_root <- false;
    update_lifecycle_registration child;
    invalidate_cache child;
    parent.layout_dirty <- true;
    child.layout_dirty <- true;
    Ok ()

let resolve_child_target t ~child ~index =
  match t.child_sink with None -> (t, index) | Some f -> f ~child ~index

let append_child ~parent ~child =
  if parent == child then Error Tree_mismatch
  else if child.parent != None then Error Tree_mismatch
  else
    let parent, index_override =
      resolve_child_target parent ~child ~index:None
    in
    let add_fn parent_id child_id =
      Toffee.add_child parent.tree parent_id child_id
    in
    let* () = attach_child parent child add_fn in
    let target_index =
      match index_override with
      | Some i when i <= 0 -> 0
      | Some i when i >= parent.child_count -> parent.child_count
      | Some i -> i
      | None -> parent.child_count
    in
    let _ = insert_child_at parent target_index child in
    Ok ()

let insert_child ~parent ~index ~child =
  if parent == child then Error Tree_mismatch
  else if child.parent != None then Error Tree_mismatch
  else
    let parent, index_override =
      resolve_child_target parent ~child ~index:(Some index)
    in
    let target_index =
      match index_override with
      | Some i when i <= 0 -> 0
      | Some i when i >= parent.child_count -> parent.child_count
      | Some i -> i
      | None ->
          if index <= 0 then 0
          else if index >= parent.child_count then parent.child_count
          else index
    in
    let add_fn parent_id child_id =
      Toffee.insert_child_at_index parent.tree parent_id target_index child_id
    in
    let* () = attach_child parent child add_fn in
    let _ = insert_child_at parent target_index child in
    Ok ()

let detach child =
  if focused child then blur child;
  match child.parent with
  | None -> Ok ()
  | Some parent ->
      remove_child_reference parent child;
      child.parent <- None;
      update_lifecycle_registration child;
      invalidate_cache child;
      parent.layout_dirty <- true;
      Error.map_toffee_error
        (Toffee.remove_child parent.tree parent.node child.node
        |> Result.map (fun _ -> ()))

let remove node =
  let* () = detach node in
  Error.map_toffee_error
    (Toffee.remove node.tree node.node |> Result.map (fun _ -> ()))

let id t = t.props.id
let parent t = t.parent

let children t =
  let rec loop acc idx =
    if idx < 0 then acc
    else
      match t.children.(idx) with
      | Some child -> loop (child :: acc) (idx - 1)
      | None -> loop acc (idx - 1)
  in
  loop [] (t.child_count - 1)

let set_child_sink t sink = t.child_sink <- sink
let reconcile_parent t = Option.value t.reconcile_parent ~default:t
let set_reconcile_parent t parent = t.reconcile_parent <- Some parent
let clear_reconcile_parent t = t.reconcile_parent <- None

let sorted_children (t : t) : t array =
  if t.child_count = 0 then (
    if t.sorted_children_len <> 0 || Array.length t.sorted_children_cache <> 0
    then (
      t.sorted_children_cache <- [||];
      t.sorted_children_len <- 0;
      t.sorted_children_dirty <- false);
    [||])
  else if t.sorted_children_dirty || t.sorted_children_len <> t.child_count then (
    let cache = Array.make t.child_count t in
    for i = 0 to t.child_count - 1 do
      match t.children.(i) with Some c -> cache.(i) <- c | None -> ()
    done;
    for i = 1 to t.child_count - 1 do
      let key = cache.(i) in
      let zi = key.props.z_index in
      let j = ref (i - 1) in
      while !j >= 0 && cache.(!j).props.z_index > zi do
        cache.(!j + 1) <- cache.(!j);
        decr j
      done;
      cache.(!j + 1) <- key
    done;
    t.sorted_children_cache <- cache;
    t.sorted_children_len <- t.child_count;
    t.sorted_children_dirty <- false;
    t.sorted_children_cache)
  else t.sorted_children_cache

let iter_sorted_children (t : t) (f : t -> unit) : unit =
  let arr = sorted_children t in
  for i = 0 to t.sorted_children_len - 1 do
    f arr.(i)
  done

let clear_layout_dirty t = t.layout_dirty <- false
let visible t = t.props.visible
let z_index t = t.props.z_index

let set_style t style =
  let style =
    let sz = Toffee.Style.size style in
    let has_explicit_dim =
      Toffee.Style.Dimension.is_length sz.width
      || Toffee.Style.Dimension.is_length sz.height
    in
    if has_explicit_dim && Float.equal (Toffee.Style.flex_shrink style) 1.0 then
      Toffee.Style.set_flex_shrink 0.0 style
    else style
  in
  match Toffee.set_style t.tree t.node style with
  | Error e -> Error (Error.Layout_error e)
  | Ok () ->
      t.props <- { t.props with style };
      let display = Toffee.Style.display style in
      if not (Toffee.Style.Display.is_none display) then
        t.original_display <- display;
      t.layout_dirty <- true;
      t.primary_sorted_dirty <- true;
      t.schedule ();
      Ok ()

let style t =
  match Toffee.style t.tree t.node with Ok s -> s | Error _ -> t.props.style

let mark_layout_dirty t =
  t.layout_dirty <- true;
  Error.map_toffee_error (Toffee.mark_dirty t.tree t.node)

let layout_dirty t = t.layout_dirty

let set_measure t measure =
  t.measure <- measure;
  ignore_result (mark_layout_dirty t)

let set_child_clip t child_clip = t.child_clip <- child_clip
let set_visible_children_selector t selector = t.visible_children <- selector

let visible_children t : visible_children =
  match t.visible_children with
  | None -> `All
  | Some f -> ( match f t with [] -> `Subset [] | lst -> `Subset lst)

let set_render_offset t ~x ~y =
  if t.render_offset_x <> x || t.render_offset_y <> y then (
    t.render_offset_x <- x;
    t.render_offset_y <- y;
    (match t.parent with
    | Some p -> p.primary_sorted_dirty <- true
    | None -> ());
    t.schedule ())

let render_offset t = (t.render_offset_x, t.render_offset_y)
let node_id t = t.node
let number t = t.num

let update_cached_layout t ~x ~y ~width ~height =
  let prev = t.cached_layout in
  t.cached_layout <- Cached_layout.create ~x ~y ~width ~height;
  let pos_changed =
    prev.Cached_layout.valid
    && (Float.round prev.x <> Float.round x
       || Float.round prev.y <> Float.round y)
  in
  if pos_changed then
    match t.parent with Some p -> p.primary_sorted_dirty <- true | None -> ()

let measure_of t = t.measure

let render_offset_acc (n : t) =
  let rec loop node accx accy =
    let accx = accx + node.render_offset_x
    and accy = accy + node.render_offset_y in
    match node.parent with None -> (accx, accy) | Some p -> loop p accx accy
  in
  loop n 0 0

let x t =
  let ox, _ = render_offset_acc t in
  if t.cached_layout.Cached_layout.valid then
    int_of_float (Float.round t.cached_layout.x) + ox
  else ox

let y t =
  let _, oy = render_offset_acc t in
  if t.cached_layout.Cached_layout.valid then
    int_of_float (Float.round t.cached_layout.y) + oy
  else oy

let width t =
  if t.cached_layout.Cached_layout.valid then
    max 1 (int_of_float (Float.round t.cached_layout.width))
  else 0

let height t =
  if t.cached_layout.Cached_layout.valid then
    max 1 (int_of_float (Float.round t.cached_layout.height))
  else 0

let bounds (t : t) : Grid.clip_rect =
  { x = x t; y = y t; width = width t; height = height t }

let children_sorted_by_primary (t : t) : t array =
  let module FD = Toffee.Style.Flex_direction in
  let axis =
    let st = t.props.style in
    match Toffee.Style.flex_direction st with
    | FD.Row | FD.Row_reverse -> `Row
    | FD.Column | FD.Column_reverse -> `Column
  in
  if t.child_count = 0 then (
    if
      t.primary_sorted_children_len <> 0
      || Array.length t.primary_sorted_children_cache <> 0
    then (
      t.primary_sorted_children_cache <- [||];
      t.primary_sorted_children_len <- 0;
      t.primary_sorted_dirty <- false;
      t.primary_axis_cache <- axis);
    [||])
  else if
    t.primary_sorted_dirty
    || t.primary_sorted_children_len <> t.child_count
    || t.primary_axis_cache <> axis
  then (
    let cache = Array.make t.child_count t in
    for i = 0 to t.child_count - 1 do
      match t.children.(i) with Some c -> cache.(i) <- c | None -> ()
    done;
    let coord =
      match axis with `Row -> fun n -> x n | `Column -> fun n -> y n
    in
    for i = 1 to t.child_count - 1 do
      let key = cache.(i) in
      let vk = coord key in
      let j = ref (i - 1) in
      while !j >= 0 && coord cache.(!j) > vk do
        cache.(!j + 1) <- cache.(!j);
        decr j
      done;
      cache.(!j + 1) <- key
    done;
    t.primary_sorted_children_cache <- cache;
    t.primary_sorted_children_len <- t.child_count;
    t.primary_sorted_dirty <- false;
    t.primary_axis_cache <- axis;
    t.primary_sorted_children_cache)
  else t.primary_sorted_children_cache

let children_in_viewport ~(parent : t) ~(viewport : Grid.clip_rect)
    ~(padding : int) : t list =
  let module FD = Toffee.Style.Flex_direction in
  let min_trigger_size = 16 in
  let axis =
    let st = style parent in
    match Toffee.Style.flex_direction st with
    | FD.Row | FD.Row_reverse -> `Row
    | FD.Column | FD.Column_reverse -> `Column
  in
  let children = children_sorted_by_primary parent in
  let total = Array.length children in
  if total = 0 then []
  else if total < min_trigger_size then Array.to_list children
  else
    let vp_top = viewport.y - padding in
    let vp_bottom = viewport.y + viewport.height + padding in
    let vp_left = viewport.x - padding in
    let vp_right = viewport.x + viewport.width + padding in
    let vp_start, vp_end =
      match axis with
      | `Row -> (vp_left, vp_right)
      | `Column -> (vp_top, vp_bottom)
    in
    let child_bounds n = bounds n in
    let rec bin lo hi candidate =
      if lo > hi then candidate
      else
        let mid = (lo + hi) lsr 1 in
        let c = children.(mid) in
        let cb = child_bounds c in
        let start = match axis with `Row -> cb.x | `Column -> cb.y in
        let stop =
          start + match axis with `Row -> cb.width | `Column -> cb.height
        in
        if stop < vp_start then bin (mid + 1) hi candidate
        else if start > vp_end then bin lo (mid - 1) candidate
        else bin lo (mid - 1) (Some mid)
    in
    let candidate = bin 0 (total - 1) None in
    let start_index = match candidate with None -> 0 | Some i -> i in
    let max_look_back = 50 in
    let rec walk_left i gaps =
      if i <= 0 then 0
      else
        let prev = children.(i - 1) in
        let cb = child_bounds prev in
        let prev_end =
          match axis with
          | `Row -> cb.x + cb.width
          | `Column -> cb.y + cb.height
        in
        if prev_end <= vp_start then
          if gaps + 1 >= max_look_back then i else walk_left (i - 1) (gaps + 1)
        else walk_left (i - 1) 0
    in
    let left = walk_left start_index 0 in
    let rec walk_right i =
      if i >= total then total
      else
        let next = children.(i) in
        let cb = child_bounds next in
        let start = match axis with `Row -> cb.x | `Column -> cb.y in
        if start >= vp_end then i else walk_right (i + 1)
    in
    let right = walk_right (start_index + 1) in
    let visible = ref [] in
    for i = left to right - 1 do
      let child = children.(i) in
      let cb = child_bounds child in
      let primary_start = match axis with `Row -> cb.x | `Column -> cb.y in
      let primary_end =
        primary_start
        + match axis with `Row -> cb.width | `Column -> cb.height
      in
      if primary_end > vp_start && primary_start < vp_end then
        let cross_ok =
          match axis with
          | `Row ->
              let child_bottom = cb.y + cb.height in
              not (child_bottom < vp_top || cb.y > vp_bottom)
          | `Column ->
              let child_right = cb.x + cb.width in
              not (child_right < vp_left || cb.x > vp_right)
        in
        if cross_ok then visible := child :: !visible
    done;
    let visible = !visible in
    let by_z a b = compare (z_index a) (z_index b) in
    List.sort by_z visible

let set_visible t visible =
  if t.props.visible <> visible then
    let current = style t in
    let display =
      if visible then t.original_display else Toffee.Style.Display.None
    in
    let updated = Toffee.Style.set_display display current in
    match set_style t updated with
    | Error _ -> ()
    | Ok () ->
        t.props <- { t.props with visible };
        update_self_live t;
        if (not visible) && t.focused then blur t

let set_z_index t z =
  if t.props.z_index <> z then (
    t.props <- { t.props with z_index = z };
    (match t.parent with
    | Some p -> p.sorted_children_dirty <- true
    | None -> ());
    t.schedule ())

let buffer t = t.props.buffer

let set_buffer t value =
  if t.props.buffer <> value then (
    t.props <- { t.props with buffer = value };
    if value = `None then t.buffer <- None;
    t.schedule ())

let render_before_hook t = t.render_before
let render_after_hook t = t.render_after
let set_render_before t hook = t.render_before <- hook
let set_render_after t hook = t.render_after <- hook
let live t = t.props.live

let set_live t value =
  if t.props.live <> value then (
    t.props <- { t.props with live = value };
    update_self_live t)

let live_count t = t.live_count

(* Event Handlers *)

let on_mouse t handler = t.mouse_handlers <- handler :: t.mouse_handlers
let on_key_down t handler = t.key_handlers <- handler :: t.key_handlers
let set_default_key_handler t handler = t.default_key_handler <- handler
let on_paste t handler = t.paste_handler <- Some handler

let rec emit_mouse_event t event =
  List.iter (fun handler -> handler event) t.mouse_handlers;
  if not (Event.Mouse.propagation_stopped event) then
    match t.parent with Some p -> emit_mouse_event p event | None -> ()

let emit_key_event t event =
  let rec run_handlers = function
    | [] -> ()
    | handler :: rest ->
        handler event;
        if not (Event.Key.default_prevented event) then run_handlers rest
  in
  run_handlers t.key_handlers

let emit_default_key_event t event =
  match t.default_key_handler with Some handler -> handler event | None -> ()

let emit_paste_event t event =
  match t.paste_handler with Some handler -> handler event | None -> ()

(* Selection *)

let set_selection t cap = t.selection <- cap
let selectable t = Option.is_some t.selection

let emit_selection_changed t selection : bool =
  match t.selection with Some cap -> cap.on_change selection | None -> false

let clear_selection_by_node t =
  match t.selection with Some cap -> cap.clear () | None -> ()

let should_start_selection t ~x ~y : bool =
  match t.selection with Some cap -> cap.should_start ~x ~y | None -> false

let get_selected_text t =
  match t.selection with Some cap -> cap.get_text () | None -> ""

(* Runtime hook setters *)

let set_on_frame t cb = t.on_frame <- cb
let set_on_size_change t cb = t.on_size_change <- cb

let set_on_lifecycle_pass t cb =
  t.lifecycle_pass <- cb;
  update_lifecycle_registration t

let run_lifecycle_pass t =
  match t.lifecycle_pass with Some f -> f t | None -> ()

(* Exposed helpers for renderer-driven pipeline *)

let child_clip_rect t = match t.child_clip with Some f -> f t | None -> None

let pre_render_update t ~delta =
  (match t.on_frame with Some f -> f t ~delta | None -> ());
  let lw = width t and lh = height t in
  if lw <> t.last_width || lh <> t.last_height then (
    t.last_width <- lw;
    t.last_height <- lh;
    match t.on_size_change with Some f -> f t | None -> ())

let render t grid ~delta =
  t.render t grid ~delta;
  mark_rendered t

let set_schedule t cb = t.schedule <- cb

let set_registration_hooks t ~register ~alloc_num =
  t.register <- register;
  t.alloc_num <- alloc_num

let register_with_renderer t = t.register t
let set_on_live_count_change t cb = t.live_count_change <- cb

let set_focus_controller t ~focus ~blur =
  t.focus_delegate <- Some focus;
  t.blur_delegate <- Some blur

let set_hardware_cursor_provider t provider = t.hardware_cursor <- provider

let hardware_cursor t =
  match t.hardware_cursor with Some f -> f t | None -> None

let set_lifecycle_controllers t ~register ~unregister =
  t.lifecycle_register <- register;
  t.lifecycle_unregister <- unregister;
  t.lifecycle_registered <- false;
  update_lifecycle_registration t

let set_is_root t value =
  if t.is_root <> value then (
    t.is_root <- value;
    update_lifecycle_registration t)

(* Buffered subtree helpers *)

let ensure_frame_buffer (t : t) ~(parent : Grid.t) : Grid.t option =
  let w = width t and h = height t in
  if w <= 0 || h <= 0 then None
  else
    match t.buffer with
    | Some buf ->
        if Grid.width buf <> w || Grid.height buf <> h then
          Grid.resize buf ~width:w ~height:h;
        Some buf
    | None ->
        let buf =
          Grid.create ~width:w ~height:h ~glyph_pool:(Grid.glyph_pool parent)
            ~width_method:(Grid.width_method parent)
            ~respect_alpha:(Grid.respect_alpha parent)
            ()
        in
        t.buffer <- Some buf;
        Some buf

let blit_frame_buffer (t : t) ~(dst : Grid.t) : unit =
  match t.buffer with
  | None -> ()
  | Some buf ->
      let w = width t and h = height t in
      if w > 0 && h > 0 then
        Grid.blit_region ~src:buf ~dst ~src_x:0 ~src_y:0 ~width:w ~height:h
          ~dst_x:(x t) ~dst_y:(y t)

module Internal = struct
  type visible_children = [ `All | `Subset of int list ]

  let create = create
  let node_id = node_id
  let number = number
  let set_schedule = set_schedule
  let set_registration_hooks = set_registration_hooks
  let register_with_renderer = register_with_renderer
  let set_focus_controller = set_focus_controller
  let set_hardware_cursor_provider = set_hardware_cursor_provider
  let hardware_cursor = hardware_cursor
  let focus_direct = focus_direct
  let blur_direct = blur_direct
  let set_lifecycle_controllers = set_lifecycle_controllers
  let set_is_root = set_is_root
  let set_on_live_count_change = set_on_live_count_change
  let live_count = live_count
  let layout_dirty = layout_dirty
  let clear_layout_dirty = clear_layout_dirty
  let update_cached_layout = update_cached_layout
  let pre_render_update = pre_render_update
  let render = render
  let ensure_frame_buffer = ensure_frame_buffer
  let blit_frame_buffer = blit_frame_buffer
  let render_before_hook = render_before_hook
  let render_after_hook = render_after_hook
  let set_render_before = set_render_before
  let set_render_after = set_render_after
  let sorted_children = sorted_children
  let iter_sorted_children = iter_sorted_children
  let children_in_viewport = children_in_viewport
  let child_clip_rect = child_clip_rect
  let visible_children = visible_children
  let emit_mouse_event = emit_mouse_event
  let emit_key_event = emit_key_event
  let emit_default_key_event = emit_default_key_event
  let emit_paste_event = emit_paste_event
  let emit_selection_changed = emit_selection_changed
  let clear_selection_by_node = clear_selection_by_node
  let should_start_selection = should_start_selection
  let get_selected_text = get_selected_text
  let run_lifecycle_pass = run_lifecycle_pass
  let measure_of = measure_of
  let set_glyph_pool = set_glyph_pool
  let glyph_pool = glyph_pool
end
