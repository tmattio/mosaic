module Props = struct
  type wrap_mode = [ `None | `Char | `Word ]

  type t = {
    wrap_mode : wrap_mode;
    tab_indicator : int option;
    tab_indicator_color : Ansi.Color.t option;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    selectable : bool;
    default_style : Ansi.Style.t;
  }

  let make ?(wrap_mode = (`Word : wrap_mode)) ?tab_indicator
      ?tab_indicator_color ?selection_bg ?selection_fg ?(selectable = true)
      ?(default_style = Ansi.Style.default) () =
    {
      wrap_mode;
      tab_indicator;
      tab_indicator_color;
      selection_bg;
      selection_fg;
      selectable;
      default_style;
    }

  let default = make ()

  let equal a b =
    a.wrap_mode = b.wrap_mode
    && Option.equal Int.equal a.tab_indicator b.tab_indicator
    && Option.equal Ansi.Color.equal a.tab_indicator_color b.tab_indicator_color
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
    && Bool.equal a.selectable b.selectable
    && Ansi.Style.equal a.default_style b.default_style
end

type viewport = { x : int; y : int; width : int; height : int }

type t = {
  node : Renderable.t;
  buffer : Text_buffer.t;
  view : Text_buffer_view.t;
  mutable wrap_mode : Props.wrap_mode;
  (* Tracks the wrap mode last applied to the buffer to avoid redundant work *)
  mutable applied_wrap_mode : Text_buffer.wrap_mode;
  mutable default_style : Ansi.Style.t;
  mutable wrap_width_hint : int option;
  mutable viewport : viewport option;
  mutable selection_bg : Ansi.Color.t option;
  mutable selection_fg : Ansi.Color.t option;
  mutable selectable : bool;
}

let node t = t.node
let buffer t = t.buffer
let view t = t.view
let default_style t = t.default_style
let wrap_mode t = t.wrap_mode
let selectable t = t.selectable

let set_default_style t style =
  t.default_style <- style;
  (* Also push defaults into the buffer so styles are composed once. *)
  Text_buffer.set_default_fg t.buffer style.Ansi.Style.fg;
  Text_buffer.set_default_bg t.buffer style.Ansi.Style.bg;
  Text_buffer.set_default_attrs t.buffer (Some style.Ansi.Style.attrs);
  Text_buffer.finalise t.buffer;
  ignore (Renderable.mark_layout_dirty t.node);
  Renderable.request_render t.node

let request_render t = Renderable.request_render t.node

let set_viewport_size t ~width ~height =
  let width = max 0 width in
  let height = max 0 height in
  let updated =
    match t.viewport with
    | Some vp when vp.width = width && vp.height = height -> false
    | Some vp ->
        t.viewport <- Some { vp with width; height };
        true
    | None ->
        t.viewport <- Some { x = 0; y = 0; width; height };
        true
  in
  (match t.wrap_mode with
  | `None -> ()
  | `Char | `Word ->
      let desired = if width > 0 then Some width else None in
      if desired <> t.wrap_width_hint then (
        Text_buffer_view.set_wrap_width t.view desired;
        t.wrap_width_hint <- desired;
        ignore (Renderable.mark_layout_dirty t.node)));
  Text_buffer_view.set_viewport_size t.view ~width ~height;
  if updated then request_render t

let set_wrap_mode t mode =
  if t.wrap_mode <> mode then (
    t.wrap_mode <- mode;
    (match mode with
    | `None ->
        Text_buffer_view.set_wrap_width t.view None;
        t.wrap_width_hint <- None
    | (`Char | `Word) as m ->
        Text_buffer_view.set_wrap_mode t.view m;
        let desired =
          match t.viewport with
          | Some vp when vp.width > 0 -> Some vp.width
          | _ -> t.wrap_width_hint
        in
        Text_buffer_view.set_wrap_width t.view desired;
        t.wrap_width_hint <- desired);
    (match mode with
    | `None -> ()
    | `Char -> t.applied_wrap_mode <- `Char
    | `Word -> t.applied_wrap_mode <- `Word);
    ignore (Renderable.mark_layout_dirty t.node);
    request_render t)

let set_tab_width t w =
  Text_buffer.set_tab_width t.buffer w;
  (* Changing tab width affects visual widths and wrapping; trigger re-measure. *)
  ignore (Renderable.mark_layout_dirty t.node);
  request_render t

let set_tab_indicator t code =
  Text_buffer_view.set_tab_indicator t.view code;
  request_render t

let set_tab_indicator_color t c =
  Text_buffer_view.set_tab_indicator_color t.view c;
  request_render t

let set_selection_bg t c =
  t.selection_bg <- c;
  request_render t

let set_selection_fg t c =
  t.selection_fg <- c;
  request_render t

let set_selection t selection =
  Text_buffer_view.set_selection t.view selection;
  request_render t

let clear_selection t =
  Text_buffer_view.clear_selection t.view;
  request_render t

let make_selection_capability t =
  let node = t.node in
  let view = t.view in
  let capability : Renderable.Select.capability =
    {
      should_start =
        (fun ~x ~y ->
          if not t.selectable then false
          else
            let nx = Renderable.x node in
            let ny = Renderable.y node in
            let nw = Renderable.width node in
            let nh = Renderable.height node in
            x >= nx && x < nx + nw && y >= ny && y < ny + nh);
      on_change =
        (fun sel_opt ->
          match sel_opt with
          | None ->
              Text_buffer_view.clear_selection view;
              Renderable.request_render node;
              false
          | Some sel -> (
              let bounds =
                Selection.convert_global_to_local (Some sel)
                  ~local_origin_x:(Renderable.x node)
                  ~local_origin_y:(Renderable.y node)
              in
              match bounds with
              | None ->
                  Text_buffer_view.clear_selection view;
                  Renderable.request_render node;
                  false
              | Some b ->
                  let style =
                    Ansi.Style.make ?fg:t.selection_fg ?bg:t.selection_bg ()
                  in
                  let changed =
                    Text_buffer_view.set_local_selection view
                      ~anchor_x:b.anchor_x ~anchor_y:b.anchor_y
                      ~focus_x:b.focus_x ~focus_y:b.focus_y ~style
                  in
                  if changed then Renderable.request_render node;
                  Option.is_some (Text_buffer_view.selection_bounds view)));
      clear =
        (fun () ->
          Text_buffer_view.clear_selection view;
          Renderable.request_render node);
      get_text = (fun () -> Text_buffer_view.get_selected_text view);
    }
  in
  capability

let apply_selection_capability t =
  if t.selectable then
    Renderable.set_selection t.node (Some (make_selection_capability t))
  else (
    Renderable.set_selection t.node None;
    Text_buffer_view.clear_selection t.view;
    Renderable.request_render t.node)

let set_selectable t flag =
  if t.selectable <> flag then (
    t.selectable <- flag;
    apply_selection_capability t)

let background_at grid ~x ~y =
  let idx = (y * Grid.width grid) + x in
  Grid.get_background grid idx

let replace_content t writer =
  (* Preserve selection across content updates by refreshing it after
     buffer changes. *)
  let prev_selection = Text_buffer_view.selection t.view in
  Text_buffer.reset t.buffer;
  writer t.buffer;
  Text_buffer.finalise t.buffer;
  (* Re-apply previous selection, if any. *)
  Text_buffer_view.set_selection t.view prev_selection;
  ignore (Renderable.mark_layout_dirty t.node);
  request_render t

let set_plain_text t ?style text =
  let style = Option.value style ~default:t.default_style in
  replace_content t (fun buffer ->
      if text <> "" then
        let chunk =
          Text_buffer.Chunk.
            {
              text = Bytes.of_string text;
              fg = style.Ansi.Style.fg;
              bg = style.Ansi.Style.bg;
              attrs = style.Ansi.Style.attrs;
              link = style.Ansi.Style.link;
            }
        in
        ignore (Text_buffer.write_chunk buffer chunk))

let apply_props t (props : Props.t) =
  (* Wrap behaviour *)
  set_wrap_mode t props.wrap_mode;
  (* Tab indicator configuration *)
  set_tab_indicator t props.tab_indicator;
  set_tab_indicator_color t props.tab_indicator_color;
  (* Selection colours and selectability *)
  set_selection_bg t props.selection_bg;
  set_selection_fg t props.selection_fg;
  set_selectable t props.selectable;
  (* Default text style *)
  set_default_style t props.default_style

let measure t ~known_dimensions ~available_space ~style:_ =
  (match t.wrap_mode with
  | `None -> ()
  | (`Char | `Word) as m ->
      if t.applied_wrap_mode <> m then (
        Text_buffer_view.set_wrap_mode t.view m;
        t.applied_wrap_mode <- m));
  let wrap_enabled = match t.wrap_mode with `None -> false | _ -> true in
  let resolved_width =
    let from_known =
      match known_dimensions with
      | Toffee.Geometry.Size.{ width = Some w; _ } when w > 0. -> Some w
      | _ -> None
    in
    match from_known with
    | Some _ as w -> w
    | None -> (
        let from_available =
          match available_space with
          | Toffee.Geometry.Size.{ width; _ } ->
              Toffee.Available_space.to_option width
        in
        match from_available with
        | Some w when w > 0. -> Some w
        | _ -> (
            match t.viewport with
            | Some vp when vp.width > 0 -> Some (float vp.width)
            | _ -> None))
  in
  let resolved_height =
    let from_known =
      match known_dimensions with
      | Toffee.Geometry.Size.{ height = Some h; _ } when h > 0. -> Some h
      | _ -> None
    in
    match from_known with
    | Some _ as h -> h
    | None -> (
        let from_available =
          match available_space with
          | Toffee.Geometry.Size.{ height; _ } ->
              Toffee.Available_space.to_option height
        in
        match from_available with Some h when h > 0. -> Some h | _ -> None)
  in
  (* Apply wrap width for measurement. *)
  let previous_hint = t.wrap_width_hint in
  let wrap_hint =
    match (wrap_enabled, resolved_width) with
    | false, _ -> None
    | true, Some w -> Some (max 1 (int_of_float (Float.floor w)))
    | true, None -> None
  in
  Text_buffer_view.set_wrap_width t.view wrap_hint;
  t.wrap_width_hint <- wrap_hint;
  Text_buffer.finalise t.buffer;
  (* Changing wrap width can affect layout; mark dirty if hint changed. *)
  if previous_hint <> t.wrap_width_hint then
    ignore (Renderable.mark_layout_dirty t.node);
  let width_for_measure = match wrap_hint with Some w -> w | None -> 0 in
  let height_hint =
    match resolved_height with
    | Some h when h > 0. -> int_of_float (Float.floor h)
    | _ -> 0
  in
  let metrics =
    Text_buffer_view.measure_for_dimensions t.view ~width:width_for_measure
      ~height:height_hint
  in
  let measured_width = max 1 metrics.max_width in
  let measured_height = max 1 metrics.line_count in
  let final_width =
    match resolved_width with
    | Some w when w > 0. -> min measured_width (int_of_float (Float.floor w))
    | _ -> measured_width
  in
  let final_height =
    match resolved_height with
    | Some h when h > 0. -> min measured_height (int_of_float (Float.floor h))
    | _ -> measured_height
  in
  Toffee.Geometry.Size.
    { width = float final_width; height = float final_height }

let render t renderable grid ~delta:_ =
  let lx = Renderable.x renderable in
  let ly = Renderable.y renderable in
  let lw = Renderable.width renderable in
  let lh = Renderable.height renderable in
  (* Do not early-return on zero sizes. We still attempt to render and rely on
     scissor/viewport to cull. This avoids cases where layout reports 0 size
     transiently and text would disappear. *)
  let requested_width =
    match t.wrap_mode with
    | `None -> None
    | _ -> (
        match t.viewport with
        | Some vp when vp.width > 0 -> Some vp.width
        | _ when lw > 0 -> Some lw
        | _ -> None)
  in
  let need_set_mode =
    match t.wrap_mode with
    | `None -> false
    | (`Char | `Word) as m -> t.applied_wrap_mode <> m
  in
  let previous_hint = t.wrap_width_hint in
  let need_set_width = previous_hint <> requested_width in
  (if need_set_mode then
     match t.wrap_mode with
     | `None -> ()
     | (`Char | `Word) as m ->
         Text_buffer_view.set_wrap_mode t.view m;
         t.applied_wrap_mode <- m);
  if need_set_width then (
    Text_buffer_view.set_wrap_width t.view requested_width;
    t.wrap_width_hint <- requested_width);
  if need_set_mode || need_set_width then (
    Text_buffer.finalise t.buffer;
    if need_set_width then ignore (Renderable.mark_layout_dirty t.node));
  (* Align buffer width method with the grid’s width method BEFORE pulling any
       drawing buffers so widths/virtual lines are computed consistently. *)
  let gwm = Grid.width_method grid in
  Text_buffer.set_width_method t.buffer gwm;
  let view = Text_buffer.View.create t.buffer in
  let lines = Text_buffer_view.virtual_lines t.view in
  let curr_tab_indicator = Text_buffer_view.tab_indicator t.view in
  let curr_tab_indicator_color = Text_buffer_view.tab_indicator_color t.view in
  let tab_w = Text_buffer.tab_width t.buffer in
  let sel_bounds = Text_buffer_view.selection_bounds t.view in
  let sel_style = Text_buffer_view.selection_style t.view in
  let resolve_link idx =
    let raw = Text_buffer.View.raw_link view idx in
    if raw = -1 then None else Text_buffer.link_at_index t.buffer raw
  in
  let apply_selection base_fg base_bg idx =
    match sel_bounds with
    | None -> (base_fg, base_bg)
    | Some (s, e) when idx >= s && idx < e -> (
        (* Priority: explicit selection_bg/fg on widget -> selection style bg/fg -> invert fallback *)
        match t.selection_bg with
        | Some sbg ->
            let sfg =
              match t.selection_fg with Some c -> c | None -> base_fg
            in
            (sfg, sbg)
        | None -> (
            match sel_style with
            | Some st -> (
                match st.Ansi.Style.bg with
                | Some sbg ->
                    let sfg =
                      match st.Ansi.Style.fg with
                      | Some c -> c
                      | None -> base_fg
                    in
                    (sfg, sbg)
                | None ->
                    let _r, _g, _b, a = Ansi.Color.to_rgba base_bg in
                    let inv_fg =
                      if a > 0 then base_bg else Ansi.Color.of_rgba 0 0 0 255
                    in
                    (inv_fg, base_fg))
            | None ->
                let _r, _g, _b, a = Ansi.Color.to_rgba base_bg in
                let inv_fg =
                  if a > 0 then base_bg else Ansi.Color.of_rgba 0 0 0 255
                in
                (inv_fg, base_fg)))
    | _ -> (base_fg, base_bg)
  in
  let buffer_width = Grid.width grid in
  let buffer_height = Grid.height grid in
  let write_cell = Grid.set_cell_alpha in
  let vp = t.viewport in
  let start_line = match vp with Some v -> max 0 v.y | None -> 0 in
  let max_lines =
    let visible_h =
      match vp with
      | Some v when v.height > 0 -> v.height
      | _ -> if lh > 0 then lh else buffer_height
    in
    if lh > 0 then min visible_h lh else visible_h
  in
  let end_line = min (Array.length lines) (start_line + max_lines) in
  let draw_width_limit =
    match vp with
    | Some v when v.width > 0 -> v.width
    | _ -> if lw > 0 then lw else max 0 (buffer_width - max 0 lx)
  in
  for line_idx = start_line to end_line - 1 do
    let line = lines.(line_idx) in
    let dest_y = ly + (line_idx - start_line) in
    if dest_y >= 0 && dest_y < buffer_height then
      let rec loop i column =
        if i >= line.length || column >= draw_width_limit then ()
        else
          let idx = line.start_index + i in
          let code = Text_buffer.View.code view idx in
          let base_width = Text_buffer.View.width view idx in
          let fg =
            match Text_buffer.View.fg_opt view idx with
            | Some c -> c
            | None ->
                Option.value t.default_style.Ansi.Style.fg
                  ~default:Ansi.Color.white
          in
          let attrs = Text_buffer.View.attrs view idx in
          if code = 10 then loop (i + 1) column
          else
            let dest_x = lx + column in
            (* When no explicit foreground is provided (neither on the span
                 nor via the default style), fall back to opaque white rather
                 than terminal Default. This ensures selection inversion yields
                 a visible background instead of a transparent one. *)
            if dest_x >= 0 && dest_x < buffer_width then (
              let initial_bg =
                match Text_buffer.View.bg_opt view idx with
                | Some c -> c
                | None ->
                    Option.value t.default_style.Ansi.Style.bg
                      ~default:(background_at grid ~x:dest_x ~y:dest_y)
              in
              let bg =
                if
                  (not (Grid.respect_alpha grid))
                  && Ansi.Color.equal initial_bg Ansi.Color.default
                then background_at grid ~x:dest_x ~y:dest_y
                else initial_bg
              in
              let is_tab = code = 9 in
              if is_tab then (
                let w = max 1 tab_w in
                let next = ((column / w) + 1) * w in
                let draw_width = max 1 (next - column) in
                let base_link = resolve_link idx in
                for off = 0 to draw_width - 1 do
                  let char_code =
                    if off = 0 then
                      match curr_tab_indicator with Some cp -> cp | None -> 32
                    else 32
                  in
                  let char_fg =
                    if off = 0 then
                      match curr_tab_indicator_color with
                      | Some c -> c
                      | None -> fg
                    else fg
                  in
                  let sel_fg, draw_bg = apply_selection char_fg bg idx in
                  let inv = Ansi.Attr.mem Ansi.Attr.Inverse attrs in
                  let cell_attrs =
                    if inv then Ansi.Attr.remove Ansi.Attr.Inverse attrs
                    else attrs
                  in
                  let dx = dest_x + off in
                  if dx >= 0 && dx < buffer_width then
                    let out_fg =
                      if off = 0 then
                        match curr_tab_indicator_color with
                        | Some c -> c
                        | None -> sel_fg
                      else sel_fg
                    in
                    let out_fg, draw_bg =
                      if inv then (draw_bg, out_fg) else (out_fg, draw_bg)
                    in
                    write_cell grid ~x:dx ~y:dest_y ~code:char_code ~fg:out_fg
                      ~bg:draw_bg ~attrs:cell_attrs ?link:base_link ()
                done;
                let next_column = column + draw_width in
                loop (i + 1) next_column)
              else
                (* Preserve continuation semantics: width=0 for continuation cells,
                     positive width only on start cells. This keeps grid invariants
                     stable across redraws. *)
                let is_cont = Glyph.is_continuation code in
                let draw_width = if is_cont then 0 else max 1 base_width in
                let draw_fg, draw_bg = apply_selection fg bg idx in
                let link = resolve_link idx in
                let inv = Ansi.Attr.mem Ansi.Attr.Inverse attrs in
                let cell_attrs =
                  if inv then Ansi.Attr.remove Ansi.Attr.Inverse attrs
                  else attrs
                in
                let draw_fg, draw_bg =
                  if inv then (draw_bg, draw_fg) else (draw_fg, draw_bg)
                in
                write_cell grid ~x:dest_x ~y:dest_y ~code ~fg:draw_fg
                  ~bg:draw_bg ~attrs:cell_attrs ?link ();
                let next_column =
                  column + if draw_width > 0 then draw_width else 0
                in
                loop (i + 1) next_column)
      in
      (* Horizontal offset: honor viewport.x for both wrapped and unwrapped lines *)
      let start_i =
        match vp with
        | Some v when v.x > 0 ->
            let idx_at =
              Text_buffer_view.position_to_index t.view ~x:v.x ~y:line_idx
            in
            max 0 (idx_at - line.start_index)
        | _ -> 0
      in
      loop start_i 0
  done

let mount ?(props = Props.default) node =
  let default_capacity = 128 in
  let pool =
    match Renderable.Internal.glyph_pool node with
    | Some p -> p
    | None -> Glyph.create_pool ()
  in
  let buffer =
    Text_buffer.create ~glyph_pool:pool ~capacity:default_capacity
      ~width_method:`Unicode ()
  in
  let view = Text_buffer_view.create buffer in
  Option.iter
    (fun i -> Text_buffer_view.set_tab_indicator view (Some i))
    props.tab_indicator;
  Option.iter
    (fun c -> Text_buffer_view.set_tab_indicator_color view (Some c))
    props.tab_indicator_color;
  let surface =
    {
      node;
      buffer;
      view;
      wrap_mode = props.wrap_mode;
      applied_wrap_mode =
        (match props.wrap_mode with
        | `None -> `Word
        | `Char -> `Char
        | `Word -> `Word);
      default_style = props.default_style;
      wrap_width_hint = None;
      viewport = None;
      selection_bg = props.selection_bg;
      selection_fg = props.selection_fg;
      selectable = props.selectable;
    }
  in
  (* Push default style to the buffer immediately. *)
  set_default_style surface props.default_style;
  Text_buffer.finalise buffer;
  Renderable.set_render node (render surface);
  Renderable.set_measure node (Some (measure surface));
  apply_selection_capability surface;
  (* Keep viewport size synchronized with the renderable's size. *)
  Renderable.set_on_size_change node
    (Some
       (fun n ->
         let w = Renderable.width n in
         let h = Renderable.height n in
         set_viewport_size surface ~width:w ~height:h));
  Renderable.request_render node;
  surface
