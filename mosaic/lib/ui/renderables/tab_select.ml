type tab = { label : string; description : string option }

module Props = struct
  type t = {
    options : tab list;
    wrap_selection : bool;
    show_description : bool;
    show_underline : bool;
    show_scroll_arrows : bool;
    mouse_navigation : bool;
    autofocus : bool;
    tab_width : int;
    background : Ansi.Color.t;
    text_color : Ansi.Color.t;
    focused_background : Ansi.Color.t;
    focused_text : Ansi.Color.t;
    selected_background : Ansi.Color.t;
    selected_text : Ansi.Color.t;
    selected_description : Ansi.Color.t;
  }

  let make ?(options = []) ?(wrap_selection = false) ?(show_description = true)
      ?(show_underline = true) ?(show_scroll_arrows = true)
      ?(mouse_navigation = false) ?(autofocus = false) ?(tab_width = 20)
      ?background ?text_color ?focused_background ?focused_text
      ?(selected_background = Ansi.Color.of_rgb 51 68 85)
      ?(selected_text = Ansi.Color.of_rgb 255 255 0)
      ?(selected_description = Ansi.Color.of_rgb 204 204 204) () =
    let background_opt = background in
    let background =
      Option.value background_opt ~default:(Ansi.Color.of_rgba 0 0 0 0)
    in
    let text_color =
      Option.value text_color ~default:(Ansi.Color.of_rgb 255 255 255)
    in
    let focused_background =
      match focused_background with
      | Some c -> c
      | None -> (
          match background_opt with
          | Some b -> b
          | None -> Ansi.Color.of_rgb 26 26 26)
    in
    let focused_text = Option.value focused_text ~default:text_color in
    {
      options;
      wrap_selection;
      show_description;
      show_underline;
      show_scroll_arrows;
      mouse_navigation;
      autofocus;
      tab_width = max 1 tab_width;
      background;
      text_color;
      focused_background;
      focused_text;
      selected_background;
      selected_text;
      selected_description;
    }

  let default = make ()

  let equal a b =
    let tab_equal (x : tab) (y : tab) =
      String.equal x.label y.label
      && Option.equal String.equal x.description y.description
    in
    let rec list_eq eq xs ys =
      match (xs, ys) with
      | [], [] -> true
      | x :: xs, y :: ys -> eq x y && list_eq eq xs ys
      | _ -> false
    in
    list_eq tab_equal a.options b.options
    && Bool.equal a.wrap_selection b.wrap_selection
    && Bool.equal a.show_description b.show_description
    && Bool.equal a.show_underline b.show_underline
    && Bool.equal a.show_scroll_arrows b.show_scroll_arrows
    && Bool.equal a.mouse_navigation b.mouse_navigation
    && Bool.equal a.autofocus b.autofocus
    && Int.equal a.tab_width b.tab_width
    && Ansi.Color.equal a.background b.background
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.focused_background b.focused_background
    && Ansi.Color.equal a.focused_text b.focused_text
    && Ansi.Color.equal a.selected_background b.selected_background
    && Ansi.Color.equal a.selected_text b.selected_text
    && Ansi.Color.equal a.selected_description b.selected_description
end

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable options : tab array;
  mutable selected_index : int;
  mutable scroll_offset : int;
  mutable extra_navigation : bool;
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
  mutable on_change_full : (int * tab -> unit) option;
  mutable on_activate_full : (int * tab -> unit) option;
}

let node t = t.node
let request t = Renderable.request_render t.node
let option_count t = Array.length t.options

let clamp_index t idx =
  let len = option_count t in
  if len = 0 then 0 else max 0 (min (len - 1) idx)

let base_height t =
  let base = 1 in
  let underline = if t.props.show_underline then 1 else 0 in
  let desc = if t.props.show_description then 1 else 0 in
  base + underline + desc

let truncate_text text max_width =
  if String.length text = 0 then ""
  else if max_width <= 0 then "…"
  else if String.length text <= max_width then text
  else if max_width = 1 then "…"
  else String.sub text 0 (max_width - 1) ^ "…"

let notify_change t =
  (match t.on_change with None -> () | Some f -> f t.selected_index);
  match t.on_change_full with
  | None -> ()
  | Some f ->
      let idx = t.selected_index in
      let len = option_count t in
      if len > 0 && idx >= 0 && idx < len then f (idx, t.options.(idx))

let notify_activate t =
  (match t.on_activate with None -> () | Some f -> f t.selected_index);
  match t.on_activate_full with
  | None -> ()
  | Some f ->
      let idx = t.selected_index in
      let len = option_count t in
      if len > 0 && idx >= 0 && idx < len then f (idx, t.options.(idx))

let update_scroll_offset t visible_count =
  let len = option_count t in
  if len = 0 then t.scroll_offset <- 0
  else
    let max_visible = max 1 visible_count in
    let half = max 0 (max_visible / 2) in
    let max_offset = max 0 (len - max_visible) in
    let desired = t.selected_index - half in
    let new_offset = max 0 (min desired max_offset) in
    t.scroll_offset <- new_offset

let calculate_visible_count t ~width =
  let width = max 0 width in
  let tab_width = max 1 t.props.tab_width in
  max 1 (width / tab_width)

let visible_count_from_layout t =
  let w = Renderable.width t.node in
  calculate_visible_count t ~width:w

let set_selected_index_internal t idx visible_count =
  let len = option_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_index then (
      t.selected_index <- idx;
      update_scroll_offset t visible_count;
      notify_change t;
      request t)

let set_selected_index t idx =
  let visible = visible_count_from_layout t in
  set_selected_index_internal t idx visible

let move_left t =
  let len = option_count t in
  if len = 0 then ()
  else if t.selected_index > 0 then
    set_selected_index_internal t (t.selected_index - 1)
      (visible_count_from_layout t)
  else if t.props.wrap_selection then
    set_selected_index_internal t (len - 1) (visible_count_from_layout t)

let move_right t =
  let len = option_count t in
  if len = 0 then ()
  else if t.selected_index < len - 1 then
    set_selected_index_internal t (t.selected_index + 1)
      (visible_count_from_layout t)
  else if t.props.wrap_selection then
    set_selected_index_internal t 0 (visible_count_from_layout t)

let measure t ~known_dimensions ~available_space ~style:_ =
  (* Content-based default: show all tabs if possible *)
  let content_width =
    Float.of_int (option_count t * max 1 t.props.tab_width)
  in
  let width =
    match known_dimensions.Toffee.Geometry.Size.width with
    | Some w when w > 0. -> w
    | _ -> (
        match Toffee.Available_space.to_option available_space.Toffee.Geometry.Size.width with
        | Some w when w > 0. -> w
        | _ -> content_width)
  in
  let height = Float.of_int (base_height t) in
  Toffee.Geometry.Size.{ width; height }

let effective_background t focused =
  if focused then t.props.focused_background else t.props.background

let effective_text_color t focused =
  if focused then t.props.focused_text else t.props.text_color

let render t renderable grid ~delta:_ =
  (* Draw into the local buffer coordinate space (0,0). Buffered nodes are
     rendered to an offscreen grid which is later blitted at the node's absolute
     position by the renderer. *)
  let lx = 0 in
  let ly = 0 in
  let width = Renderable.width renderable in
  let height = Renderable.height renderable in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused renderable in
    let base_bg = effective_background t focused in
    Grid.fill_rect grid ~x:lx ~y:ly ~width ~height ~color:base_bg;
    let visible_count = calculate_visible_count t ~width in
    update_scroll_offset t visible_count;
    let max_visible = visible_count in
    let end_index = min (option_count t) (t.scroll_offset + max_visible) in
    let base_text = effective_text_color t focused in
    let tab_width = max 1 t.props.tab_width in
    for i = 0 to max_visible - 1 do
      let actual_index = t.scroll_offset + i in
      if actual_index < end_index then
        let tab = t.options.(actual_index) in
        let tab_x = lx + (i * tab_width) in
        if tab_x < lx + width then
          let available = min tab_width (width - (i * tab_width)) in
          if available > 0 then (
            let is_selected = actual_index = t.selected_index in
            if is_selected then
              Grid.fill_rect grid ~x:tab_x ~y:ly ~width:available ~height:1
                ~color:t.props.selected_background;
            let left_padding = if available > 0 then 1 else 0 in
            let right_padding = if available - left_padding > 0 then 1 else 0 in
            let label_width = available - left_padding - right_padding in
            let label = truncate_text tab.label label_width in
            let label_color =
              if is_selected then t.props.selected_text else base_text
            in
            if label <> "" then
              Grid.draw_text
                ~style:(Ansi.Style.make ~fg:label_color ())
                grid ~x:(tab_x + left_padding) ~y:ly ~text:label;
            if is_selected && t.props.show_underline && height >= 2 then
              let underline_style =
                Ansi.Style.make ~fg:label_color ~bg:t.props.selected_background
                  ()
              in
              let width = available in
              let underline =
                let buf = Buffer.create (width * 3) in
                for _ = 1 to width do
                  Buffer.add_string buf "▬"
                done;
                Buffer.contents buf
              in
              Grid.draw_text ~style:underline_style grid ~x:tab_x ~y:(ly + 1)
                ~text:underline)
    done;
    (if
       t.props.show_description
       && height >= if t.props.show_underline then 3 else 2
     then
       if option_count t > 0 then
         match t.options.(t.selected_index).description with
         | None -> ()
         | Some desc ->
             let row = ly + if t.props.show_underline then 2 else 1 in
             let left_padding = if width > 0 then 1 else 0 in
             let right_padding = if width - left_padding > 0 then 1 else 0 in
             let available = width - left_padding - right_padding in
             let text = truncate_text desc available in
             if text <> "" then
               Grid.draw_text
                 ~style:(Ansi.Style.make ~fg:t.props.selected_description ())
                 grid ~x:(lx + left_padding) ~y:row ~text);
    if t.props.show_scroll_arrows && option_count t > max_visible then (
      if t.scroll_offset > 0 then
        Grid.draw_text
          ~style:(Ansi.Style.make ~fg:(Ansi.Color.of_rgb 170 170 170) ())
          grid ~x:lx ~y:ly ~text:"‹";
      if t.scroll_offset + max_visible < option_count t then
        Grid.draw_text
          ~style:(Ansi.Style.make ~fg:(Ansi.Color.of_rgb 170 170 170) ())
          grid
          ~x:(lx + width - 1)
          ~y:ly ~text:"›")

let request_layout_update t = ignore (Renderable.mark_layout_dirty t.node)
let options t = Array.to_list t.options
let selected_index t = t.selected_index

let option_at t idx =
  if idx < 0 || idx >= Array.length t.options then None
  else Some t.options.(idx)

let selected_option t = option_at t t.selected_index

let set_options t options =
  let arr = Array.of_list options in
  t.options <- arr;
  t.selected_index <- clamp_index t t.selected_index;
  update_scroll_offset t (visible_count_from_layout t);
  request t

let set_wrap_selection t flag =
  if t.props.wrap_selection <> flag then (
    t.props <- { t.props with wrap_selection = flag };
    request t)

let set_tab_width t width =
  if width > 0 && width <> t.props.tab_width then (
    t.props <- { t.props with tab_width = width };
    update_scroll_offset t (visible_count_from_layout t);
    request t)

let set_show_description t show =
  if t.props.show_description <> show then (
    t.props <- { t.props with show_description = show };
    request_layout_update t;
    request t)

let set_show_underline t show =
  if t.props.show_underline <> show then (
    t.props <- { t.props with show_underline = show };
    request_layout_update t;
    request t)

let set_show_scroll_arrows t show =
  if t.props.show_scroll_arrows <> show then (
    t.props <- { t.props with show_scroll_arrows = show };
    request t)

let set_extra_navigation t flag = t.extra_navigation <- flag

let set_background t color =
  if not (Ansi.Color.equal t.props.background color) then (
    t.props <- { t.props with background = color };
    request t)

let set_text_color t color =
  if not (Ansi.Color.equal t.props.text_color color) then (
    t.props <- { t.props with text_color = color };
    request t)

let set_focused_background t color =
  if not (Ansi.Color.equal t.props.focused_background color) then (
    t.props <- { t.props with focused_background = color };
    request t)

let set_focused_text t color =
  if not (Ansi.Color.equal t.props.focused_text color) then (
    t.props <- { t.props with focused_text = color };
    request t)

let set_selected_background t color =
  if not (Ansi.Color.equal t.props.selected_background color) then (
    t.props <- { t.props with selected_background = color };
    request t)

let set_selected_text t color =
  if not (Ansi.Color.equal t.props.selected_text color) then (
    t.props <- { t.props with selected_text = color };
    request t)

let set_selected_description_color t color =
  if not (Ansi.Color.equal t.props.selected_description color) then (
    t.props <- { t.props with selected_description = color };
    request t)

let set_on_change t cb = t.on_change <- cb
let set_on_activate t cb = t.on_activate <- cb
let set_on_change_full t cb = t.on_change_full <- cb
let set_on_activate_full t cb = t.on_activate_full <- cb

let handle_key t event =
  let w = Renderable.width t.node in
  let visible_count = calculate_visible_count t ~width:w in
  let kev = Event.Key.data event in
  match kev.key with
  | Left ->
      move_left t;
      true
  | Right ->
      move_right t;
      true
  | Char c when Uchar.equal c (Uchar.of_char '[') ->
      move_left t;
      true
  | Char c when Uchar.equal c (Uchar.of_char ']') ->
      move_right t;
      true
  | Home when t.extra_navigation ->
      set_selected_index_internal t 0 visible_count;
      true
  | End when t.extra_navigation ->
      let len = option_count t in
      if len > 0 then (
        set_selected_index_internal t (len - 1) visible_count;
        true)
      else true
  | Enter | KP_enter ->
      notify_activate t;
      true
  | _ -> false

let handle_mouse t (event : Event.mouse) =
  let lx = Renderable.x t.node in
  (* ly not used here; remove to avoid warnings *)
  let lw = Renderable.width t.node in
  let visible_count = calculate_visible_count t ~width:lw in
  if not t.extra_navigation then ()
  else
    match Event.Mouse.kind event with
    | Down -> (
        match Event.Mouse.button event with
        | Some Input.Mouse.Left ->
            let x = Event.Mouse.x event in
            if x >= lx && x < lx + lw then
              let relative = x - lx in
              let tab_width = max 1 t.props.tab_width in
              let index = t.scroll_offset + (relative / tab_width) in
              if index < option_count t then (
                set_selected_index_internal t index visible_count;
                Event.Mouse.stop_propagation event)
        | _ -> ())
    | Scroll -> (
        match Event.Mouse.scroll_delta event with
        | Some (direction, delta) ->
            if
              (direction = Event.Mouse.Scroll_left
              || direction = Event.Mouse.Scroll_up)
              && delta > 0
            then (
              ignore (move_left t);
              Event.Mouse.stop_propagation event)
            else if
              (direction = Event.Mouse.Scroll_right
              || direction = Event.Mouse.Scroll_down)
              && delta > 0
            then (
              ignore (move_right t);
              Event.Mouse.stop_propagation event)
        | None -> ())
    | _ -> ()

let mount ?(props = Props.default) node =
  let tabs =
    {
      node;
      props;
      options = Array.of_list props.options;
      selected_index = 0;
      scroll_offset = 0;
      extra_navigation = props.mouse_navigation;
      on_change = None;
      on_activate = None;
      on_change_full = None;
      on_activate_full = None;
    }
  in
  tabs.selected_index <- clamp_index tabs 0;
  update_scroll_offset tabs (visible_count_from_layout tabs);
  Renderable.set_render node (render tabs);
  Renderable.set_measure node (Some (measure tabs));
  Renderable.set_buffer node `Self;
  Renderable.set_focusable node true;
  Renderable.on_mouse node (fun ev ->
      match Event.Mouse.kind ev with
      | Down | Scroll -> handle_mouse tabs ev
      | _ -> ());
  Renderable.on_key_down node (fun evt -> ignore (handle_key tabs evt));
  (match tabs.props.autofocus with
  | true -> ignore (Renderable.focus node)
  | false -> ());
  request tabs;
  tabs

let apply_props t (props : Props.t) =
  (* Options and navigation flags *)
  if not (Props.equal t.props props) then (
    t.props <- props;
    t.options <- Array.of_list props.options;
    t.extra_navigation <- props.mouse_navigation;
    (* Selection and layout-related behaviour depends on wrap/description/underline. *)
    update_scroll_offset t (visible_count_from_layout t);
    request t)
