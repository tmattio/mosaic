type item = { name : string; description : string option }

module Props = struct
  type t = {
    options : item list;
    background : Ansi.Color.t;
    text_color : Ansi.Color.t;
    focused_background : Ansi.Color.t;
    focused_text_color : Ansi.Color.t;
    selected_background : Ansi.Color.t;
    selected_text_color : Ansi.Color.t;
    description_color : Ansi.Color.t;
    selected_description_color : Ansi.Color.t;
    show_scroll_indicator : bool;
    wrap_selection : bool;
    show_description : bool;
    item_spacing : int;
    fast_scroll_step : int;
    selected_index : int;
    autofocus : bool;
  }

  let make ?(options = []) ?background ?text_color ?focused_background
      ?focused_text_color ?(selected_background = Ansi.Color.of_rgb 51 68 85)
      ?(selected_text_color = Ansi.Color.of_rgb 255 255 0)
      ?(description_color = Ansi.Color.of_rgb 136 136 136)
      ?(selected_description_color = Ansi.Color.of_rgb 204 204 204)
      ?(show_scroll_indicator = false) ?(wrap_selection = false)
      ?(show_description = true) ?(item_spacing = 0) ?(fast_scroll_step = 5)
      ?(selected_index = 0) ?(autofocus = false) () =
    let background_opt = background in
    let background =
      match background_opt with
      | Some c -> c
      | None -> Ansi.Color.of_rgba 0 0 0 0
    in
    let text_color =
      match text_color with
      | Some c -> c
      | None -> Ansi.Color.of_rgb 255 255 255
    in
    let focused_background =
      match focused_background with
      | Some c -> c
      | None -> (
          (* fall back to non-transparent dark when not provided *)
          match background_opt with
          | Some b -> b
          | None -> Ansi.Color.of_rgb 26 26 26)
    in
    let focused_text_color =
      match focused_text_color with Some c -> c | None -> text_color
    in
    {
      options;
      background;
      text_color;
      focused_background;
      focused_text_color;
      selected_background;
      selected_text_color;
      description_color;
      selected_description_color;
      show_scroll_indicator;
      wrap_selection;
      show_description;
      item_spacing = max 0 item_spacing;
      fast_scroll_step = max 1 fast_scroll_step;
      selected_index = max 0 selected_index;
      autofocus;
    }

  let default = make ()

  let equal a b =
    let item_eq (x : item) (y : item) =
      String.equal x.name y.name
      && Option.equal String.equal x.description y.description
    in
    let rec list_eq xs ys =
      match (xs, ys) with
      | [], [] -> true
      | x :: xs, y :: ys -> item_eq x y && list_eq xs ys
      | _ -> false
    in
    list_eq a.options b.options
    && Ansi.Color.equal a.background b.background
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.focused_background b.focused_background
    && Ansi.Color.equal a.focused_text_color b.focused_text_color
    && Ansi.Color.equal a.selected_background b.selected_background
    && Ansi.Color.equal a.selected_text_color b.selected_text_color
    && Ansi.Color.equal a.description_color b.description_color
    && Ansi.Color.equal a.selected_description_color
         b.selected_description_color
    && Bool.equal a.show_scroll_indicator b.show_scroll_indicator
    && Bool.equal a.wrap_selection b.wrap_selection
    && Bool.equal a.show_description b.show_description
    && Int.equal a.item_spacing b.item_spacing
    && Int.equal a.fast_scroll_step b.fast_scroll_step
    && Int.equal a.selected_index b.selected_index
    && Bool.equal a.autofocus b.autofocus
end

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable items : item array;
  mutable selected_index : int;
  mutable scroll_offset : int;
  mutable lines_per_item : int;
  mutable max_visible_items : int;
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
}

let node t = t.node
let request t = Renderable.request_render t.node
let option_count t = Array.length t.items

let clamp_index t idx =
  let len = option_count t in
  if len = 0 then 0 else max 0 (min (len - 1) idx)

let effective_bg t focused =
  if focused then t.props.focused_background else t.props.background

let effective_fg t focused =
  if focused then t.props.focused_text_color else t.props.text_color

let recalc_lines_per_item t =
  let base = if t.props.show_description then 2 else 1 in
  t.lines_per_item <- base + t.props.item_spacing

let recalc_max_visible t height =
  let h = max 0 height in
  let lpi = max 1 t.lines_per_item in
  t.max_visible_items <- max 1 (h / lpi)

let update_scroll_offset t =
  let len = option_count t in
  if len = 0 then t.scroll_offset <- 0
  else
    let half = max 0 (t.max_visible_items / 2) in
    let max_off = max 0 (len - t.max_visible_items) in
    let desired = t.selected_index - half in
    t.scroll_offset <- max 0 (min desired max_off)

let set_selected_index_internal t idx =
  let len = option_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_index then (
      t.selected_index <- idx;
      t.props <- { t.props with selected_index = idx };
      update_scroll_offset t;
      (match t.on_change with None -> () | Some f -> f t.selected_index);
      request t)

let options t = Array.to_list t.items
let selected_index t = t.selected_index

let selected_item t =
  let len = option_count t in
  if len = 0 then None else Some t.items.(t.selected_index)

let set_options t opts =
  t.items <- Array.of_list opts;
  t.selected_index <- clamp_index t t.selected_index;
  t.props <- { t.props with options = opts; selected_index = t.selected_index };
  update_scroll_offset t;
  request t

let set_selected_index t idx = set_selected_index_internal t idx

let set_wrap_selection t flag =
  if t.props.wrap_selection <> flag then (
    t.props <- { t.props with wrap_selection = flag };
    request t)

let set_show_description t flag =
  if t.props.show_description <> flag then (
    t.props <- { t.props with show_description = flag };
    recalc_lines_per_item t;
    update_scroll_offset t;
    request t)

let set_show_scroll_indicator t flag =
  if t.props.show_scroll_indicator <> flag then (
    t.props <- { t.props with show_scroll_indicator = flag };
    request t)

let set_item_spacing t n =
  let n = max 0 n in
  if t.props.item_spacing <> n then (
    t.props <- { t.props with item_spacing = n };
    recalc_lines_per_item t;
    update_scroll_offset t;
    request t)

let set_fast_scroll_step t n =
  let n = max 1 n in
  if t.props.fast_scroll_step <> n then (
    t.props <- { t.props with fast_scroll_step = n };
    request t)

let set_background t c =
  if not (Ansi.Color.equal t.props.background c) then (
    t.props <- { t.props with background = c };
    request t)

let set_text_color t c =
  if not (Ansi.Color.equal t.props.text_color c) then (
    t.props <- { t.props with text_color = c };
    request t)

let set_focused_background t c =
  if not (Ansi.Color.equal t.props.focused_background c) then (
    t.props <- { t.props with focused_background = c };
    request t)

let set_focused_text_color t c =
  if not (Ansi.Color.equal t.props.focused_text_color c) then (
    t.props <- { t.props with focused_text_color = c };
    request t)

let set_selected_background t c =
  if not (Ansi.Color.equal t.props.selected_background c) then (
    t.props <- { t.props with selected_background = c };
    request t)

let set_selected_text_color t c =
  if not (Ansi.Color.equal t.props.selected_text_color c) then (
    t.props <- { t.props with selected_text_color = c };
    request t)

let set_description_color t c =
  if not (Ansi.Color.equal t.props.description_color c) then (
    t.props <- { t.props with description_color = c };
    request t)

let set_selected_description_color t c =
  if not (Ansi.Color.equal t.props.selected_description_color c) then (
    t.props <- { t.props with selected_description_color = c };
    request t)

let set_on_change t cb = t.on_change <- cb
let set_on_activate t cb = t.on_activate <- cb

let move_up ?(steps = 1) t =
  let len = option_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index - steps in
    if new_index >= 0 then set_selected_index_internal t new_index
    else if t.props.wrap_selection then set_selected_index_internal t (len - 1)
    else set_selected_index_internal t 0

let move_down ?(steps = 1) t =
  let len = option_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index + steps in
    if new_index < len then set_selected_index_internal t new_index
    else if t.props.wrap_selection then set_selected_index_internal t 0
    else set_selected_index_internal t (len - 1)

let handle_key t (event : Event.key) : bool =
  let kev = Event.Key.data event in
  let shift = kev.modifier.shift in
  match kev.key with
  | Up ->
      move_up ~steps:(if shift then t.props.fast_scroll_step else 1) t;
      true
  | Down ->
      move_down ~steps:(if shift then t.props.fast_scroll_step else 1) t;
      true
  | Char c when Uchar.equal c (Uchar.of_char 'k') ->
      move_up t;
      true
  | Char c when Uchar.equal c (Uchar.of_char 'j') ->
      move_down t;
      true
  | Enter | KP_enter ->
      (match t.on_activate with None -> () | Some f -> f t.selected_index);
      true
  | _ -> false

let handle_mouse t (event : Event.mouse) : unit =
  let lx = Renderable.x t.node in
  let ly = Renderable.y t.node in
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  let x = Event.Mouse.x event in
  let y = Event.Mouse.y event in
  match Event.Mouse.kind event with
  | Down -> (
      match Event.Mouse.button event with
      | Some Input.Mouse.Left ->
          if x >= lx && x < lx + lw && y >= ly && y < ly + lh then
            let local_y = y - ly in
            let index = t.scroll_offset + (local_y / max 1 t.lines_per_item) in
            if index < option_count t then (
              set_selected_index_internal t index;
              Event.Mouse.stop_propagation event)
      | _ -> ())
  | Scroll -> (
      match Event.Mouse.scroll_delta event with
      | Some (Event.Mouse.Scroll_up, delta) when delta > 0 ->
          move_up t;
          Event.Mouse.stop_propagation event
      | Some (Event.Mouse.Scroll_down, delta) when delta > 0 ->
          move_down t;
          Event.Mouse.stop_propagation event
      | _ -> ())
  | _ -> ()

let render t renderable grid ~delta:_ =
  (* Local buffer coordinates (0,0) for buffered rendering parity. *)
  let lx = 0 in
  let ly = 0 in
  let width = Renderable.width renderable in
  let height = Renderable.height renderable in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused renderable in
    let base_bg = effective_bg t focused in
    Grid.fill_rect grid ~x:lx ~y:ly ~width ~height ~color:base_bg;
    (* Recompute pagination against current height *)
    recalc_lines_per_item t;
    recalc_max_visible t height;
    update_scroll_offset t;
    let max_visible = t.max_visible_items in
    let start_index = t.scroll_offset in
    let end_index = min (option_count t) (start_index + max_visible) in
    let base_text = effective_fg t focused in
    for i = 0 to max_visible - 1 do
      let actual_index = start_index + i in
      if actual_index < end_index then
        let it = t.items.(actual_index) in
        let item_y = ly + (i * t.lines_per_item) in
        if item_y < ly + height then (
          let is_selected = actual_index = t.selected_index in
          let content_height = if t.props.show_description then 2 else 1 in
          if is_selected then
            Grid.fill_rect grid ~x:lx ~y:item_y ~width ~height:content_height
              ~color:t.props.selected_background;
          (* name line *)
          let indicator = if is_selected then "▶ " else "  " in
          let name_color =
            if is_selected then t.props.selected_text_color else base_text
          in
          let name_text = indicator ^ it.name in
          Grid.draw_text
            ~style:(Ansi.Style.make ~fg:name_color ())
            grid ~x:(lx + 1) ~y:item_y ~text:name_text;
          (* description line (optional) *)
          if t.props.show_description then
            match it.description with
            | None -> ()
            | Some desc ->
                if item_y + 1 < ly + height then
                  let desc_color =
                    if is_selected then t.props.selected_description_color
                    else t.props.description_color
                  in
                  Grid.draw_text
                    ~style:(Ansi.Style.make ~fg:desc_color ())
                    grid ~x:(lx + 3) ~y:(item_y + 1) ~text:desc)
    done;
    if t.props.show_scroll_indicator && option_count t > max_visible then
      let len = max 1 (option_count t - 1) in
      let scroll_percent = float t.selected_index /. float len in
      let indicator_height = max 1 (height - 2) in
      let indicator_y =
        ly + 1 + int_of_float (floor (scroll_percent *. float indicator_height))
      in
      let indicator_x = lx + width - 1 in
      Grid.draw_text
        ~style:(Ansi.Style.make ~fg:(Ansi.Color.of_rgb 102 102 102) ())
        grid ~x:indicator_x ~y:indicator_y ~text:"█"

let on_size_change t (_ : Renderable.t) =
  let lh = Renderable.height t.node in
  recalc_max_visible t lh;
  update_scroll_offset t;
  request t

let mount ?(props = Props.default) node =
  let items = Array.of_list props.options in
  let initial_selected =
    let len = Array.length items in
    if len = 0 then 0 else max 0 (min (len - 1) props.selected_index)
  in
  let props = { props with selected_index = initial_selected } in
  let view =
    {
      node;
      props;
      items;
      selected_index = initial_selected;
      scroll_offset = 0;
      lines_per_item = 1;
      max_visible_items = 1;
      on_change = None;
      on_activate = None;
    }
  in
  recalc_lines_per_item view;
  recalc_max_visible view (Renderable.height node);
  update_scroll_offset view;
  Renderable.set_render node (render view);
  Renderable.set_buffer node `Self;
  Renderable.set_focusable node true;
  Renderable.on_key_down node (fun evt -> ignore (handle_key view evt));
  Renderable.on_mouse node (fun ev ->
      match Event.Mouse.kind ev with
      | Down | Scroll -> handle_mouse view ev
      | _ -> ());
  Renderable.set_on_size_change node (Some (fun n -> on_size_change view n));
  (match view.props.autofocus with
  | true -> ignore (Renderable.focus node)
  | false -> ());
  request view;
  view

let apply_props t (props : Props.t) =
  (* Options and navigation *)
  set_options t props.options;
  set_selected_index t props.selected_index;
  set_wrap_selection t props.wrap_selection;
  set_show_description t props.show_description;
  set_show_scroll_indicator t props.show_scroll_indicator;
  set_item_spacing t props.item_spacing;
  set_fast_scroll_step t props.fast_scroll_step;
  t.props <- { t.props with autofocus = props.autofocus };
  (* Colors *)
  set_background t props.background;
  set_text_color t props.text_color;
  set_focused_background t props.focused_background;
  set_focused_text_color t props.focused_text_color;
  set_selected_background t props.selected_background;
  set_selected_text_color t props.selected_text_color;
  set_description_color t props.description_color;
  set_selected_description_color t props.selected_description_color
