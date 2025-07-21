open Element

type t = Element.t
type element = t
type padding = Padding.t
type border = Border.t
type border_style = Border.style = Solid | Rounded | Double | Thick | ASCII
type align = Element.align
type size_def = Element.size_def

let padding = Padding.make
let padding_all = Padding.all
let padding_xy = Padding.xy
let pad = Padding.pad
let border = Border.make
let normal_border = Border.normal
let ascii_border = Border.ascii
let rounded_border = Border.rounded
let double_border = Border.double
let thick_border = Border.thick
let text = text
let hbox = hbox
let vbox = vbox
let spacer = spacer
let rich_text = rich_text
let zstack = zstack
let flow = flow
let grid = grid
let scroll = scroll
let flex_spacer = flex_spacer
let divider = divider
let center = center
let styled = styled

(* The ephemeral cache is created for each top-level render call.
   The key is a combination of the element's physical address and the
   layout bounds, ensuring that we cache layouts correctly for different
   available sizes. *)
module Cache_key = struct
  type t = Element.t * int * int

  let equal (e1, w1, h1) (e2, w2, h2) = e1 == e2 && w1 = w2 && h1 = h2

  let hash (e, w, h) =
    Hashtbl.hash
      (Element.pp Format.str_formatter e;
       (Format.flush_str_formatter (), w, h))
end

module Layout_cache = Hashtbl.Make (Cache_key)

let render buffer top_level_element =
  let cache = Layout_cache.create 128 in

  let rec render_layout ?(clip = None) (layout : Layout.t) =
    let element = Layout.element layout in
    let rect = Layout.geometry layout in
    let x, y, width, height = rect in
    let pos = (x, y) in
    let bounds = (width, height) in

    (* Define a clip for the current element's children. This prevents
       children from drawing outside their parent's bounds. *)
    let child_clip =
      let new_clip_rect = Render.Clip.make x y width height in
      Render.Clip.intersect_opt clip (Some new_clip_rect)
    in

    (* 1. Draw the element's own primitive features (background, border, text). *)
    (match element with
    | Box b -> (
        let options = Box.options b in
        (match options.background with
        | Some style -> Graphics.fill_rect ~clip ~buffer ~rect ~style ()
        | None -> ());
        match options.border with
        | Some border -> Graphics.draw_border ~clip ~buffer ~rect ~border ()
        | None -> ())
    | Text t ->
        let content = Text.content t in
        let style = Text.style t in
        let align = Text.alignment t in
        let tab_width = Text.tab_width t in
        let wrap = Text.is_wrapping t in
        Graphics.draw_text ~clip ~buffer ~pos ~bounds ~text:content ~style
          ~align ~tab_width ~wrap ()
    | Rich_text rt ->
        let segments = Rich_text.segments rt in
        Graphics.draw_rich_text ~clip ~buffer ~pos ~width ~segments ()
    | Flow _ | Z_stack _ | Grid _ ->
        (* Flow, Z_stack, and Grid need background fill for proper rendering *)
        (* Fill the entire element bounds with spaces to clear the area *)
        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let cell_x = x + fst pos in
            let cell_y = y + snd pos in
            Render.set_char ?clip buffer cell_x cell_y (Uchar.of_char ' ')
              Render.Style.empty
          done
        done
    | _ ->
        (* Other elements like Spacer, Grid, Scroll have no primitive visuals themselves. *)
        ());

    (* 2. Recursively render the layouts of all children. *)
    let children_layouts = Layout.children layout in
    (* For Box elements, compute content clip adjusting for faked borders *)
    let final_child_clip = 
      match element with
      | Box b ->
          let options = Box.options b in
          let border_left_space = match options.border with | Some brd -> if Border.left brd then 1 else 0 | None -> 0 in
          let border_right_space = match options.border with | Some brd -> if Border.right brd then 1 else 0 | None -> 0 in
          let border_top_space = match options.border with | Some brd -> if Border.top brd then 1 else 0 | None -> 0 in
          let border_bottom_space = match options.border with | Some brd -> if Border.bottom brd then 1 else 0 | None -> 0 in
          let content_x = x + border_left_space + Padding.left options.padding in
          let content_y = y + border_top_space + Padding.top options.padding in
          let content_w = max 0 (width - border_left_space - border_right_space - Padding.left options.padding - Padding.right options.padding) in
          let content_h = max 0 (height - border_top_space - border_bottom_space - Padding.top options.padding - Padding.bottom options.padding) in
          
          (* Compute faked flags based on current clip (or full rect if no clip) *)
          let clip_x, clip_y, clip_w, clip_h =
            match clip with
            | Some c -> (Render.Clip.x c, Render.Clip.y c, Render.Clip.width c, Render.Clip.height c)
            | None -> (x, y, width, height)
          in
          let clip_right = clip_x + clip_w - 1 in
          let clip_bottom = clip_y + clip_h - 1 in
          let fake_left = (content_x < clip_x) && (match options.border with | Some brd -> Border.left brd | _ -> false) in
          let fake_right = (content_x + content_w - 1 > clip_right) && (match options.border with | Some brd -> Border.right brd | _ -> false) in
          let fake_top = (content_y < clip_y) && (match options.border with | Some brd -> Border.top brd | _ -> false) in
          let fake_bottom = (content_y + content_h - 1 > clip_bottom) && (match options.border with | Some brd -> Border.bottom brd | _ -> false) in
          
          (* Adjust effective content for faked borders *)
          let eff_content_x = if fake_left then max content_x (clip_x + 1) else content_x in
          let eff_content_y = if fake_top then max content_y (clip_y + 1) else content_y in
          let eff_content_right = if fake_right then min (content_x + content_w - 1) (clip_right - 1) else content_x + content_w - 1 in
          let eff_content_bottom = if fake_bottom then min (content_y + content_h - 1) (clip_bottom - 1) else content_y + content_h - 1 in
          let eff_content_w = max 0 (eff_content_right - eff_content_x + 1) in
          let eff_content_h = max 0 (eff_content_bottom - eff_content_y + 1) in
          
          Render.Clip.intersect_opt clip (Some (Render.Clip.make eff_content_x eff_content_y eff_content_w eff_content_h))
      | _ -> child_clip
    in
    List.iter (render_layout ~clip:final_child_clip) children_layouts
  in

  (* This recursive function orchestrates layout and caching. *)
  let rec calculate_and_get_layout (bounds : Layout.Bounds.t) (elem : t) :
      Layout.t =
    let cache_key =
      (elem, Layout.Bounds.width bounds, Layout.Bounds.height bounds)
    in
    match Layout_cache.find_opt cache cache_key with
    | Some cached_layout -> cached_layout (* Cache hit! *)
    | None ->
        (* Cache miss: perform the calculation. *)
        let children =
          match elem with
          | Box b -> Box.children b
          | Z_stack z -> Z_stack.children z
          | Flow f -> Flow.children f
          | Grid g -> Grid.children g
          | Scroll s -> [ Scroll.child s ]
          | _ -> []
        in
        (* Recursively ensure children are calculated before the parent.
           This populates the cache from the bottom up. *)
        List.iter
          (fun _ -> ())
          (List.map
             (fun child -> calculate_and_get_layout bounds child)
             children);

        let result = Layout.calculate bounds elem in
        Layout_cache.add cache cache_key result;
        result
  in

  (* 1. Create the initial layout bounds from the screen dimensions. *)
  let width, height = Render.dimensions buffer in
  let initial_bounds = Layout.Bounds.make ~x:0 ~y:0 ~width ~height in

  (* 2. Calculate the entire layout tree first, populating the cache. *)
  let computed_layout_tree =
    calculate_and_get_layout initial_bounds top_level_element
  in

  (* 3. Now, render the computed tree. *)
  render_layout computed_layout_tree

let pp_element = Element.pp
let measure = Element.measure
