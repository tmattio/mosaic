(** Computes size using styles and measure functions *)

open Geometry
open Tree

(** Compute the size of a leaf node (node with no children) *)
let compute_leaf_layout ~(inputs : Layout_input.t) ~(style : Style.t)
    ~(resolve_calc_value : Style.calc_resolver)
    ~(measure_function :
       float option size -> Available_space.t size -> float size) :
    Layout_output.t =
  (* Unpack inputs *)
  let known_dimensions = Layout_input.known_dimensions inputs in
  let parent_size = Layout_input.parent_size inputs in
  let available_space = Layout_input.available_space inputs in
  let sizing_mode = Layout_input.sizing_mode inputs in
  let run_mode = Layout_input.run_mode inputs in

  (* Note: both horizontal and vertical percentage padding/borders are resolved against 
     the container's inline size (i.e. width). This is not a bug, but is how CSS is 
     specified (see: https://developer.mozilla.org/en-US/docs/Web/CSS/padding#values) *)
  let parent_inline_size = parent_size.width in

  let margin =
    Style.margin style
    |> Rect.map (fun lpa ->
           Style.Length_percentage_auto.resolve_or_zero lpa parent_inline_size
             resolve_calc_value)
  in
  let padding =
    Style.padding style
    |> Rect.map (fun lp ->
           Style.Length_percentage.resolve_or_zero lp parent_inline_size
             resolve_calc_value)
  in
  let border =
    Style.border style
    |> Rect.map (fun lp ->
           Style.Length_percentage.resolve_or_zero lp parent_inline_size
             resolve_calc_value)
  in
  let padding_border = Rect.add padding border in
  let pb_sum = Rect.sum_axes padding_border in
  let box_sizing_adjustment =
    if Style.box_sizing style = Style.Box_sizing.Content_box then pb_sum
    else Size.zero
  in

  (* Resolve node's preferred/min/max sizes (width/heights) against the available space
     (percentages resolve to pixel values). For ContentSize mode, we pretend that the 
     node has no size styles as these should be ignored. *)
  let node_size, node_min_size, node_max_size, aspect_ratio =
    match sizing_mode with
    | Sizing_mode.Content_size -> (known_dimensions, Size.none, Size.none, None)
    | Sizing_mode.Inherent_size ->
        let aspect_ratio = Style.aspect_ratio style in

        let style_size =
          Style.size style |> fun dims ->
          Size.
            {
              width =
                Style.Dimension.maybe_resolve dims.width parent_size.width
                  resolve_calc_value;
              height =
                Style.Dimension.maybe_resolve dims.height parent_size.height
                  resolve_calc_value;
            }
          |> fun size ->
          Size.apply_aspect_ratio size aspect_ratio |> fun size ->
          Size.maybe_add size box_sizing_adjustment
        in

        let style_min_size =
          Style.min_size style |> fun dims ->
          Size.
            {
              width =
                Style.Dimension.maybe_resolve dims.width parent_size.width
                  resolve_calc_value;
              height =
                Style.Dimension.maybe_resolve dims.height parent_size.height
                  resolve_calc_value;
            }
          |> fun size ->
          Size.apply_aspect_ratio size aspect_ratio |> fun size ->
          Size.maybe_add size box_sizing_adjustment
        in

        let style_max_size =
          Style.max_size style |> fun dims ->
          Size.
            {
              width =
                Style.Dimension.maybe_resolve dims.width parent_size.width
                  resolve_calc_value;
              height =
                Style.Dimension.maybe_resolve dims.height parent_size.height
                  resolve_calc_value;
            }
          |> fun size -> Size.maybe_add size box_sizing_adjustment
        in

        let node_size = Size.choose_first known_dimensions style_size in
        (node_size, style_min_size, style_max_size, aspect_ratio)
  in

  (* Scrollbar gutters are reserved when the `overflow` property is set to `Overflow::Scroll`.
     However, the axis are switched (transposed) because a node that scrolls vertically needs
     *horizontal* space to be reserved for a scrollbar *)
  let scrollbar_gutter =
    let overflow = Style.overflow style in
    Point.{ x = overflow.y; y = overflow.x }
    |> Point.map (function
         | Style.Overflow.Scroll -> Style.scrollbar_width style
         | _ -> 0.0)
  in
  (* TODO: make side configurable based on the `direction` property *)
  let content_box_inset =
    let open Rect in
    {
      padding_border with
      right = padding_border.right +. scrollbar_gutter.x;
      bottom = padding_border.bottom +. scrollbar_gutter.y;
    }
  in

  let has_styles_preventing_being_collapsed_through =
    (match Style.display style with Style.Display.Block -> false | _ -> true)
    || Style.Overflow.is_container (Style.overflow style).x
    || Style.Overflow.is_container (Style.overflow style).y
    || Style.position style = Style.Position.Absolute
    || padding.top > 0.0 || padding.bottom > 0.0 || border.top > 0.0
    || border.bottom > 0.0
    || (match node_size.height with Some h when h > 0.0 -> true | _ -> false)
    ||
    match node_min_size.height with Some h when h > 0.0 -> true | _ -> false
  in

  (* Return early if both width and height are known *)
  match
    (run_mode, has_styles_preventing_being_collapsed_through, node_size)
  with
  | Run_mode.Compute_size, true, { width = Some width; height = Some height } ->
      let size =
        Size.{ width; height } |> fun s ->
        Size.clamp s node_min_size node_max_size |> fun s -> Size.max s pb_sum
      in
      Layout_output.make ~size ~content_size:Size.zero
        ~first_baselines:Point.none ~top_margin:Collapsible_margin_set.zero
        ~bottom_margin:Collapsible_margin_set.zero
        ~margins_can_collapse_through:false
  | _ ->
      (* Normal path - compute available space and measure *)

      (* Compute available space *)
      let available_space_computed =
        let compute_axis known avail_space_axis margin_sum node_size_axis
            node_min_size_axis node_max_size_axis inset_sum =
          let open Available_space in
          (* Start with known dimensions or available space *)
          (match known with Some v -> of_float v | None -> avail_space_axis)
          (* Subtract margin *)
          |> fun space ->
          sub_or_zero space (Some margin_sum)
          (* Override with known dimensions if present *)
          |> fun space ->
          set_or_self space known
          (* Override with node size if present *)
          |> fun space ->
          set_or_self space node_size_axis
          (* Apply max constraint even for Max_content *)
          |> fun space ->
          min_or_self space node_max_size_axis
          (* Apply constraints and subtract inset *)
          |> fun space ->
          map_definite_value space (fun size ->
              (* Clamp to min/max constraints *)
              let clamped =
                match (node_min_size_axis, node_max_size_axis) with
                | Some min, Some max -> Float.min max (Float.max min size)
                | Some min, None -> Float.max min size
                | None, Some max -> Float.min max size
                | None, None -> size
              in
              (* Subtract content box inset *)
              clamped -. inset_sum)
        in

        let width =
          compute_axis known_dimensions.width available_space.width
            (Rect.horizontal_axis_sum margin)
            node_size.width node_min_size.width node_max_size.width
            (Rect.horizontal_axis_sum content_box_inset)
        in
        let height =
          compute_axis known_dimensions.height available_space.height
            (Rect.vertical_axis_sum margin)
            node_size.height node_min_size.height node_max_size.height
            (Rect.vertical_axis_sum content_box_inset)
        in
        Size.{ width; height }
      in

      (* Measure node *)
      let measured_size =
        let known_for_measure =
          match run_mode with
          | Run_mode.Compute_size -> known_dimensions
          | Run_mode.Perform_layout -> Size.none
          | Run_mode.Perform_hidden_layout ->
              invalid_arg "Leaf node cannot be measured in hidden layout mode"
        in
        measure_function known_for_measure available_space_computed
      in

      (* Match Taffy's leaf computation logic exactly *)
      let clamped_size =
        let chosen = Size.choose_first known_dimensions node_size in
        let measured_with_inset =
          Size.add measured_size (Rect.sum_axes content_box_inset)
        in

        (* Taffy's logic: known_dimensions.or(node_size).unwrap_or(measured_size + content_box_inset.sum_axes()) *)
        let base_size =
          match (chosen.width, chosen.height) with
          | Some w, Some h -> Size.{ width = w; height = h }
          | Some w, None ->
              Size.{ width = w; height = measured_with_inset.height }
          | None, Some h ->
              Size.{ width = measured_with_inset.width; height = h }
          | None, None -> measured_with_inset
        in

        (* Clamp to min/max constraints *)
        Size.clamp base_size node_min_size node_max_size
      in

      (* Apply aspect ratio AFTER clamping, matching Taffy's logic exactly *)
      let size =
        match aspect_ratio with
        | Some ratio ->
            (* Taffy: height: f32_max(clamped_size.height, aspect_ratio.map(|ratio| clamped_size.width / ratio).unwrap_or(0.0)) *)
            let aspect_height = clamped_size.width /. ratio in
            Size.
              {
                width = clamped_size.width;
                height = Float.max clamped_size.height aspect_height;
              }
        | None -> clamped_size
      in

      let size = Size.max size pb_sum in

      Layout_output.make ~size
        ~content_size:(Size.add measured_size (Rect.sum_axes padding))
        ~first_baselines:Point.none ~top_margin:Collapsible_margin_set.zero
        ~bottom_margin:Collapsible_margin_set.zero
        ~margins_can_collapse_through:
          ((not has_styles_preventing_being_collapsed_through)
          && size.height = 0.0 && measured_size.height = 0.0)
