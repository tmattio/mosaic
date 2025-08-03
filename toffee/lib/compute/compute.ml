open Geometry
open Style
open Tree_intf
open Layout

(* Re-export compute functions from submodules *)
let compute_block_layout = Block_layout.compute_block_layout
let compute_flexbox_layout = Flexbox_layout.compute_flexbox_layout
let compute_grid_layout = Grid_layout.compute_grid_layout
let compute_leaf_layout = Leaf_layout.compute_leaf_layout

let compute_root_layout (type tree)
    (module Tree : LayoutPartialTree
      with type t = tree
       and type core_container_style = Style.style) (tree : tree)
    (root : Node.Node_id.t) (available_space : Style.Available_space.t size) :
    unit =
  let known_dimensions = ref Size.none in

  let parent_size =
    Size.map available_space ~f:Style.Available_space.into_option
  in
  let style = Tree.get_core_container_style tree root in

  (* Check if this is a block layout *)
  if Style.is_block style then (
    let aspect_ratio = Style.aspect_ratio style in
    let margin =
      Resolve.resolve_or_zero_rect_with_option
        Resolve.resolve_or_zero_length_percentage_auto (Style.margin style)
        parent_size.width (fun calc basis ->
          Tree.resolve_calc_value tree ~ptr:calc ~basis)
    in
    let padding =
      Resolve.resolve_or_zero_rect_with_option
        Resolve.resolve_or_zero_length_percentage (Style.padding style)
        parent_size.width (fun calc basis ->
          Tree.resolve_calc_value tree ~ptr:calc ~basis)
    in
    let border =
      Resolve.resolve_or_zero_rect_with_option
        Resolve.resolve_or_zero_length_percentage (Style.border style)
        parent_size.width (fun calc basis ->
          Tree.resolve_calc_value tree ~ptr:calc ~basis)
    in
    let padding_border_size = Rect.sum_axes (Rect.add padding border) in
    let box_sizing_adjustment =
      if Style.box_sizing style = Content_box then padding_border_size
      else Size.zero
    in

    let min_size =
      Style.min_size style
      |> (fun s ->
      Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension s parent_size
        (fun calc basis -> Tree.resolve_calc_value tree ~ptr:calc ~basis))
      |> Resolve.maybe_apply_aspect_ratio aspect_ratio
      |> fun s -> Size.maybe_add s box_sizing_adjustment
    in
    let max_size =
      Style.max_size style
      |> (fun s ->
      Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension s parent_size
        (fun calc basis -> Tree.resolve_calc_value tree ~ptr:calc ~basis))
      |> Resolve.maybe_apply_aspect_ratio aspect_ratio
      |> fun s -> Size.maybe_add s box_sizing_adjustment
    in
    let clamped_style_size =
      ( Style.size style
      |> (fun s ->
      Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension s parent_size
        (fun calc basis -> Tree.resolve_calc_value tree ~ptr:calc ~basis))
      |> Resolve.maybe_apply_aspect_ratio aspect_ratio
      |> fun s -> Size.maybe_add s box_sizing_adjustment )
      |> fun s -> Size.maybe_clamp s min_size max_size
    in

    let min_max_definite_size =
      Size.map2 min_size max_size ~f:(fun min max ->
          match (min, max) with
          | Some min, Some max when max <= min -> Some min
          | _ -> None)
    in

    let available_space_based_size =
      {
        width =
          (match available_space.width with
          | Style.Available_space.Definite w ->
              Some (w -. Rect.horizontal_axis_sum margin)
          | _ -> None);
        height = None;
      }
    in

    let styled_based_known_dimensions =
      ( ( ( !known_dimensions |> fun kd ->
            match min_max_definite_size with
            | { width = Some _; _ } | { height = Some _; _ } ->
                Size.or_ kd min_max_definite_size
            | _ -> kd )
        |> fun kd ->
          match clamped_style_size with
          | { width = Some _; _ } | { height = Some _; _ } ->
              Size.or_ kd clamped_style_size
          | _ -> kd )
      |> fun kd ->
        match available_space_based_size with
        | { width = Some _; _ } | { height = Some _; _ } ->
            Size.or_ kd available_space_based_size
        | _ -> kd )
      |> fun s ->
      Size.maybe_max s (Size.map padding_border_size ~f:(fun v -> Some v))
    in

    known_dimensions := styled_based_known_dimensions;

    (* Recursively compute node layout *)
    let module TreeExt = LayoutPartialTreeExt (Tree) in
    let output =
      TreeExt.perform_child_layout tree root ~known_dimensions:!known_dimensions
        ~parent_size ~available_space ~sizing_mode:Sizing_mode.Inherent_size
        ~vertical_margins_are_collapsible:line_false
    in

    let style = Tree.get_core_container_style tree root in
    let padding =
      Resolve.resolve_or_zero_rect_with_option
        Resolve.resolve_or_zero_length_percentage (Style.padding style)
        (Style.Available_space.into_option available_space.width)
        (fun calc basis -> Tree.resolve_calc_value tree ~ptr:calc ~basis)
    in
    let border =
      Resolve.resolve_or_zero_rect_with_option
        Resolve.resolve_or_zero_length_percentage (Style.border style)
        (Style.Available_space.into_option available_space.width)
        (fun calc basis -> Tree.resolve_calc_value tree ~ptr:calc ~basis)
    in
    let margin =
      Resolve.resolve_or_zero_rect_with_option
        Resolve.resolve_or_zero_length_percentage_auto (Style.margin style)
        (Style.Available_space.into_option available_space.width)
        (fun calc basis -> Tree.resolve_calc_value tree ~ptr:calc ~basis)
    in
    let scrollbar_size =
      {
        width =
          (if (Style.overflow style).y = Scroll then Style.scrollbar_width style
           else 0.0);
        height =
          (if (Style.overflow style).x = Scroll then Style.scrollbar_width style
           else 0.0);
      }
    in

    Tree.set_unrounded_layout tree root
      {
        Layout.order = 0;
        location = point_zero;
        size = output.size;
        content_size = output.size;
        (* Using size as content_size for now *)
        scrollbar_size;
        padding;
        border;
        margin;
      })

let compute_cached_layout (type tree)
    (module Tree : CacheTree with type t = tree) (tree : tree)
    (node : Node.Node_id.t) (inputs : Layout_input.t)
    (compute_uncached :
      tree -> Node.Node_id.t -> Layout_input.t -> Layout_output.t) :
    Layout_output.t =
  let open Layout_input in
  let { known_dimensions; available_space; run_mode; _ } = inputs in

  (* First check if we have a cached result for the given input *)
  let cache_entry =
    Tree.cache_get tree node known_dimensions available_space run_mode
  in
  match cache_entry with
  | Some cached_size_and_baselines -> cached_size_and_baselines
  | None ->
      let computed_size_and_baselines = compute_uncached tree node inputs in
      (* Cache result *)
      Tree.cache_store tree node known_dimensions available_space run_mode
        computed_size_and_baselines;
      computed_size_and_baselines

let compute_hidden_layout (type tree)
    (module Tree : LayoutPartialTree
      with type t = tree
       and type core_container_style = Style.style)
    (module CacheTree : CacheTree with type t = tree) (tree : tree)
    (node : Node.Node_id.t) : Layout_output.t =
  (* Clear cache and set zeroed-out layout for the node *)
  CacheTree.cache_clear tree node;
  Tree.set_unrounded_layout tree node (Layout.with_order 0);

  (* Perform hidden layout on all children *)
  let child_count = Tree.child_count tree node in
  for index = 0 to child_count - 1 do
    let child_id = Tree.get_child_id tree node index in
    let _ = Tree.compute_child_layout tree child_id Layout_input.hidden in
    ()
  done;

  Layout_output.hidden

let round_layout (type tree) (module Tree : RoundTree with type t = tree)
    (tree : tree) (node_id : Node.Node_id.t) : unit =
  let rec round_layout_inner tree node_id cumulative_x cumulative_y =
    let unrounded_layout = Tree.get_unrounded_layout tree node_id in
    let layout = ref unrounded_layout in

    let cumulative_x = cumulative_x +. unrounded_layout.Layout.location.x in
    let cumulative_y = cumulative_y +. unrounded_layout.Layout.location.y in

    let round = Float.round in

    layout :=
      {
        !layout with
        Layout.location =
          {
            x = round unrounded_layout.Layout.location.x;
            y = round unrounded_layout.Layout.location.y;
          };
        size =
          {
            width =
              round (cumulative_x +. unrounded_layout.Layout.size.width)
              -. round cumulative_x;
            height =
              round (cumulative_y +. unrounded_layout.Layout.size.height)
              -. round cumulative_y;
          };
        scrollbar_size =
          {
            width = round unrounded_layout.Layout.scrollbar_size.width;
            height = round unrounded_layout.Layout.scrollbar_size.height;
          };
        border =
          {
            left =
              round (cumulative_x +. unrounded_layout.Layout.border.left)
              -. round cumulative_x;
            right =
              round (cumulative_x +. unrounded_layout.Layout.size.width)
              -. round
                   (cumulative_x +. unrounded_layout.Layout.size.width
                  -. unrounded_layout.Layout.border.right);
            top =
              round (cumulative_y +. unrounded_layout.Layout.border.top)
              -. round cumulative_y;
            bottom =
              round (cumulative_y +. unrounded_layout.Layout.size.height)
              -. round
                   (cumulative_y +. unrounded_layout.Layout.size.height
                  -. unrounded_layout.Layout.border.bottom);
          };
        padding =
          {
            left =
              round (cumulative_x +. unrounded_layout.Layout.padding.left)
              -. round cumulative_x;
            right =
              round (cumulative_x +. unrounded_layout.Layout.size.width)
              -. round
                   (cumulative_x +. unrounded_layout.Layout.size.width
                  -. unrounded_layout.Layout.padding.right);
            top =
              round (cumulative_y +. unrounded_layout.Layout.padding.top)
              -. round cumulative_y;
            bottom =
              round (cumulative_y +. unrounded_layout.Layout.size.height)
              -. round
                   (cumulative_y +. unrounded_layout.Layout.size.height
                  -. unrounded_layout.Layout.padding.bottom);
          };
      };

    Tree.set_final_layout tree node_id !layout;

    let child_count = Tree.child_count tree node_id in
    for index = 0 to child_count - 1 do
      let child = Tree.get_child_id tree node_id index in
      round_layout_inner tree child cumulative_x cumulative_y
    done
  in
  round_layout_inner tree node_id 0.0 0.0
