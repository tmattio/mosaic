(*
  flexbox.ml
  ---------------------------------------------------------------------------
  **Flexbox layout**: computes the CSS flexbox layout algorithm based on the
  W3C specification. This is a full OCaml port of `flexbox.rs` from the Taffy
  reference implementation.

  The flexbox algorithm is implemented according to:
  https://www.w3.org/TR/css-flexbox-1/

  ---------------------------------------------------------------------------
  SPDX-License-Identifier: MIT OR Apache-2.0
  ---------------------------------------------------------------------------
*)

open Geometry

(* Use the module type from tree_intf.ml *)
module type LAYOUT_FLEXBOX_CONTAINER =
  Tree_intf.LayoutFlexboxContainer
    with type flexbox_container_style = Style.style
     and type flexbox_item_style = Style.style

(* Type aliases to match the new lowercase style *)
type 'a size = 'a Geometry.size
type 'a point = 'a Geometry.point
type 'a rect = 'a Geometry.rect

(** Helper to get first defined option *)
let first_some a b = match a with Some _ -> a | None -> b

(** Create a size with only main axis set *)
let size_from_main dir value =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> { width = value; height = None }
  | Style.Flex.Column | Style.Flex.Column_reverse ->
      { width = None; height = value }

type 'node flex_item = {
  node : 'node;  (** The identifier for the associated node *)
  order : int;  (** The order of the node relative to its siblings *)
  size : float option size;  (** The base size of this item *)
  min_size : float option size;  (** The minimum allowable size of this item *)
  max_size : float option size;  (** The maximum allowable size of this item *)
  align_self : Style.Alignment.align_self;
      (** The cross-alignment of this item *)
  overflow : Style.overflow point;  (** The overflow style of the item *)
  scrollbar_width : float;  (** The width of the scrollbars (if it has any) *)
  flex_shrink : float;  (** The flex shrink style of the item *)
  flex_grow : float;  (** The flex grow style of the item *)
  mutable resolved_minimum_main_size : float;
      (** The minimum size of the item, taking into account content-based
          automatic minimum sizes *)
  inset : float option rect;  (** The final offset of this item *)
  margin : float rect;  (** The margin of this item *)
  margin_is_auto : bool rect;
      (** Whether each margin is an auto margin or not *)
  padding : float rect;  (** The padding of this item *)
  border : float rect;  (** The border of this item *)
  mutable flex_basis : float;  (** The default size of this item *)
  mutable inner_flex_basis : float;
      (** The default size of this item, minus padding and border *)
  mutable violation : float;
      (** The amount by which this item has deviated from its target size *)
  mutable frozen : bool;  (** Is the size of this item locked *)
  mutable content_flex_fraction : float;
      (** Either the max- or min- content flex fraction See
          https://www.w3.org/TR/css-flexbox-1/#intrinsic-main-sizes *)
  mutable hypothetical_inner_size : float size;
      (** The proposed inner size of this item *)
  mutable hypothetical_outer_size : float size;
      (** The proposed outer size of this item *)
  mutable target_size : float size;  (** The size that this item wants to be *)
  mutable outer_target_size : float size;
      (** The size that this item wants to be, plus any padding and border *)
  mutable baseline : float;  (** The position of the bottom edge of this item *)
  mutable offset_main : float;  (** A temporary value for the main offset *)
  mutable offset_cross : float;  (** A temporary value for the cross offset *)
}
(** The intermediate results of a flexbox calculation for a single item *)

type algo_constants = {
  dir : Style.Flex.flex_direction;
      (** The direction of the current segment being laid out *)
  is_row : bool;  (** Is this segment a row *)
  is_column : bool;  (** Is this segment a column *)
  is_wrap : bool;  (** Is wrapping enabled (in either direction) *)
  is_wrap_reverse : bool;  (** Is the wrap direction inverted *)
  min_size : float option size;  (** The item's min_size style *)
  max_size : float option size;  (** The item's max_size style *)
  margin : float rect;  (** The margin of this section *)
  border : float rect;  (** The border of this section *)
  content_box_inset : float rect;
      (** The space between the content box and the border box. This consists of
          padding + border + scrollbar_gutter. *)
  scrollbar_gutter : float point;
      (** The size reserved for scrollbar gutters in each axis *)
  gap : float size;  (** The gap of this section *)
  align_items : Style.Alignment.align_items;
      (** The align_items property of this node *)
  align_content : Style.Alignment.align_content;
      (** The align_content property of this node *)
  justify_content : Style.Alignment.justify_content option;
      (** The justify_content property of this node *)
  node_outer_size : float option size;
      (** The border-box size of the node being laid out (if known) *)
  node_inner_size : float option size;
      (** The content-box size of the node being laid out (if known) *)
  container_size : float size;
      (** The size of the virtual container containing the flex items *)
  inner_container_size : float size;  (** The size of the internal container *)
}
(** Container for the constants used throughout the flexbox algorithm *)

(** Helper to convert child_iter to a list *)
let child_ids_to_list (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (node : Node.Node_id.t) : Node.Node_id.t list =
  (* This implementation assumes child_iter can be converted to a list.
     The actual implementation will depend on the concrete type. *)
  let count = Tree.child_count tree node in
  let rec collect acc i =
    if i >= count then List.rev acc
    else collect (Tree.get_child_id tree node i :: acc) (i + 1)
  in
  collect [] 0

(** Generate anonymous flex items from the container's children *)
let generate_anonymous_flex_items (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (node : Node.Node_id.t) (constants : algo_constants) :
    Node.Node_id.t flex_item list =
  let children = child_ids_to_list (module Tree) tree node in

  List.mapi
    (fun index child : Node.Node_id.t flex_item option ->
      let child_style = Tree.get_flexbox_child_style tree child in

      (* Extract style properties from the child style *)
      let position = child_style.position in
      let box_gen_mode =
        match child_style.display with
        | Style.None -> (Style.None : Style.box_generation_mode)
        | _ -> Style.Normal
      in

      (* Filter out absolute positioned and display:none items *)
      if
        position = Style.Absolute
        || box_gen_mode = (Style.None : Style.box_generation_mode)
      then None
      else
        let aspect_ratio = child_style.aspect_ratio in
        let box_sizing = child_style.box_sizing in

        (* Resolve spacing values *)
        let container_width = constants.node_inner_size.width in
        let padding =
          rect_map child_style.padding (fun lp ->
              match lp with
              | Style.Length_percentage.Length v -> v
              | Style.Length_percentage.Percent p ->
                  Option.value ~default:0.0
                    (Option.map (fun w -> w *. p) container_width))
        in
        let border =
          rect_map child_style.border (fun lp ->
              match lp with
              | Style.Length_percentage.Length v -> v
              | Style.Length_percentage.Percent p ->
                  Option.value ~default:0.0
                    (Option.map (fun w -> w *. p) container_width))
        in
        let pb_sum = rect_sum_axes (rect_add padding border) in
        let box_sizing_adjustment =
          if box_sizing = Style.Content_box then pb_sum else size_zero
        in

        (* Resolve size properties *)
        let resolve_dimension_with_aspect dim parent_size =
          match dim with
          | Style.Dimension.Auto -> None
          | Style.Dimension.Length v -> Some v
          | Style.Dimension.Percent p ->
              Option.map (fun s -> s *. p) parent_size
        in

        let apply_aspect_ratio_to_size size =
          match aspect_ratio with
          | None -> size
          | Some ratio -> (
              match (size.width, size.height) with
              | Some w, None -> { size with height = Some (w /. ratio) }
              | None, Some h -> { size with width = Some (h *. ratio) }
              | _ -> size)
        in

        let parent_size = constants.node_inner_size in
        let size_base =
          {
            width =
              resolve_dimension_with_aspect child_style.size.width
                parent_size.width;
            height =
              resolve_dimension_with_aspect child_style.size.height
                parent_size.height;
          }
        in
        let size =
          let with_aspect = apply_aspect_ratio_to_size size_base in
          if box_sizing = Style.Content_box then
            Math.size_maybe_add_option_float with_aspect box_sizing_adjustment
          else with_aspect
        in
        let min_size_base =
          {
            width =
              resolve_dimension_with_aspect child_style.min_size.width
                parent_size.width;
            height =
              resolve_dimension_with_aspect child_style.min_size.height
                parent_size.height;
          }
        in
        let min_size =
          let with_aspect = apply_aspect_ratio_to_size min_size_base in
          if box_sizing = Style.Content_box then
            Math.size_maybe_add_option_float with_aspect box_sizing_adjustment
          else with_aspect
        in
        let max_size_base =
          {
            width =
              resolve_dimension_with_aspect child_style.max_size.width
                parent_size.width;
            height =
              resolve_dimension_with_aspect child_style.max_size.height
                parent_size.height;
          }
        in
        let max_size =
          let with_aspect = apply_aspect_ratio_to_size max_size_base in
          if box_sizing = Style.Content_box then
            Math.size_maybe_add_option_float with_aspect box_sizing_adjustment
          else with_aspect
        in

        (* Create flex item *)
        Some
          {
            node = child;
            order = index;
            size;
            min_size;
            max_size;
            align_self =
              Option.value ~default:Style.Alignment.Stretch
                child_style.align_self;
            overflow = child_style.overflow;
            scrollbar_width = child_style.scrollbar_width;
            flex_shrink = child_style.flex_shrink;
            flex_grow = child_style.flex_grow;
            resolved_minimum_main_size = 0.0;
            inset =
              rect_map child_style.inset (fun lpa ->
                  match lpa with
                  | Style.Length_percentage_auto.Auto -> None
                  | Style.Length_percentage_auto.Length v -> Some v
                  | Style.Length_percentage_auto.Percent p ->
                      Option.map
                        (fun s -> s *. p)
                        (if constants.is_row then parent_size.width
                         else parent_size.height));
            margin =
              rect_map child_style.margin (fun lpa ->
                  match lpa with
                  | Style.Length_percentage_auto.Auto ->
                      0.0 (* Auto margins handled separately *)
                  | Style.Length_percentage_auto.Length v -> v
                  | Style.Length_percentage_auto.Percent p ->
                      Option.value ~default:0.0
                        (Option.map (fun w -> w *. p) container_width));
            margin_is_auto =
              rect_map child_style.margin (fun lpa ->
                  match lpa with
                  | Style.Length_percentage_auto.Auto -> true
                  | _ -> false);
            padding;
            border;
            flex_basis = 0.0;
            (* Will be computed in determine_flex_base_size *)
            inner_flex_basis = 0.0;
            violation = 0.0;
            frozen = false;
            content_flex_fraction = 0.0;
            hypothetical_inner_size = size_zero;
            hypothetical_outer_size = size_zero;
            target_size = size_zero;
            outer_target_size = size_zero;
            baseline = 0.0;
            offset_main = 0.0;
            offset_cross = 0.0;
          })
    children
  |> List.filter_map Fun.id

(** Determine available space for flex items *)
let determine_available_space (known_dimensions : float option size)
    (outer_available_space : Style.Available_space.t size)
    (constants : algo_constants) : Style.Available_space.t size =
  let width =
    match known_dimensions.width with
    | Some node_width ->
        Style.Available_space.Definite
          (node_width -. rect_horizontal_axis_sum constants.content_box_inset)
    | None ->
        let margin_sum = rect_horizontal_axis_sum constants.margin in
        let inset_sum = rect_horizontal_axis_sum constants.content_box_inset in
        ( outer_available_space.width |> fun x ->
          Style.Available_space.maybe_sub x margin_sum )
        |> fun x -> Style.Available_space.maybe_sub x inset_sum
  in

  let height =
    match known_dimensions.height with
    | Some node_height ->
        Style.Available_space.Definite
          (node_height -. rect_vertical_axis_sum constants.content_box_inset)
    | None ->
        let margin_sum = rect_vertical_axis_sum constants.margin in
        let inset_sum = rect_vertical_axis_sum constants.content_box_inset in
        ( outer_available_space.height |> fun x ->
          Style.Available_space.maybe_sub x margin_sum )
        |> fun x -> Style.Available_space.maybe_sub x inset_sum
  in

  { width; height }

type 'node flex_line = {
  items : 'node flex_item list;
  mutable cross_size : float;
  mutable offset_cross : float;
}
(** Flex line - a collection of items that fit on one line *)

(** Helper functions for axis operations *)
let main_size dir size =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> size.width
  | Style.Flex.Column | Style.Flex.Column_reverse -> size.height

let cross_size dir size =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> size.height
  | Style.Flex.Column | Style.Flex.Column_reverse -> size.width

let set_main_size dir size value =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> { size with width = value }
  | Style.Flex.Column | Style.Flex.Column_reverse ->
      { size with height = value }

let set_cross_size dir size value =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> { size with height = value }
  | Style.Flex.Column | Style.Flex.Column_reverse -> { size with width = value }

let main_axis dir =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> Layout.Requested_axis.Horizontal
  | Style.Flex.Column | Style.Flex.Column_reverse ->
      Layout.Requested_axis.Vertical

let cross_axis dir =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> Layout.Requested_axis.Vertical
  | Style.Flex.Column | Style.Flex.Column_reverse ->
      Layout.Requested_axis.Horizontal

let main_axis_sum dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect_horizontal_axis_sum rect
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect_vertical_axis_sum rect

let cross_axis_sum dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect_vertical_axis_sum rect
  | Style.Flex.Column | Style.Flex.Column_reverse ->
      rect_horizontal_axis_sum rect

(** Determine flex base size for each item *)
let determine_flex_base_size (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (constants : algo_constants)
    (available_space : Style.Available_space.t size)
    (flex_items : Node.Node_id.t flex_item list) : unit =
  let dir = constants.dir in

  List.iter
    (fun child ->
      let child_style = Tree.get_flexbox_child_style tree child.node in

      (* Parent size for child sizing *)
      let cross_axis_parent_size = cross_size dir constants.node_inner_size in
      let child_parent_size =
        set_cross_size dir size_none cross_axis_parent_size
      in

      (* Available space for child sizing *)
      let cross_axis_margin_sum = cross_axis_sum dir constants.margin in
      let child_min_cross =
        Option.map
          (fun v -> v +. cross_axis_margin_sum)
          (cross_size dir child.min_size)
      in
      let child_max_cross =
        Option.map
          (fun v -> v +. cross_axis_margin_sum)
          (cross_size dir child.max_size)
      in

      (* Clamp available space by min and max size *)
      let cross_axis_available_space =
        match cross_size dir available_space with
        | Style.Available_space.Definite value ->
            let clamped =
              Option.value ~default:value cross_axis_parent_size |> fun v ->
              match (child_min_cross, child_max_cross) with
              | None, None -> v
              | Some min, None -> Float.max min v
              | None, Some max -> Float.min v max
              | Some min, Some max -> Float.max min (Float.min v max)
            in
            Style.Available_space.Definite clamped
        | Style.Available_space.Min_content -> (
            match child_min_cross with
            | Some min -> Style.Available_space.Definite min
            | None -> Style.Available_space.Min_content)
        | Style.Available_space.Max_content -> (
            match child_max_cross with
            | Some max -> Style.Available_space.Definite max
            | None -> Style.Available_space.Max_content)
      in

      (* Known dimensions for child sizing *)
      let child_known_dimensions =
        let base = set_main_size dir child.size None in
        if
          child.align_self = Style.Alignment.Stretch
          && cross_size dir base = None
        then
          let cross_val =
            Style.Available_space.into_option cross_axis_available_space
            |> Option.map (fun v -> v -. cross_axis_sum dir child.margin)
          in
          set_cross_size dir base cross_val
        else base
      in

      (* Compute flex basis *)
      let container_width = main_size dir constants.node_inner_size in

      (* Get flex basis from child style *)
      let flex_basis_style =
        match child_style.flex_basis with
        | Style.Dimension.Auto -> None
        | Style.Dimension.Length v -> Some v
        | Style.Dimension.Percent p ->
            Option.map (fun w -> w *. p) container_width
      in

      let box_sizing = child_style.box_sizing in

      let box_sizing_adjustment =
        if box_sizing = Style.Content_box then
          main_size dir (rect_sum_axes (rect_add child.padding child.border))
        else 0.0
      in

      let flex_basis =
        match flex_basis_style with
        | Some basis -> Some (basis +. box_sizing_adjustment)
        | None ->
            Option.map
              (fun v -> v +. box_sizing_adjustment)
              (main_size dir child.size)
      in

      (* Debug output *)
      Printf.printf
        "determine_flex_base_size: dir=%s, child.size=(w=%s, h=%s), \
         main_size=%s, flex_basis=%s\n"
        (match dir with
        | Row -> "Row"
        | Row_reverse -> "Row_reverse"
        | Column -> "Column"
        | Column_reverse -> "Column_reverse")
        (Option.fold ~none:"None" ~some:(Printf.sprintf "%.1f") child.size.width)
        (Option.fold ~none:"None" ~some:(Printf.sprintf "%.1f")
           child.size.height)
        (Option.fold ~none:"None" ~some:(Printf.sprintf "%.1f")
           (main_size dir child.size))
        (Option.fold ~none:"None" ~some:(Printf.sprintf "%.1f") flex_basis);

      child.flex_basis <-
        (match flex_basis with
        | Some basis -> basis
        | None ->
            (* Size the item into available space *)
            let child_available_space =
              let main_space =
                if
                  main_size dir available_space
                  = Style.Available_space.Min_content
                then Style.Available_space.Min_content
                else Style.Available_space.Max_content
              in
              ( {
                  width = Style.Available_space.Max_content;
                  height = Style.Available_space.Max_content;
                }
              |> fun s -> set_main_size dir s main_space )
              |> fun s -> set_cross_size dir s cross_axis_available_space
            in

            let layout_input =
              {
                Layout.Layout_input.run_mode = Layout.Run_mode.Compute_size;
                sizing_mode = Layout.Sizing_mode.Content_size;
                axis = Layout.Requested_axis.Both;
                known_dimensions = child_known_dimensions;
                parent_size = child_parent_size;
                available_space = child_available_space;
                vertical_margins_are_collapsible =
                  { start = false; end_ = false };
              }
            in
            let layout_output =
              Tree.compute_child_layout tree child.node layout_input
            in
            main_size dir layout_output.size);

      (* Floor flex-basis by padding_border_sum *)
      let padding_border_sum =
        main_axis_sum dir child.padding +. main_axis_sum dir child.border
      in
      child.flex_basis <- max child.flex_basis padding_border_sum;

      (* Calculate inner flex basis *)
      child.inner_flex_basis <-
        child.flex_basis
        -. main_axis_sum dir child.padding
        -. main_axis_sum dir child.border;

      (* Calculate resolved minimum main size *)
      let style_min_main_size = main_size dir child.min_size in
      let auto_min_size =
        let overflow_dir =
          match dir with
          | Style.Flex.Row | Style.Flex.Row_reverse -> child.overflow.x
          | _ -> child.overflow.y
        in
        match overflow_dir with
        | Style.Visible | Style.Clip -> Option.None
        | Style.Hidden | Style.Scroll -> Option.Some 0.0
      in

      child.resolved_minimum_main_size <-
        (match first_some style_min_main_size auto_min_size with
        | Option.Some min -> min
        | Option.None ->
            (* Calculate min-content size *)
            let child_available_space =
              let base =
                {
                  width = Style.Available_space.Min_content;
                  height = Style.Available_space.Min_content;
                }
              in
              match dir with
              | Style.Flex.Row | Style.Flex.Row_reverse ->
                  { base with height = cross_axis_available_space }
              | Style.Flex.Column | Style.Flex.Column_reverse ->
                  { base with width = cross_axis_available_space }
            in

            let layout_input =
              {
                Layout.Layout_input.run_mode = Layout.Run_mode.Compute_size;
                sizing_mode = Layout.Sizing_mode.Content_size;
                axis = Layout.Requested_axis.Both;
                known_dimensions = child_known_dimensions;
                parent_size = child_parent_size;
                available_space = child_available_space;
                vertical_margins_are_collapsible =
                  { start = false; end_ = false };
              }
            in
            let layout_output =
              Tree.compute_child_layout tree child.node layout_input
            in
            let min_content_size = main_size dir layout_output.size in

            (* Subtract margins for min contribution *)
            let margin_sum = main_axis_sum dir child.margin in
            min child.flex_basis min_content_size -. margin_sum);

      (* Calculate hypothetical sizes *)
      let clamp_main value =
        Math.FloatOption.maybe_clamp value
          (main_size dir child.min_size)
          (main_size dir child.max_size)
      in
      let main_val = Some (clamp_main child.inner_flex_basis) in
      let sized = set_main_size dir child.size main_val in
      child.hypothetical_inner_size <-
        size_map sized (Option.value ~default:0.0);

      (* Debug output *)
      Printf.printf
        "hypothetical_inner_size: inner_flex_basis=%.1f, sized=(w=%s, h=%s), \
         hypothetical=(w=%.1f, h=%.1f)\n"
        child.inner_flex_basis
        (Option.fold ~none:"None" ~some:(Printf.sprintf "%.1f") sized.width)
        (Option.fold ~none:"None" ~some:(Printf.sprintf "%.1f") sized.height)
        child.hypothetical_inner_size.width child.hypothetical_inner_size.height;

      child.hypothetical_outer_size <-
        size_add child.hypothetical_inner_size
          (rect_sum_axes (rect_add child.padding child.border)))
    flex_items

(** Collect flex items into flex lines *)
let collect_flex_lines (constants : algo_constants)
    (available_space : Style.Available_space.t size)
    (flex_items : 'node flex_item list) : 'node flex_line list =
  let main_axis_available_space = main_size constants.dir available_space in
  let main_axis_gap = main_size constants.dir constants.gap in

  (* If not wrapping, return single line *)
  if not constants.is_wrap then
    [ { items = flex_items; cross_size = 0.0; offset_cross = 0.0 } ]
  else
    (* Implement line breaking for wrap mode *)
    let rec break_lines acc current_line current_main_size remaining_items =
      match remaining_items with
      | [] ->
          (* Add final line if it has items *)
          if current_line = [] then List.rev acc
          else
            List.rev
              ({
                 items = List.rev current_line;
                 cross_size = 0.0;
                 offset_cross = 0.0;
               }
              :: acc)
      | item :: rest ->
          let item_main_size =
            main_size constants.dir item.hypothetical_outer_size
            +. main_axis_sum constants.dir item.margin
          in

          let gap_penalty = if current_line = [] then 0.0 else main_axis_gap in
          let new_main_size =
            current_main_size +. item_main_size +. gap_penalty
          in

          (* Check if item fits on current line *)
          let fits =
            match main_axis_available_space with
            | Style.Available_space.Definite space ->
                current_line = [] || new_main_size <= space
            | _ -> true (* No wrapping with indefinite space *)
          in

          if fits then
            (* Add item to current line *)
            break_lines acc (item :: current_line) new_main_size rest
          else
            (* Start new line with this item *)
            let new_line =
              {
                items = List.rev current_line;
                cross_size = 0.0;
                offset_cross = 0.0;
              }
            in
            break_lines (new_line :: acc) [ item ] item_main_size rest
    in

    break_lines [] [] 0.0 flex_items

(** Helper to sum axis gaps *)
let sum_axis_gaps gap num_items =
  if num_items > 0 then gap *. float_of_int (num_items - 1) else 0.0

(** Determine container main size *)
let determine_container_main_size
    (available_space : Style.Available_space.t size)
    (flex_lines : 'a flex_line list) (constants : algo_constants ref) : unit =
  let main_padding_border =
    main_axis_sum !constants.dir !constants.content_box_inset
  in
  let main_margin = main_axis_sum !constants.dir !constants.margin in

  let outer_main_size =
    match main_size !constants.dir available_space with
    | Style.Available_space.Definite main_size_val ->
        let clamped =
          Math.FloatOption.maybe_clamp
            (main_size_val -. main_margin)
            (main_size !constants.dir !constants.min_size)
            (main_size !constants.dir !constants.max_size)
        in
        clamped
    | _ ->
        (* Calculate based on content *)
        let content_main_size =
          List.fold_left
            (fun acc line ->
              let line_main_size =
                List.fold_left
                  (fun acc item ->
                    acc +. main_size !constants.dir item.hypothetical_outer_size)
                  0.0 line.items
              in
              let gaps =
                sum_axis_gaps
                  (main_size !constants.dir !constants.gap)
                  (List.length line.items)
              in
              max acc (line_main_size +. gaps))
            0.0 flex_lines
        in

        let size = content_main_size +. main_padding_border in
        Math.FloatOption.maybe_clamp size
          (main_size !constants.dir !constants.min_size)
          (main_size !constants.dir !constants.max_size)
  in

  Printf.printf
    "determine_container_main_size: setting container main size to %.1f\n"
    outer_main_size;
  constants :=
    {
      !constants with
      container_size =
        set_main_size !constants.dir !constants.container_size outer_main_size;
      inner_container_size =
        set_main_size !constants.dir !constants.inner_container_size
          (outer_main_size -. main_padding_border);
    }

(** Resolve flexible lengths *)
let resolve_flexible_lengths (line : 'a flex_line) (constants : algo_constants)
    : unit =
  Printf.printf "resolve_flexible_lengths: %d items in line\n"
    (List.length line.items);
  let total_main_axis_gap =
    sum_axis_gaps
      (main_size constants.dir constants.gap)
      (List.length line.items)
  in

  (* 1. Determine the used flex factor *)
  let total_hypothetical_outer_main_size =
    List.fold_left
      (fun acc child ->
        acc +. main_size constants.dir child.hypothetical_outer_size)
      0.0 line.items
  in

  let used_flex_factor =
    total_main_axis_gap +. total_hypothetical_outer_main_size
  in
  let container_main_size =
    main_size constants.dir constants.inner_container_size
  in
  let growing = used_flex_factor < container_main_size in
  let shrinking = used_flex_factor > container_main_size in
  let exactly_sized = (not growing) && not shrinking in
  Printf.printf
    "Flex resolution: used_flex_factor=%.1f, container_main_size=%.1f, \
     growing=%b\n"
    used_flex_factor container_main_size growing;

  (* 2. Size inflexible items *)
  List.iter
    (fun child ->
      Printf.printf "Child: flex_grow=%.1f, flex_shrink=%.1f\n" child.flex_grow
        child.flex_shrink;
      let inner_target_size =
        main_size constants.dir child.hypothetical_inner_size
      in
      (* Initialize target_size with hypothetical_inner_size *)
      child.target_size <- child.hypothetical_inner_size;
      (* Then update the main axis with the computed value *)
      child.target_size <-
        set_main_size constants.dir child.target_size inner_target_size;

      if
        exactly_sized
        || (child.flex_grow = 0.0 && child.flex_shrink = 0.0)
        || growing
           && child.flex_basis
              > main_size constants.dir child.hypothetical_inner_size
        || shrinking
           && child.flex_basis
              < main_size constants.dir child.hypothetical_inner_size
      then (
        Printf.printf
          "Freezing child: exactly_sized=%b, grow=%.1f, shrink=%.1f, growing=%b\n"
          exactly_sized child.flex_grow child.flex_shrink growing;
        child.frozen <- true;
        let outer_target_size =
          inner_target_size +. main_axis_sum constants.dir child.margin
        in
        child.outer_target_size <-
          set_main_size constants.dir child.outer_target_size outer_target_size))
    line.items;

  if exactly_sized then ()
  else
    (* 3. Loop to resolve flexible lengths *)
    let rec flex_loop () =
      (* Check for flexible items *)
      let unfrozen_items =
        List.filter (fun child -> not child.frozen) line.items
      in

      if unfrozen_items = [] then ()
      else
        (* Calculate remaining free space *)
        let used_space =
          total_main_axis_gap
          +. List.fold_left
               (fun acc child ->
                 acc
                 +.
                 if child.frozen then
                   main_size constants.dir child.outer_target_size
                 else
                   child.flex_basis +. main_axis_sum constants.dir child.margin)
               0.0 line.items
        in

        let remaining_free_space = container_main_size -. used_space in

        if abs_float remaining_free_space < 0.01 then
          (* Close enough to zero, freeze all items *)
          List.iter
            (fun child ->
              if not child.frozen then (
                child.frozen <- true;
                child.target_size <-
                  set_main_size constants.dir child.target_size
                    child.inner_flex_basis;
                child.outer_target_size <-
                  set_main_size constants.dir child.outer_target_size
                    (child.inner_flex_basis
                    +. main_axis_sum constants.dir child.margin)))
            line.items
        else
          (* Distribute free space *)
          let flex_factor_sum =
            List.fold_left
              (fun acc child ->
                if child.frozen then acc
                else if growing then acc +. child.flex_grow
                else acc +. (child.flex_shrink *. child.inner_flex_basis))
              0.0 line.items
          in

          (* Distribute space and check for violations *)
          let violation_occurred = ref false in

          (* Only distribute space if there are flex factors to work with *)
          Printf.printf
            "Flex distribution: growing=%b, shrinking=%b, flex_factor_sum=%.1f\n"
            growing shrinking flex_factor_sum;
          if
            (growing && flex_factor_sum > 0.0)
            || (shrinking && flex_factor_sum > 0.0)
          then
            List.iter
              (fun child ->
                if not child.frozen then (
                  let child_flex_factor =
                    if growing then child.flex_grow
                    else child.flex_shrink *. child.inner_flex_basis
                  in

                  let ratio =
                    if flex_factor_sum > 0.0 then
                      child_flex_factor /. flex_factor_sum
                    else 0.0
                  in

                  let child_free_space = remaining_free_space *. ratio in
                  let child_target_size =
                    child.inner_flex_basis +. child_free_space
                  in

                  (* Check for min/max violations *)
                  let clamped =
                    Math.FloatOption.maybe_clamp child_target_size
                      (main_size constants.dir child.min_size)
                      (main_size constants.dir child.max_size)
                  in

                  child.violation <- clamped -. child_target_size;
                  child.target_size <-
                    set_main_size constants.dir child.target_size clamped;
                  child.outer_target_size <-
                    set_main_size constants.dir child.outer_target_size
                      (clamped +. main_axis_sum constants.dir child.margin);

                  if abs_float child.violation > 0.01 then (
                    violation_occurred := true;
                    child.frozen <- true)))
              line.items
          else (
            (* No flex factors - just check min/max violations *)
            Printf.printf "No flex factors case - freezing all unfrozen items\n";
            List.iter
              (fun child ->
                if not child.frozen then (
                  Printf.printf
                    "Processing unfrozen child: target_size main=%.1f\n"
                    (main_size constants.dir child.target_size);
                  (* Clamp by min/max *)
                  let current_main =
                    main_size constants.dir child.target_size
                  in
                  let clamped =
                    Math.FloatOption.maybe_clamp current_main
                      (main_size constants.dir child.min_size)
                      (main_size constants.dir child.max_size)
                  in

                  child.violation <- clamped -. current_main;
                  if abs_float child.violation > 0.01 then (
                    child.target_size <-
                      set_main_size constants.dir child.target_size clamped;
                    child.outer_target_size <-
                      set_main_size constants.dir child.outer_target_size
                        (clamped +. main_axis_sum constants.dir child.margin);
                    violation_occurred := true)
                  else
                    (* No violation - still need to set outer_target_size *)
                    child.outer_target_size <-
                      set_main_size constants.dir child.outer_target_size
                        (current_main
                        +. main_axis_sum constants.dir child.margin);
                  (* Always freeze when no flex factors *)
                  child.frozen <- true))
              line.items);

          (* If violations occurred, loop again; otherwise freeze remaining *)
          if !violation_occurred then flex_loop ()
          else
            List.iter
              (fun child -> if not child.frozen then child.frozen <- true)
              line.items
    in

    flex_loop ()

(** Determine hypothetical cross size *)
let determine_hypothetical_cross_size (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (line : Node.Node_id.t flex_line) (constants : algo_constants)
    (available_space : Style.Available_space.t size) : unit =
  List.iter
    (fun child ->
      let child_cross_size = cross_size constants.dir child.size in

      if child_cross_size = None then (
        (* Need to compute cross size *)
        let child_main_size = main_size constants.dir child.target_size in
        let child_known_dimensions =
          ( child.size |> fun size ->
            set_main_size constants.dir size (Some child_main_size) )
          |> fun size -> set_cross_size constants.dir size None
        in

        let child_parent_size =
          constants.node_inner_size |> fun size ->
          set_main_size constants.dir size
            (Some (main_size constants.dir constants.inner_container_size))
        in

        let child_available_space =
          match constants.dir with
          | Style.Flex.Row | Style.Flex.Row_reverse ->
              {
                available_space with
                width = Style.Available_space.Definite child_main_size;
              }
          | Style.Flex.Column | Style.Flex.Column_reverse ->
              {
                available_space with
                height = Style.Available_space.Definite child_main_size;
              }
        in

        (* Align stretch overrides computed cross size *)
        if
          child.align_self = Style.Alignment.Stretch
          && cross_size constants.dir child.size = None
          && cross_size constants.dir available_space
             <> Style.Available_space.Min_content
        then (
          let cross_size_val =
            match cross_size constants.dir available_space with
            | Style.Available_space.Definite size ->
                size -. cross_axis_sum constants.dir child.margin
            | _ -> 0.0 (* Should not happen due to check above *)
          in
          child.target_size <-
            set_cross_size constants.dir child.target_size cross_size_val;
          child.outer_target_size <-
            set_cross_size constants.dir child.outer_target_size
              (cross_size_val +. cross_axis_sum constants.dir child.margin))
        else
          (* Measure child cross size *)
          let layout_input =
            {
              Layout.Layout_input.run_mode = Layout.Run_mode.Compute_size;
              sizing_mode = Layout.Sizing_mode.Content_size;
              axis = Layout.Requested_axis.Both;
              known_dimensions = child_known_dimensions;
              parent_size = child_parent_size;
              available_space = child_available_space;
              vertical_margins_are_collapsible = { start = false; end_ = false };
            }
          in
          let layout_output =
            Tree.compute_child_layout tree child.node layout_input
          in
          let measured_size = layout_output.size in

          let measured_cross_size = cross_size constants.dir measured_size in

          let clamped =
            Math.FloatOption.maybe_clamp measured_cross_size
              (cross_size constants.dir child.min_size)
              (cross_size constants.dir child.max_size)
          in

          child.target_size <-
            set_cross_size constants.dir child.target_size clamped;
          child.outer_target_size <-
            set_cross_size constants.dir child.outer_target_size
              (clamped +. cross_axis_sum constants.dir child.margin))
      else
        (* Cross size already known - set target_size cross dimension *)
        let cross_val =
          match child_cross_size with Some s -> s | None -> 0.0
        in
        Printf.printf
          "determine_hypothetical_cross_size: cross_val=%.1f, current \
           target_size=(%.1f x %.1f)\n"
          cross_val child.target_size.width child.target_size.height;
        child.target_size <-
          set_cross_size constants.dir child.target_size cross_val;
        Printf.printf "After setting cross: target_size=(%.1f x %.1f)\n"
          child.target_size.width child.target_size.height;
        child.outer_target_size <-
          set_cross_size constants.dir child.outer_target_size
            (cross_val +. cross_axis_sum constants.dir child.margin))
    line.items

(** Calculate children baselines *)
let calculate_children_base_lines (flex_lines : 'a flex_line list ref)
    (constants : algo_constants) : unit =
  (* Check if we need baselines *)
  let needs_baseline =
    List.exists
      (fun line ->
        List.exists
          (fun child -> child.align_self = Style.Alignment.Baseline)
          line.items)
      !flex_lines
  in

  if needs_baseline then
    List.iter
      (fun line ->
        List.iter
          (fun child ->
            (* Calculate baseline for each child that needs it *)
            if child.align_self = Style.Alignment.Baseline then
              (* For now, use a simple baseline calculation *)
              (* TODO: Implement proper baseline calculation *)
              child.baseline <-
                cross_size constants.dir child.target_size *. 0.8)
          line.items)
      !flex_lines

(** Calculate cross size of each flex line *)
let calculate_cross_size (flex_lines : 'a flex_line list ref)
    (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      (* Calculate line cross size based on largest item *)
      let max_cross_size =
        List.fold_left
          (fun acc child ->
            max acc (cross_size constants.dir child.outer_target_size))
          0.0 line.items
      in
      line.cross_size <- max_cross_size)
    !flex_lines

(** Handle align-content: stretch *)
let handle_align_content_stretch (flex_lines : 'a flex_line list ref)
    (known_dimensions : float option size) (constants : algo_constants) : unit =
  let num_lines = List.length !flex_lines in
  if num_lines > 0 && constants.align_content = Style.Alignment.Stretch then
    match cross_size constants.dir known_dimensions with
    | Some container_cross_size ->
        let total_cross_gaps =
          sum_axis_gaps (cross_size constants.dir constants.gap) num_lines
        in
        let total_line_cross_size =
          List.fold_left
            (fun acc line -> acc +. line.cross_size)
            0.0 !flex_lines
        in

        let free_space =
          container_cross_size -. total_line_cross_size -. total_cross_gaps
        in
        if free_space > 0.0 then
          let space_per_line = free_space /. float_of_int num_lines in
          List.iter
            (fun line -> line.cross_size <- line.cross_size +. space_per_line)
            !flex_lines
    | None -> ()

(** Determine used cross size of each flex item *)
let determine_used_cross_size (flex_lines : 'a flex_line list)
    (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      List.iter
        (fun child ->
          if
            child.align_self = Style.Alignment.Stretch
            && cross_size constants.dir child.size = None
          then (
            (* Stretch to fill line cross size *)
            let stretched_cross_size =
              line.cross_size -. cross_axis_sum constants.dir child.margin
            in
            let clamped =
              Math.FloatOption.maybe_clamp stretched_cross_size
                (cross_size constants.dir child.min_size)
                (cross_size constants.dir child.max_size)
            in
            child.target_size <-
              set_cross_size constants.dir child.target_size clamped;
            child.outer_target_size <-
              set_cross_size constants.dir child.outer_target_size
                (clamped +. cross_axis_sum constants.dir child.margin)))
        line.items)
    flex_lines

(** Perform main axis alignment *)
let perform_main_alignment (flex_lines : 'a flex_line list)
    (constants : algo_constants) : unit =
  Printf.printf "perform_main_alignment called with %d lines\n"
    (List.length flex_lines);
  List.iter
    (fun line ->
      let total_main_axis_gap =
        sum_axis_gaps
          (main_size constants.dir constants.gap)
          (List.length line.items)
      in
      let used_space =
        total_main_axis_gap
        +. List.fold_left
             (fun acc child ->
               acc +. main_size constants.dir child.outer_target_size)
             0.0 line.items
      in

      let free_space =
        main_size constants.dir constants.inner_container_size -. used_space
      in
      let num_items = List.length line.items in

      (* Calculate main axis offset based on justification *)
      let start_offset, gap_addition =
        match constants.justify_content with
        | None | Some Style.Alignment.Flex_start -> (0.0, 0.0)
        | Some Style.Alignment.Flex_end -> (free_space, 0.0)
        | Some Style.Alignment.Center -> (free_space /. 2.0, 0.0)
        | Some Style.Alignment.Space_between ->
            if num_items > 1 then
              (0.0, free_space /. float_of_int (num_items - 1))
            else (0.0, 0.0)
        | Some Style.Alignment.Space_around ->
            let space_per_item = free_space /. float_of_int num_items in
            (space_per_item /. 2.0, space_per_item)
        | Some Style.Alignment.Space_evenly ->
            let space_per_gap = free_space /. float_of_int (num_items + 1) in
            (space_per_gap, space_per_gap)
        | _ -> (0.0, 0.0)
        (* Other values like Start, End, etc. *)
      in

      (* Position items along main axis *)
      let rec position_items (items : 'a flex_item list) offset =
        match items with
        | [] -> ()
        | (child : 'a flex_item) :: rest ->
            child.offset_main <-
              offset +. (main_axis_sum constants.dir child.margin /. 2.0);
            Printf.printf
              "Positioning child: offset=%.1f, outer_size=%.1f, next_offset \
               will be %.1f\n"
              offset
              (main_size constants.dir child.outer_target_size)
              (offset
              +. main_size constants.dir child.outer_target_size
              +. main_size constants.dir constants.gap
              +. gap_addition);
            let next_offset =
              offset
              +. main_size constants.dir child.outer_target_size
              +. main_size constants.dir constants.gap
              +. gap_addition
            in
            position_items rest next_offset
      in

      position_items line.items start_offset)
    flex_lines

(** Perform cross axis alignment *)
let perform_cross_alignment (flex_lines : 'a flex_line list)
    (constants : algo_constants) : unit =
  (* First align lines *)
  let total_cross_axis_gap =
    sum_axis_gaps
      (cross_size constants.dir constants.gap)
      (List.length flex_lines)
  in
  let used_space =
    total_cross_axis_gap
    +. List.fold_left (fun acc line -> acc +. line.cross_size) 0.0 flex_lines
  in

  let free_space =
    cross_size constants.dir constants.inner_container_size -. used_space
  in
  let num_lines = List.length flex_lines in

  (* Calculate line offsets based on align-content *)
  let start_offset, gap_addition =
    match constants.align_content with
    | Style.Alignment.Flex_start -> (0.0, 0.0)
    | Style.Alignment.Flex_end -> (free_space, 0.0)
    | Style.Alignment.Center -> (free_space /. 2.0, 0.0)
    | Style.Alignment.Space_between ->
        if num_lines > 1 then (0.0, free_space /. float_of_int (num_lines - 1))
        else (0.0, 0.0)
    | Style.Alignment.Space_around ->
        let space_per_line = free_space /. float_of_int num_lines in
        (space_per_line /. 2.0, space_per_line)
    | Style.Alignment.Space_evenly ->
        let space_per_gap = free_space /. float_of_int (num_lines + 1) in
        (space_per_gap, space_per_gap)
    | Style.Alignment.Stretch -> (0.0, 0.0) (* Already handled *)
    | _ -> (0.0, 0.0)
  in

  (* Position lines and items within lines *)
  let rec position_lines lines offset =
    match lines with
    | [] -> ()
    | line :: rest ->
        line.offset_cross <- offset;

        (* Align items within line *)
        List.iter
          (fun child ->
            let free_space =
              line.cross_size
              -. cross_size constants.dir child.outer_target_size
            in
            let offset_within_line =
              match child.align_self with
              | Style.Alignment.Flex_start -> 0.0
              | Style.Alignment.Flex_end -> free_space
              | Style.Alignment.Center -> free_space /. 2.0
              | Style.Alignment.Baseline ->
                  0.0 (* TODO: proper baseline alignment *)
              | Style.Alignment.Stretch -> 0.0
              | _ -> 0.0
            in

            child.offset_cross <-
              offset +. offset_within_line
              +. (cross_axis_sum constants.dir child.margin /. 2.0))
          line.items;

        let next_offset =
          offset +. line.cross_size
          +. cross_size constants.dir constants.gap
          +. gap_addition
        in
        position_lines rest next_offset
  in

  position_lines flex_lines start_offset

(** Resolve cross-axis auto margins *)
let resolve_cross_axis_auto_margins (flex_lines : 'a flex_line list)
    (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      List.iter
        (fun child ->
          (* Check for auto margins in cross axis *)
          let has_auto_margin =
            match constants.dir with
            | Style.Flex.Row | Style.Flex.Row_reverse ->
                child.margin_is_auto.top || child.margin_is_auto.bottom
            | Style.Flex.Column | Style.Flex.Column_reverse ->
                child.margin_is_auto.left || child.margin_is_auto.right
          in

          if has_auto_margin then
            (* TODO: Implement auto margin resolution *)
            ())
        line.items)
    flex_lines

(** Determine final container size *)
let determine_container_size (known_dimensions : float option size)
    (constants : algo_constants) (flex_lines : 'a flex_line list) : float size =
  (* Main size already determined *)
  let main_size = main_size constants.dir constants.container_size in
  Printf.printf
    "determine_container_size: container_size=(%.1f, %.1f), main_size=%.1f\n"
    constants.container_size.width constants.container_size.height main_size;

  (* Determine cross size if not known *)
  let cross_size_val =
    match cross_size constants.dir known_dimensions with
    | Some size -> size
    | None ->
        (* Calculate based on content *)
        let content_cross_size =
          let lines_size =
            List.fold_left
              (fun acc line -> acc +. line.cross_size)
              0.0 flex_lines
          in
          let gaps =
            sum_axis_gaps
              (cross_size constants.dir constants.gap)
              (List.length flex_lines)
          in
          lines_size +. gaps
          +. cross_axis_sum constants.dir constants.content_box_inset
        in

        Math.FloatOption.maybe_clamp content_cross_size
          (cross_size constants.dir constants.min_size)
          (cross_size constants.dir constants.max_size)
  in

  (* Update container sizes in constants *)
  let updated_container_size =
    set_cross_size constants.dir constants.container_size cross_size_val
  in
  let inner_cross_size =
    Float.max
      (cross_size_val
      -. cross_axis_sum constants.dir constants.content_box_inset)
      0.0
  in
  let updated_inner_container_size =
    set_cross_size constants.dir constants.inner_container_size inner_cross_size
  in

  (* Create updated constants - but since this function doesn't return constants,
     we assume these updates are handled elsewhere in the actual implementation *)
  let _ = (updated_container_size, updated_inner_container_size) in

  let final_size =
    let size_with_main = set_main_size constants.dir size_zero main_size in
    set_cross_size constants.dir size_with_main cross_size_val
  in

  final_size

(** Perform final layout and positioning *)
let perform_final_layout_and_positioning (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (node : Node.Node_id.t) (container_size : float size)
    (constants : algo_constants) (flex_lines : Node.Node_id.t flex_line list) :
    float size =
  Printf.printf "perform_final_layout_and_positioning called with %d lines\n"
    (List.length flex_lines);
  (* Set container layout *)
  let container_layout = Layout.Layout.empty in

  Tree.set_unrounded_layout tree node container_layout;

  (* Track content size *)
  let content_size = ref size_zero in

  (* Layout all flex items *)
  List.iter
    (fun line ->
      List.iter
        (fun child ->
          (* Calculate final position *)
          let location =
            match constants.dir with
            | Style.Flex.Row ->
                {
                  x = child.offset_main +. constants.content_box_inset.left;
                  y = child.offset_cross +. constants.content_box_inset.top;
                }
            | Style.Flex.Row_reverse ->
                {
                  x =
                    container_size.width -. child.offset_main
                    -. main_size constants.dir child.target_size
                    -. constants.content_box_inset.right;
                  y = child.offset_cross +. constants.content_box_inset.top;
                }
            | Style.Flex.Column ->
                {
                  x = child.offset_cross +. constants.content_box_inset.left;
                  y = child.offset_main +. constants.content_box_inset.top;
                }
            | Style.Flex.Column_reverse ->
                {
                  x = child.offset_cross +. constants.content_box_inset.left;
                  y =
                    container_size.height -. child.offset_main
                    -. main_size constants.dir child.target_size
                    -. constants.content_box_inset.bottom;
                }
          in

          (* Apply inset/position adjustments if needed *)
          (* TODO: Handle absolute positioning *)
          Printf.printf "Setting child layout: target_size=(%.1f x %.1f)\n"
            child.target_size.width child.target_size.height;
          let child_layout =
            {
              Layout.Layout.empty with
              order = child.order;
              location;
              size = child.target_size;
            }
          in

          Tree.set_unrounded_layout tree child.node child_layout;

          (* Update content size *)
          (* @feature content_size *)
          let child_overflow = child.overflow in
          content_size :=
            size_zip_map !content_size
              (Content_size.compute_content_size_contribution ~location
                 ~size:child.target_size ~content_size:child.target_size
                 ~overflow:child_overflow) (fun a b -> Float.max a b))
        line.items)
    flex_lines;

  !content_size

(** Perform absolute layout on absolutely positioned children *)
let perform_absolute_layout_on_absolute_children (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (node : Node.Node_id.t) (constants : algo_constants) : float size =
  let module TExt = Tree_intf.LayoutPartialTreeExt (Tree) in
  let content_size = ref size_zero in
  let child_count = Tree.child_count tree node in

  for order = 0 to child_count - 1 do
    let child_id = Tree.get_child_id tree node order in
    let child_style = Tree.get_flexbox_child_style tree child_id in

    (* Skip items that are display:none or are not position:absolute *)
    if
      child_style.display <> Style.None && child_style.position = Style.Absolute
    then (
      let overflow = child_style.overflow in
      let scrollbar_width = child_style.scrollbar_width in
      let aspect_ratio = child_style.aspect_ratio in
      let align_self =
        Option.value ~default:constants.align_items child_style.align_self
      in

      (* Resolve margins *)
      let margin =
        rect_map child_style.margin (fun lpa ->
            match lpa with
            | Style.Length_percentage_auto.Auto -> None
            | Style.Length_percentage_auto.Length v -> Some v
            | Style.Length_percentage_auto.Percent p ->
                Option.map (fun w -> w *. p) constants.node_inner_size.width)
      in

      (* Resolve padding and border *)
      let padding =
        rect_map child_style.padding (fun lp ->
            match lp with
            | Style.Length_percentage.Length v -> v
            | Style.Length_percentage.Percent p ->
                Option.value ~default:0.0
                  (Option.map (fun w -> w *. p) constants.node_inner_size.width))
      in
      let border =
        rect_map child_style.border (fun lp ->
            match lp with
            | Style.Length_percentage.Length v -> v
            | Style.Length_percentage.Percent p ->
                Option.value ~default:0.0
                  (Option.map (fun w -> w *. p) constants.node_inner_size.width))
      in
      let padding_border_sum = rect_sum_axes (rect_add padding border) in
      let box_sizing_adjustment =
        if child_style.box_sizing = Style.Content_box then padding_border_sum
        else size_zero
      in

      (* Resolve inset *)
      let inset =
        rect_map child_style.inset (fun lpa ->
            match lpa with
            | Style.Length_percentage_auto.Auto -> None
            | Style.Length_percentage_auto.Length v -> Some v
            | Style.Length_percentage_auto.Percent p ->
                Option.map
                  (fun s -> s *. p)
                  (if constants.is_row then constants.node_inner_size.width
                   else constants.node_inner_size.height))
      in

      (* Compute known dimensions *)
      let resolve_dimension_with_aspect dim parent_size =
        match dim with
        | Style.Dimension.Auto -> None
        | Style.Dimension.Length v -> Some v
        | Style.Dimension.Percent p -> Option.map (fun s -> s *. p) parent_size
      in

      let apply_aspect_ratio_to_size size =
        match aspect_ratio with
        | None -> size
        | Some ratio -> (
            match (size.width, size.height) with
            | Some w, None -> { size with height = Some (w /. ratio) }
            | None, Some h -> { size with width = Some (h *. ratio) }
            | _ -> size)
      in

      let parent_size = constants.node_inner_size in
      let size =
        {
          width =
            resolve_dimension_with_aspect child_style.size.width
              parent_size.width;
          height =
            resolve_dimension_with_aspect child_style.size.height
              parent_size.height;
        }
        |> apply_aspect_ratio_to_size
        |> fun size ->
        size_zip_map size box_sizing_adjustment (fun sz adj ->
            Option.map (fun s -> s +. adj) sz)
      in

      let min_size =
        {
          width =
            resolve_dimension_with_aspect child_style.min_size.width
              parent_size.width;
          height =
            resolve_dimension_with_aspect child_style.min_size.height
              parent_size.height;
        }
      in

      let max_size =
        {
          width =
            resolve_dimension_with_aspect child_style.max_size.width
              parent_size.width;
          height =
            resolve_dimension_with_aspect child_style.max_size.height
              parent_size.height;
        }
      in

      (* Perform child layout *)
      let layout_output =
        TExt.perform_child_layout tree child_id ~known_dimensions:size
          ~parent_size
          ~available_space:
            {
              width =
                Style.Available_space.Definite
                  constants.inner_container_size.width;
              height =
                Style.Available_space.Definite
                  constants.inner_container_size.height;
            }
          ~sizing_mode:Layout.Sizing_mode.Content_size
          ~vertical_margins_are_collapsible:line_false
      in

      let measured_size = layout_output.size in
      let unclamped =
        {
          width =
            (match size.width with
            | Some w -> Some w
            | None -> Some measured_size.width);
          height =
            (match size.height with
            | Some h -> Some h
            | None -> Some measured_size.height);
        }
      in
      let clamped = Size.maybe_clamp unclamped min_size max_size in
      let final_size =
        {
          width = Option.value ~default:measured_size.width clamped.width;
          height = Option.value ~default:measured_size.height clamped.height;
        }
      in

      (* Resolve auto margins and compute position *)
      let free_space =
        {
          width = constants.inner_container_size.width -. final_size.width;
          height = constants.inner_container_size.height -. final_size.height;
        }
      in

      let auto_margin_counts =
        {
          width =
            ((if Option.is_none margin.left then 1 else 0)
            + if Option.is_none margin.right then 1 else 0);
          height =
            ((if Option.is_none margin.top then 1 else 0)
            + if Option.is_none margin.bottom then 1 else 0);
        }
      in

      let auto_margin_size =
        {
          width =
            (if auto_margin_counts.width > 0 then
               free_space.width /. float_of_int auto_margin_counts.width
             else 0.0);
          height =
            (if auto_margin_counts.height > 0 then
               free_space.height /. float_of_int auto_margin_counts.height
             else 0.0);
        }
      in

      let resolved_margin =
        {
          left = Option.value ~default:auto_margin_size.width margin.left;
          right = Option.value ~default:auto_margin_size.width margin.right;
          top = Option.value ~default:auto_margin_size.height margin.top;
          bottom = Option.value ~default:auto_margin_size.height margin.bottom;
        }
      in

      (* Compute position based on inset *)
      let location =
        {
          x =
            (match (inset.left, inset.right) with
            | Some left, _ ->
                constants.content_box_inset.left +. left +. resolved_margin.left
            | None, Some right ->
                constants.inner_container_size.width -. final_size.width
                -. right -. resolved_margin.right
                -. constants.content_box_inset.right
            | None, None ->
                (* Apply alignment *)
                let free_space_x =
                  constants.inner_container_size.width -. final_size.width
                  -. resolved_margin.left -. resolved_margin.right
                in
                let offset_x =
                  match align_self with
                  | Style.Alignment.Start | Style.Alignment.Flex_start -> 0.0
                  | Style.Alignment.End | Style.Alignment.Flex_end ->
                      free_space_x
                  | Style.Alignment.Center -> free_space_x /. 2.0
                  | _ -> 0.0
                in
                constants.content_box_inset.left +. resolved_margin.left
                +. offset_x);
          y =
            (match (inset.top, inset.bottom) with
            | Some top, _ ->
                constants.content_box_inset.top +. top +. resolved_margin.top
            | None, Some bottom ->
                constants.inner_container_size.height -. final_size.height
                -. bottom -. resolved_margin.bottom
                -. constants.content_box_inset.bottom
            | None, None ->
                (* Apply alignment *)
                let free_space_y =
                  constants.inner_container_size.height -. final_size.height
                  -. resolved_margin.top -. resolved_margin.bottom
                in
                let offset_y =
                  match align_self with
                  | Style.Alignment.Start | Style.Alignment.Flex_start -> 0.0
                  | Style.Alignment.End | Style.Alignment.Flex_end ->
                      free_space_y
                  | Style.Alignment.Center -> free_space_y /. 2.0
                  | _ -> 0.0
                in
                constants.content_box_inset.top +. resolved_margin.top
                +. offset_y);
        }
      in

      (* Set scrollbar size *)
      let scrollbar_size =
        {
          width = (if overflow.y = Style.Scroll then scrollbar_width else 0.0);
          height = (if overflow.x = Style.Scroll then scrollbar_width else 0.0);
        }
      in

      (* Set child layout *)
      Tree.set_unrounded_layout tree child_id
        {
          Layout.Layout.empty with
          order;
          location;
          size = final_size;
          padding;
          border;
          margin = resolved_margin;
          scrollbar_size;
        };

      (* Update content size *)
      (* @feature content_size *)
      content_size :=
        size_zip_map !content_size
          (Content_size.compute_content_size_contribution ~location
             ~size:final_size ~content_size:layout_output.size ~overflow)
          (fun a b -> Float.max a b))
  done;

  !content_size

(** Compute a preliminary size for an item *)
let compute_preliminary (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (node : Node.Node_id.t) (inputs : Layout.Layout_input.t) :
    Layout.Layout_output.t =
  let open Layout.Layout_input in
  let { known_dimensions; parent_size; available_space; _ } = inputs in

  (* Define some general constants we will need for the remainder of the algorithm *)
  let style = Tree.get_flexbox_container_style tree node in
  (* Extract style properties and create constants - inline to avoid module passing issues *)
  let dir = style.flex_direction in
  let is_row =
    match dir with
    | Style.Flex.Row | Style.Flex.Row_reverse -> true
    | _ -> false
  in
  let is_column =
    match dir with
    | Style.Flex.Column | Style.Flex.Column_reverse -> true
    | _ -> false
  in
  let is_wrap =
    match style.flex_wrap with
    | Style.Flex.Wrap | Style.Flex.Wrap_reverse -> true
    | _ -> false
  in
  let is_wrap_reverse =
    match style.flex_wrap with Style.Flex.Wrap_reverse -> true | _ -> false
  in

  (* Extract spacing values from style *)
  let container_width = Option.value ~default:0.0 parent_size.width in
  let margin =
    rect_map style.margin (fun lpa ->
        match lpa with
        | Style.Length_percentage_auto.Auto -> 0.0
        | Style.Length_percentage_auto.Length v -> v
        | Style.Length_percentage_auto.Percent p -> container_width *. p)
  in
  let border =
    rect_map style.border (fun lp ->
        match lp with
        | Style.Length_percentage.Length v -> v
        | Style.Length_percentage.Percent p -> container_width *. p)
  in
  let padding =
    rect_map style.padding (fun lp ->
        match lp with
        | Style.Length_percentage.Length v -> v
        | Style.Length_percentage.Percent p -> container_width *. p)
  in
  let content_box_inset = rect_add padding border in
  let scrollbar_gutter =
    if style.overflow.x = Style.Scroll || style.overflow.y = Style.Scroll then
      { x = style.scrollbar_width; y = style.scrollbar_width }
    else point_zero
  in

  let node_outer_size = known_dimensions in
  let node_inner_size =
    let inset_sum = rect_sum_axes content_box_inset in
    {
      width = Option.map (fun w -> w -. inset_sum.width) node_outer_size.width;
      height =
        Option.map (fun h -> h -. inset_sum.height) node_outer_size.height;
    }
  in
  Printf.printf
    "compute_preliminary: known_dimensions=(width=%s, height=%s), \
     node_inner_size=(width=%s, height=%s)\n"
    (match known_dimensions.width with
    | Some v -> Printf.sprintf "Some(%.1f)" v
    | None -> "None")
    (match known_dimensions.height with
    | Some v -> Printf.sprintf "Some(%.1f)" v
    | None -> "None")
    (match node_inner_size.width with
    | Some v -> Printf.sprintf "Some(%.1f)" v
    | None -> "None")
    (match node_inner_size.height with
    | Some v -> Printf.sprintf "Some(%.1f)" v
    | None -> "None");

  let constants =
    ref
      {
        dir;
        is_row;
        is_column;
        is_wrap;
        is_wrap_reverse;
        min_size =
          {
            width =
              (match style.min_size.width with
              | Style.Dimension.Auto -> None
              | Style.Dimension.Length v -> Some v
              | Style.Dimension.Percent p ->
                  Option.map (fun w -> w *. p) parent_size.width);
            height =
              (match style.min_size.height with
              | Style.Dimension.Auto -> None
              | Style.Dimension.Length v -> Some v
              | Style.Dimension.Percent p ->
                  Option.map (fun h -> h *. p) parent_size.height);
          };
        max_size =
          {
            width =
              (match style.max_size.width with
              | Style.Dimension.Auto -> None
              | Style.Dimension.Length v -> Some v
              | Style.Dimension.Percent p ->
                  Option.map (fun w -> w *. p) parent_size.width);
            height =
              (match style.max_size.height with
              | Style.Dimension.Auto -> None
              | Style.Dimension.Length v -> Some v
              | Style.Dimension.Percent p ->
                  Option.map (fun h -> h *. p) parent_size.height);
          };
        margin;
        border;
        content_box_inset;
        scrollbar_gutter;
        gap =
          {
            width =
              (match style.gap.width with
              | Style.Length_percentage.Length v -> v
              | Style.Length_percentage.Percent p -> container_width *. p);
            height =
              (match style.gap.height with
              | Style.Length_percentage.Length v -> v
              | Style.Length_percentage.Percent p -> container_width *. p);
          };
        align_items =
          Option.value ~default:Style.Alignment.Stretch style.align_items;
        align_content =
          Option.value ~default:Style.Alignment.Stretch style.align_content;
        justify_content = style.justify_content;
        node_outer_size;
        node_inner_size;
        inner_container_size = size_zero;
        container_size = size_zero;
      }
  in

  (* 9. Flex Layout Algorithm *)
  (* 9.1. Initial Setup *)

  (* 1. Generate anonymous flex items as described in §4 Flex Items *)
  let flex_items =
    generate_anonymous_flex_items (module Tree) tree node !constants
  in

  (* 9.2. Line Length Determination *)

  (* 2. Determine the available main and cross space for the flex items *)
  let available_space =
    determine_available_space known_dimensions available_space !constants
  in

  (* 3. Determine the flex base size and hypothetical main size of each item *)
  determine_flex_base_size
    (module Tree)
    tree !constants available_space flex_items;

  (* 9.3. Main Size Determination *)

  (* 5. Collect flex items into flex lines *)
  let flex_lines =
    ref (collect_flex_lines !constants available_space flex_items)
  in

  (* If container size is undefined, determine the container's main size *)
  Printf.printf "node_inner_size: width=%s, height=%s\n"
    (match !constants.node_inner_size.width with
    | Some v -> Printf.sprintf "Some(%.1f)" v
    | None -> "None")
    (match !constants.node_inner_size.height with
    | Some v -> Printf.sprintf "Some(%.1f)" v
    | None -> "None");
  (* Set container sizes based on known dimensions *)
  (match !constants.node_inner_size |> main_size !constants.dir with
  | Some inner_main_size ->
      (* Container main size is defined, set it based on the known dimensions *)
      let outer_main_size =
        inner_main_size
        +. main_axis_sum !constants.dir !constants.content_box_inset
      in
      constants :=
        {
          !constants with
          inner_container_size =
            set_main_size !constants.dir !constants.inner_container_size
              inner_main_size;
          container_size =
            set_main_size !constants.dir !constants.container_size
              outer_main_size;
        }
  | None -> determine_container_main_size available_space !flex_lines constants);

  (* Also handle cross axis if known *)
  (match !constants.node_inner_size |> cross_size !constants.dir with
  | Some inner_cross_size ->
      let outer_cross_size =
        inner_cross_size
        +. cross_axis_sum !constants.dir !constants.content_box_inset
      in
      constants :=
        {
          !constants with
          inner_container_size =
            set_cross_size !constants.dir !constants.inner_container_size
              inner_cross_size;
          container_size =
            set_cross_size !constants.dir !constants.container_size
              outer_cross_size;
        }
  | None -> ());

  (* Re-resolve percentage gaps if needed *)
  (match !constants.node_inner_size |> main_size !constants.dir with
  | Some _ -> () (* Already handled *)
  | None ->
      let style = Tree.get_flexbox_container_style tree node in
      let inner_container_size =
        main_size !constants.dir !constants.inner_container_size
      in
      let new_gap =
        match main_size !constants.dir style.gap with
        | Style.Length_percentage.Length v -> v
        | Style.Length_percentage.Percent p -> inner_container_size *. p
      in
      constants :=
        {
          !constants with
          gap = set_main_size !constants.dir !constants.gap new_gap;
        });

  (* 6. Resolve the flexible lengths of all the flex items to find their used main size *)
  List.iter (fun line -> resolve_flexible_lengths line !constants) !flex_lines;

  (* 9.4. Cross Size Determination *)

  (* 7. Determine the hypothetical cross size of each item *)
  List.iter
    (fun line ->
      determine_hypothetical_cross_size
        (module Tree)
        tree line !constants available_space)
    !flex_lines;

  (* Calculate child baselines *)
  calculate_children_base_lines flex_lines !constants;

  (* 8. Calculate the cross size of each flex line *)
  calculate_cross_size flex_lines !constants;

  (* 9. Handle 'align-content: stretch' *)
  handle_align_content_stretch flex_lines known_dimensions !constants;

  (* 10. Collapse visibility:collapse items - skipped for now *)

  (* 11. Determine the used cross size of each flex item *)
  determine_used_cross_size !flex_lines !constants;

  (* 12. Main-axis alignment *)
  perform_main_alignment !flex_lines !constants;

  (* 13. Cross-axis alignment *)
  perform_cross_alignment !flex_lines !constants;

  (* 14. Resolve cross-axis auto margins *)
  resolve_cross_axis_auto_margins !flex_lines !constants;

  (* Final: determine container size and perform absolute positioning *)
  let final_size =
    determine_container_size known_dimensions !constants !flex_lines
  in
  Printf.printf "Flexbox final_size: width=%.1f, height=%.1f\n" final_size.width
    final_size.height;

  (* Perform final layout *)
  let inflow_content_size =
    perform_final_layout_and_positioning
      (module Tree)
      tree node final_size !constants !flex_lines
  in

  (* Perform absolute layout on absolutely positioned children *)
  let absolute_content_size =
    perform_absolute_layout_on_absolute_children
      (module Tree)
      tree node !constants
  in

  (* Perform hidden layout on hidden children *)
  let module TExt = Tree_intf.LayoutPartialTreeExt (Tree) in
  let child_count = Tree.child_count tree node in
  for order = 0 to child_count - 1 do
    let child_id = Tree.get_child_id tree node order in
    let child_style = Tree.get_flexbox_child_style tree child_id in
    if child_style.display = Style.None then (
      Tree.set_unrounded_layout tree child_id (Layout.Layout.with_order order);
      let _ =
        TExt.perform_child_layout tree child_id ~known_dimensions:size_none
          ~parent_size:size_none
          ~available_space:
            {
              width = Style.Available_space.Max_content;
              height = Style.Available_space.Max_content;
            }
          ~sizing_mode:Layout.Sizing_mode.Inherent_size
          ~vertical_margins_are_collapsible:line_false
      in
      ())
  done;

  (* Compute final content size *)
  (* @feature content_size *)
  let content_size =
    size_zip_map inflow_content_size absolute_content_size (fun a b ->
        Float.max a b)
  in

  (* Return the layout output *)
  Layout.Layout_output.of_sizes ~size:final_size ~content_size

(** Compute layout constants from the given style *)
(* compute_constants function was inlined into compute_preliminary *)

(** Main entry point for flexbox layout computation *)
let compute_flexbox_layout (type tree)
    (module Tree : LAYOUT_FLEXBOX_CONTAINER with type t = tree) (tree : tree)
    (node : Node.Node_id.t) (inputs : Layout.Layout_input.t) :
    Layout.Layout_output.t =
  let open Layout.Layout_input in
  let { known_dimensions; parent_size; run_mode; sizing_mode; _ } = inputs in
  let style = Tree.get_flexbox_container_style tree node in

  (* Pull these out earlier to avoid borrowing issues *)
  let aspect_ratio = style.aspect_ratio in
  let container_width = Option.value ~default:0.0 parent_size.width in

  let padding =
    rect_map style.padding (fun lp ->
        match lp with
        | Style.Length_percentage.Length v -> v
        | Style.Length_percentage.Percent p -> container_width *. p)
  in
  let border =
    rect_map style.border (fun lp ->
        match lp with
        | Style.Length_percentage.Length v -> v
        | Style.Length_percentage.Percent p -> container_width *. p)
  in
  let padding_border_sum = rect_sum_axes (rect_add padding border) in
  let box_sizing_adjustment =
    if style.box_sizing = Style.Content_box then padding_border_sum
    else size_zero
  in

  (* Resolve min/max sizes *)
  let resolve_size_with_aspect size_style =
    let base =
      {
        width =
          (match size_style.width with
          | Style.Dimension.Auto -> None
          | Style.Dimension.Length v -> Some v
          | Style.Dimension.Percent p ->
              Option.map (fun w -> w *. p) parent_size.width);
        height =
          (match size_style.height with
          | Style.Dimension.Auto -> None
          | Style.Dimension.Length v -> Some v
          | Style.Dimension.Percent p ->
              Option.map (fun h -> h *. p) parent_size.height);
      }
    in
    (* Apply aspect ratio if needed *)
    match aspect_ratio with
    | None -> base
    | Some ratio -> (
        match (base.width, base.height) with
        | Some w, None -> { base with height = Some (w /. ratio) }
        | None, Some h -> { base with width = Some (h *. ratio) }
        | _ -> base)
  in

  let min_size =
    let base = resolve_size_with_aspect style.min_size in
    Math.size_maybe_add_option_float base box_sizing_adjustment
  in
  let max_size =
    let base = resolve_size_with_aspect style.max_size in
    Math.size_maybe_add_option_float base box_sizing_adjustment
  in

  let clamped_style_size =
    if sizing_mode = Layout.Sizing_mode.Inherent_size then
      let base = resolve_size_with_aspect style.size in
      let with_adjustment =
        Math.size_maybe_add_option_float base box_sizing_adjustment
      in
      Math.size_maybe_clamp_option_option with_adjustment min_size max_size
    else size_none
  in

  (* If both min and max in a given axis are set and max <= min then this determines the size in that axis *)
  let min_max_definite_size =
    size_zip_map min_size max_size (fun min_opt max_opt ->
        match (min_opt, max_opt) with
        | Some min, Some max when max <= min -> Some min
        | _ -> None)
  in

  (* The size of the container should be floored by the padding and border *)
  let styled_based_known_dimensions =
    let base =
      Math.size_maybe_max_option_option known_dimensions
        (Math.size_maybe_max_option_option min_max_definite_size
           clamped_style_size)
    in
    let result = Math.size_maybe_max_option_float base padding_border_sum in
    Printf.printf "Flexbox styled_based_known_dimensions: width=%s, height=%s\n"
      (match result.width with
      | Some v -> Printf.sprintf "Some(%.1f)" v
      | None -> "None")
      (match result.height with
      | Some v -> Printf.sprintf "Some(%.1f)" v
      | None -> "None");
    result
  in

  (* Short-circuit layout if the container's size is fully determined by the container's size 
     and the run mode is ComputeSize (and thus the container's size is all that we're interested in) *)
  if run_mode = Layout.Run_mode.Compute_size then
    match styled_based_known_dimensions with
    | { width = Some width; height = Some height } ->
        Layout.Layout_output.of_outer_size { width; height }
    | _ ->
        (* Delegate to compute_preliminary for full layout *)
        compute_preliminary
          (module Tree)
          tree node
          { inputs with known_dimensions = styled_based_known_dimensions }
  else (
    (* Delegate to compute_preliminary for full layout *)
    Printf.printf
      "Passing known_dimensions to compute_preliminary: width=%s, height=%s\n"
      (match styled_based_known_dimensions.width with
      | Some v -> Printf.sprintf "Some(%.1f)" v
      | None -> "None")
      (match styled_based_known_dimensions.height with
      | Some v -> Printf.sprintf "Some(%.1f)" v
      | None -> "None");
    compute_preliminary
      (module Tree)
      tree node
      { inputs with known_dimensions = styled_based_known_dimensions })
