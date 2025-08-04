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
(* Use the module type from Tree_intf directly *)

(* Type aliases to match the new lowercase style *)
type 'a size = 'a Geometry.size
type 'a point = 'a Geometry.point
type 'a rect = 'a Geometry.rect

(** Create a size with only main axis set *)
let size_from_main dir value =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> { width = value; height = None }
  | Style.Flex.Column | Style.Flex.Column_reverse ->
      { width = None; height = value }

(** Get cross axis value from a point *)
let cross_point dir point =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> point.y
  | Style.Flex.Column | Style.Flex.Column_reverse -> point.x

(** Get main axis value from a point *)
let main_point dir point =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> point.x
  | Style.Flex.Column | Style.Flex.Column_reverse -> point.y

(** Get main start value from a rect *)
let main_start dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.left
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.top

(** Get main end value from a rect *)
let main_end dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.right
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.bottom

(** Get cross start value from a rect *)
let cross_start dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.top
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.left

(** Get cross end value from a rect *)
let cross_end dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.bottom
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.right

(** Get main start value from an option rect *)
let main_start_opt dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.left
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.top

(** Get main end value from an option rect *)
let main_end_opt dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.right
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.bottom

(** Get cross start value from an option rect *)
let cross_start_opt dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.top
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.left

(** Get cross end value from an option rect *)
let cross_end_opt dir rect =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> rect.bottom
  | Style.Flex.Column | Style.Flex.Column_reverse -> rect.right

(** Check if direction is row *)
let is_row dir =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> true
  | Style.Flex.Column | Style.Flex.Column_reverse -> false

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
  mutable margin : float rect;  (** The margin of this item *)
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
  is_scroll_container : bool;  (** Whether the item is a scroll container *)
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
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
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
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (node : Node.Node_id.t) (constants : algo_constants) :
    Node.Node_id.t flex_item list =
  let children = child_ids_to_list (module Tree) tree node in

  List.mapi
    (fun index child : Node.Node_id.t flex_item option ->
      let child_style = Tree.get_flexbox_child_style tree child in

      (* Extract style properties from the child style *)
      let position = child_style.position in
      let box_gen_mode = Style.box_generation_mode child_style in

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
        let is_scroll_container =
          Style.is_scroll_container child_style.overflow.x
          || Style.is_scroll_container child_style.overflow.y
        in
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
              rect_zip_size child_style.inset constants.node_inner_size
                (fun lpa parent_size_axis ->
                  match lpa with
                  | Style.Length_percentage_auto.Length v -> Some v
                  | Style.Length_percentage_auto.Percent p ->
                      Option.map (fun sz -> sz *. p) parent_size_axis
                  | Style.Length_percentage_auto.Auto -> None);
            margin =
              rect_map child_style.margin (fun lpa ->
                  match lpa with
                  | Style.Length_percentage_auto.Auto ->
                      0.0 (* Auto margins handled separately *)
                  | _ -> (
                      match lpa with
                      | Style.Length_percentage_auto.Length v -> v
                      | Style.Length_percentage_auto.Percent p ->
                          Option.value ~default:0.0
                            (Option.map (fun w -> w *. p) container_width)
                      | Style.Length_percentage_auto.Auto -> 0.0));
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
            is_scroll_container;
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
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
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
      let style_min_main_size =
        match main_size dir child.min_size with
        | Some v -> Some v
        | None ->
            (* Check if overflow creates automatic minimum size *)
            if child.is_scroll_container then Some 0.0 else None
      in

      child.resolved_minimum_main_size <-
        (match style_min_main_size with
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

      child.hypothetical_outer_size <-
        size_add child.hypothetical_inner_size
          (rect_sum_axes
             (rect_add (rect_add child.padding child.border) child.margin)))
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
    match main_axis_available_space with
    (* If sizing under max-content constraint, flex items never wrap *)
    | Style.Available_space.Max_content ->
        [ { items = flex_items; cross_size = 0.0; offset_cross = 0.0 } ]
    (* If sizing under min-content constraint, wrap at every opportunity *)
    | Style.Available_space.Min_content ->
        List.map
          (fun item ->
            { items = [ item ]; cross_size = 0.0; offset_cross = 0.0 })
          flex_items
    (* Definite space - normal wrapping logic *)
    | Style.Available_space.Definite space ->
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
              in

              let gap_penalty =
                if current_line = [] then 0.0 else main_axis_gap
              in
              let new_main_size =
                current_main_size +. item_main_size +. gap_penalty
              in

              (* Check if item fits on current line *)
              let fits = current_line = [] || new_main_size <= space in

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
  (* Gaps only exist between items, so... *)
  if num_items <= 1 then
    (* ...if there are less than 2 items then there are no gaps *)
    0.0
  else
    (* ...otherwise there are (num_items - 1) gaps *)
    gap *. float_of_int (num_items - 1)

(** Determine container main size *)
let determine_container_main_size (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (sizing_mode : Layout.Sizing_mode.t)
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
        (* First, compute content_flex_fraction for intrinsic sizing *)
        if
          sizing_mode = Layout.Sizing_mode.Content_size
          || sizing_mode = Layout.Sizing_mode.Inherent_size
        then
          List.iter
            (fun line ->
              List.iter
                (fun item ->
                  (* Compute content contribution *)
                  let content_contribution =
                    match available_space |> main_size !constants.dir with
                    | Style.Available_space.Min_content ->
                        (* For min-content, use flex basis if item is a scroll container *)
                        if item.is_scroll_container then
                          item.flex_basis
                          +. main_axis_sum !constants.dir item.margin
                        else
                          (* Otherwise compute min-content size *)
                          let child_available_space =
                            {
                              width = Style.Available_space.Min_content;
                              height = Style.Available_space.Min_content;
                            }
                            |> fun s ->
                            set_cross_size !constants.dir s
                              (cross_size !constants.dir available_space)
                          in
                          let layout_input =
                            {
                              Layout.Layout_input.run_mode =
                                Layout.Run_mode.Compute_size;
                              sizing_mode = Layout.Sizing_mode.Content_size;
                              axis = Layout.Requested_axis.Both;
                              known_dimensions = size_none;
                              parent_size = !constants.node_inner_size;
                              available_space = child_available_space;
                              vertical_margins_are_collapsible =
                                { start = false; end_ = false };
                            }
                          in
                          let layout_output =
                            Tree.compute_child_layout tree item.node
                              layout_input
                          in
                          let content_size =
                            main_size !constants.dir layout_output.size
                          in
                          let margin_sum =
                            main_axis_sum !constants.dir item.margin
                          in
                          content_size +. margin_sum
                    | Style.Available_space.Max_content ->
                        (* For max-content, use flex basis if item is a scroll container *)
                        if item.is_scroll_container then
                          item.flex_basis
                          +. main_axis_sum !constants.dir item.margin
                        else
                          (* Otherwise compute max-content size *)
                          let child_available_space =
                            {
                              width = Style.Available_space.Max_content;
                              height = Style.Available_space.Max_content;
                            }
                            |> fun s ->
                            set_cross_size !constants.dir s
                              (cross_size !constants.dir available_space)
                          in
                          let layout_input =
                            {
                              Layout.Layout_input.run_mode =
                                Layout.Run_mode.Compute_size;
                              sizing_mode = Layout.Sizing_mode.Content_size;
                              axis = Layout.Requested_axis.Both;
                              known_dimensions = size_none;
                              parent_size = !constants.node_inner_size;
                              available_space = child_available_space;
                              vertical_margins_are_collapsible =
                                { start = false; end_ = false };
                            }
                          in
                          let layout_output =
                            Tree.compute_child_layout tree item.node
                              layout_input
                          in
                          let content_size =
                            main_size !constants.dir layout_output.size
                          in
                          let margin_sum =
                            main_axis_sum !constants.dir item.margin
                          in
                          content_size +. margin_sum
                    | _ ->
                        item.flex_basis
                        +. main_axis_sum !constants.dir item.margin
                  in

                  (* Calculate content flex fraction *)
                  let diff = content_contribution -. item.flex_basis in
                  item.content_flex_fraction <-
                    (if diff > 0.0 then diff /. Float.max 1.0 item.flex_grow
                     else if diff < 0.0 then
                       let scaled_shrink_factor =
                         Float.max 1.0
                           (item.flex_shrink *. item.inner_flex_basis)
                       in
                       diff /. scaled_shrink_factor
                     else 0.0))
                line.items)
            flex_lines;

        let content_main_size =
          List.fold_left
            (fun acc line ->
              let line_main_size =
                List.fold_left
                  (fun acc item ->
                    (* When computing intrinsic sizes, use the content flex fraction *)
                    if
                      sizing_mode = Layout.Sizing_mode.Content_size
                      || sizing_mode = Layout.Sizing_mode.Inherent_size
                    then
                      let flex_contribution =
                        if item.content_flex_fraction > 0.0 then
                          Float.max 1.0 item.flex_grow
                          *. item.content_flex_fraction
                        else if item.content_flex_fraction < 0.0 then
                          let scaled_shrink_factor =
                            Float.max 1.0
                              (item.flex_shrink *. item.inner_flex_basis)
                          in
                          scaled_shrink_factor *. item.content_flex_fraction
                        else 0.0
                      in
                      let size = item.flex_basis +. flex_contribution in
                      acc +. size
                    else
                      acc
                      +. main_size !constants.dir item.hypothetical_outer_size)
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
  let total_main_axis_gap =
    sum_axis_gaps
      (main_size constants.dir constants.gap)
      (List.length line.items)
  in

  (* 1. Determine the used flex factor. Sum the outer hypothetical main sizes of all
     items on the line. If the sum is less than the flex container's inner main size,
     use the flex grow factor for the rest of this algorithm; otherwise, use the
     flex shrink factor. *)
  let total_hypothetical_outer_main_size =
    List.fold_left
      (fun acc child ->
        acc +. main_size constants.dir child.hypothetical_outer_size)
      0.0 line.items
  in

  let used_flex_factor =
    total_main_axis_gap +. total_hypothetical_outer_main_size
  in
  let growing =
    used_flex_factor
    < Option.value ~default:0.0
        (main_size constants.dir constants.node_inner_size)
  in
  let shrinking =
    used_flex_factor
    > Option.value ~default:0.0
        (main_size constants.dir constants.node_inner_size)
  in
  let exactly_sized = (not growing) && not shrinking in

  (* 2. Size inflexible items. Freeze, setting its target main size to its hypothetical main size
     - Any item that has a flex factor of zero
     - If using the flex grow factor: any item that has a flex base size
       greater than its hypothetical main size
     - If using the flex shrink factor: any item that has a flex base size
       smaller than its hypothetical main size *)
  List.iter
    (fun child ->
      let inner_target_size =
        main_size constants.dir child.hypothetical_inner_size
      in
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
        child.frozen <- true;
        let outer_target_size =
          inner_target_size +. main_axis_sum constants.dir child.margin
        in
        child.outer_target_size <-
          set_main_size constants.dir child.outer_target_size outer_target_size))
    line.items;

  if exactly_sized then ()
  else
    (* 3. Calculate initial free space. Sum the outer sizes of all items on the line,
       and subtract this from the flex container's inner main size. For frozen items,
       use their outer target main size; for other items, use their outer flex base size. *)
    let used_space =
      total_main_axis_gap
      +. List.fold_left
           (fun acc child ->
             acc
             +.
             if child.frozen then
               main_size constants.dir child.outer_target_size
             else child.flex_basis +. main_axis_sum constants.dir child.margin)
           0.0 line.items
    in

    let initial_free_space =
      Math.OptionFloat.maybe_sub
        (main_size constants.dir constants.node_inner_size)
        used_space
      |> Option.value ~default:0.0
    in

    (* 4. Loop *)
    let rec flex_loop () =
      (* a. Check for flexible items. If all the flex items on the line are frozen,
         free space has been distributed; exit this loop. *)
      if List.for_all (fun child -> child.frozen) line.items then ()
      else
        (* b. Calculate the remaining free space as for initial free space, above.
           If the sum of the unfrozen flex items' flex factors is less than one,
           multiply the initial free space by this sum. If the magnitude of this
           value is less than the magnitude of the remaining free space, use this
           as the remaining free space. *)
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

        let unfrozen = List.filter (fun child -> not child.frozen) line.items in

        let sum_flex_grow, sum_flex_shrink =
          List.fold_left
            (fun (flex_grow, flex_shrink) item ->
              (flex_grow +. item.flex_grow, flex_shrink +. item.flex_shrink))
            (0.0, 0.0) unfrozen
        in

        let free_space =
          if growing && sum_flex_grow < 1.0 then
            Math.OptionOption.maybe_min
              (Some
                 ((initial_free_space *. sum_flex_grow) -. total_main_axis_gap))
              (Math.OptionFloat.maybe_sub
                 (main_size constants.dir constants.node_inner_size)
                 used_space)
            |> Option.value ~default:0.0
          else if shrinking && sum_flex_shrink < 1.0 then
            Math.OptionOption.maybe_max
              (Some
                 ((initial_free_space *. sum_flex_shrink) -. total_main_axis_gap))
              (Math.OptionFloat.maybe_sub
                 (main_size constants.dir constants.node_inner_size)
                 used_space)
            |> Option.value ~default:0.0
          else
            Math.OptionFloat.maybe_sub
              (main_size constants.dir constants.node_inner_size)
              used_space
            |> Option.value ~default:(used_flex_factor -. used_space)
        in

        (* c. Distribute free space proportional to the flex factors.
           - If the remaining free space is zero
               Do Nothing
           - If using the flex grow factor
               Find the ratio of the item's flex grow factor to the sum of the
               flex grow factors of all unfrozen items on the line. Set the item's
               target main size to its flex base size plus a fraction of the remaining
               free space proportional to the ratio.
           - If using the flex shrink factor
               For every unfrozen item on the line, multiply its flex shrink factor by
               its inner flex base size, and note this as its scaled flex shrink factor.
               Find the ratio of the item's scaled flex shrink factor to the sum of the
               scaled flex shrink factors of all unfrozen items on the line. Set the item's
               target main size to its flex base size minus a fraction of the absolute value
               of the remaining free space proportional to the ratio. Note this may result
               in a negative inner main size; it will be corrected in the next step.
           - Otherwise
               Do Nothing *)
        if Float.is_finite free_space then
          if growing && sum_flex_grow > 0.0 then
            List.iter
              (fun child ->
                if not child.frozen then
                  child.target_size <-
                    set_main_size constants.dir child.target_size
                      (child.flex_basis
                      +. (free_space *. (child.flex_grow /. sum_flex_grow))))
              line.items
          else if shrinking && sum_flex_shrink > 0.0 then (
            let sum_scaled_shrink_factor =
              List.fold_left
                (fun acc child ->
                  if child.frozen then acc
                  else acc +. (child.inner_flex_basis *. child.flex_shrink))
                0.0 line.items
            in
            if sum_scaled_shrink_factor > 0.0 then
              List.iter
                (fun child ->
                  if not child.frozen then
                    let scaled_shrink_factor =
                      child.inner_flex_basis *. child.flex_shrink
                    in
                    child.target_size <-
                      set_main_size constants.dir child.target_size
                        (child.flex_basis
                        +. free_space
                           *. (scaled_shrink_factor /. sum_scaled_shrink_factor)
                        ))
                line.items;

            (* d. Fix min/max violations. Clamp each non-frozen item's target main size by its
           used min and max main sizes and floor its content-box size at zero. If the
           item's target main size was made smaller by this, it's a max violation.
           If the item's target main size was made larger by this, it's a min violation. *)
            let total_violation =
              List.fold_left
                (fun acc child ->
                  if child.frozen then acc
                  else
                    let resolved_min_main =
                      Some child.resolved_minimum_main_size
                    in
                    let max_main = main_size constants.dir child.max_size in
                    let clamped =
                      Math.FloatOption.maybe_clamp
                        (main_size constants.dir child.target_size)
                        resolved_min_main max_main
                      |> Float.max 0.0
                    in
                    child.violation <-
                      clamped -. main_size constants.dir child.target_size;
                    child.target_size <-
                      set_main_size constants.dir child.target_size clamped;
                    child.outer_target_size <-
                      set_main_size constants.dir child.outer_target_size
                        (clamped +. main_axis_sum constants.dir child.margin);
                    acc +. child.violation)
                0.0 line.items
            in

            (* e. Freeze over-flexed items. The total violation is the sum of the adjustments
           from the previous step ∑(clamped size - unclamped size). If the total violation is:
           - Zero
               Freeze all items.
           - Positive
               Freeze all the items with min violations.
           - Negative
               Freeze all the items with max violations. *)
            List.iter
              (fun child ->
                if not child.frozen then
                  child.frozen <-
                    (if total_violation > 0.0 then child.violation > 0.0
                     else if total_violation < 0.0 then child.violation < 0.0
                     else true))
              line.items;

            (* f. Return to the start of this loop. *)
            flex_loop ())
    in

    flex_loop ()

(** Determine the hypothetical cross size of each item.

    #
    [9.4. Cross Size Determination](https://www.w3.org/TR/css-flexbox-1/#cross-sizing)

    - [**Determine the hypothetical cross size of each item**](https://www.w3.org/TR/css-flexbox-1/#algo-cross-item)
      by performing layout with the used main size and the available space,
      treating auto as fit-content. *)
let determine_hypothetical_cross_size (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (line : Node.Node_id.t flex_line) (constants : algo_constants)
    (available_space : Style.Available_space.t size) : unit =
  List.iter
    (fun child ->
      let padding_border_sum =
        rect_add child.padding child.border |> fun r ->
        cross_axis_sum constants.dir r
      in

      let child_known_main =
        main_size constants.dir constants.container_size |> Option.some
      in

      let child_cross =
        child.size |> cross_size constants.dir |> fun c ->
        Math.OptionOption.maybe_clamp c
          (cross_size constants.dir child.min_size)
          (cross_size constants.dir child.max_size)
        |> fun c -> Math.OptionFloat.maybe_max c padding_border_sum
      in

      let child_available_cross =
        available_space |> cross_size constants.dir |> fun ac ->
        Math.AvailableSpaceOption.maybe_clamp ac
          (cross_size constants.dir child.min_size)
          (cross_size constants.dir child.max_size)
        |> fun ac -> Math.AvailableSpaceFloat.maybe_max ac padding_border_sum
      in

      let child_inner_cross =
        match child_cross with
        | Some cc -> cc
        | None ->
            (* Need to measure child size *)
            let known_dimensions =
              {
                width =
                  (if constants.is_row then
                     Some (main_size constants.dir child.target_size)
                   else child_cross);
                height =
                  (if constants.is_row then child_cross
                   else Some (main_size constants.dir child.target_size));
              }
            in
            let parent_size = constants.node_inner_size in
            let avail_space =
              {
                width =
                  (if constants.is_row then
                     match child_known_main with
                     | Some v -> Style.Available_space.Definite v
                     | None -> Style.Available_space.Max_content
                   else child_available_cross);
                height =
                  (if constants.is_row then child_available_cross
                   else
                     match child_known_main with
                     | Some v -> Style.Available_space.Definite v
                     | None -> Style.Available_space.Max_content);
              }
            in

            (* Create proper module for measure_child_size *)
            let module TreeExt = Tree_intf.LayoutPartialTreeExt (Tree) in
            TreeExt.measure_child_size tree child.node ~known_dimensions
              ~parent_size ~available_space:avail_space
              ~sizing_mode:Layout.Sizing_mode.Content_size
              ~axis:
                (match constants.dir with
                | Style.Flex.Row | Style.Flex.Row_reverse -> Geometry.Vertical
                | Style.Flex.Column | Style.Flex.Column_reverse ->
                    Geometry.Horizontal)
              ~vertical_margins_are_collapsible:{ start = false; end_ = false }
            |> fun measured ->
            Math.FloatOption.maybe_clamp measured
              (cross_size constants.dir child.min_size)
              (cross_size constants.dir child.max_size)
            |> Float.max padding_border_sum
      in

      let child_outer_cross =
        child_inner_cross +. cross_axis_sum constants.dir child.margin
      in

      child.hypothetical_inner_size <-
        set_cross_size constants.dir child.hypothetical_inner_size
          child_inner_cross;
      child.hypothetical_outer_size <-
        set_cross_size constants.dir child.hypothetical_outer_size
          child_outer_cross)
    line.items

(** Helper functions for auto margin checks *)
let main_start_is_auto dir margin_is_auto =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> margin_is_auto.left
  | Style.Flex.Column | Style.Flex.Column_reverse -> margin_is_auto.top

let main_end_is_auto dir margin_is_auto =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> margin_is_auto.right
  | Style.Flex.Column | Style.Flex.Column_reverse -> margin_is_auto.bottom

let cross_start_is_auto dir margin_is_auto =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> margin_is_auto.top
  | Style.Flex.Column | Style.Flex.Column_reverse -> margin_is_auto.left

let cross_end_is_auto dir margin_is_auto =
  match dir with
  | Style.Flex.Row | Style.Flex.Row_reverse -> margin_is_auto.bottom
  | Style.Flex.Column | Style.Flex.Column_reverse -> margin_is_auto.right

(** Calculate the base lines of the children. *)
let calculate_children_base_lines (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (node_size : float option size)
    (available_space : Style.Available_space.t size)
    (flex_lines : 'a flex_line list ref) (constants : algo_constants) : unit =
  (* Only compute baselines for flex rows because we only support baseline alignment in the cross axis
     where that axis is also the inline axis
     TODO: this may need revisiting if/when we support vertical writing modes *)
  if not constants.is_row then ()
  else
    List.iter
      (fun line ->
        (* If a flex line has one or zero items participating in baseline alignment then baseline alignment is a no-op so we skip *)
        let line_baseline_child_count =
          List.length
            (List.filter
               (fun child -> child.align_self = Style.Alignment.Baseline)
               line.items)
        in
        if line_baseline_child_count <= 1 then ()
        else
          List.iter
            (fun child ->
              (* Only calculate baselines for children participating in baseline alignment *)
              if child.align_self = Style.Alignment.Baseline then
                let known_dimensions =
                  {
                    width =
                      (if constants.is_row then
                         Some (main_size constants.dir child.target_size)
                       else
                         Some
                           (cross_size constants.dir
                              child.hypothetical_inner_size));
                    height =
                      (if constants.is_row then
                         Some
                           (cross_size constants.dir
                              child.hypothetical_inner_size)
                       else Some (main_size constants.dir child.target_size));
                  }
                in
                let parent_size = constants.node_inner_size in
                let avail_space =
                  {
                    width =
                      (if constants.is_row then
                         match
                           main_size constants.dir constants.container_size
                         with
                         | v -> Style.Available_space.Definite v
                       else
                         Math.AvailableSpaceOption.maybe_set
                           available_space.width node_size.width);
                    height =
                      (if constants.is_row then
                         Math.AvailableSpaceOption.maybe_set
                           available_space.height node_size.height
                       else
                         match
                           main_size constants.dir constants.container_size
                         with
                         | v -> Style.Available_space.Definite v);
                  }
                in

                (* Create proper module for perform_child_layout *)
                let module TreeExt = Tree_intf.LayoutPartialTreeExt (Tree) in
                let measured_size_and_baselines =
                  TreeExt.perform_child_layout tree child.node ~known_dimensions
                    ~parent_size ~available_space:avail_space
                    ~sizing_mode:Layout.Sizing_mode.Content_size
                    ~vertical_margins_are_collapsible:
                      { start = false; end_ = false }
                in

                let baseline = measured_size_and_baselines.first_baselines.y in
                let height = measured_size_and_baselines.size.height in

                child.baseline <-
                  Option.value ~default:height baseline +. child.margin.top)
            line.items)
      !flex_lines

(** Calculate the cross size of each flex line.

    #
    [9.4. Cross Size Determination](https://www.w3.org/TR/css-flexbox-1/#cross-sizing)

    - [**Calculate the cross size of each flex line**](https://www.w3.org/TR/css-flexbox-1/#algo-cross-line).
*)
let calculate_cross_size (flex_lines : 'a flex_line list ref)
    (node_size : float option size) (constants : algo_constants) : unit =
  (* If the flex container is single-line and has a definite cross size,
     the cross size of the flex line is the flex container's inner cross size. *)
  if
    (not constants.is_wrap)
    && Option.is_some (cross_size constants.dir node_size)
  then
    let cross_axis_padding_border =
      cross_axis_sum constants.dir constants.content_box_inset
    in
    let cross_min_size = cross_size constants.dir constants.min_size in
    let cross_max_size = cross_size constants.dir constants.max_size in
    match !flex_lines with
    | line :: _ ->
        line.cross_size <-
          ( cross_size constants.dir node_size |> fun c ->
            Math.OptionOption.maybe_clamp c cross_min_size cross_max_size
            |> fun c ->
            Math.OptionFloat.maybe_sub c cross_axis_padding_border |> fun c ->
            Math.OptionFloat.maybe_max c 0.0 |> Option.value ~default:0.0 )
    | [] -> ()
  else
    (* Otherwise, for each flex line:
       
       1. Collect all the flex items whose inline-axis is parallel to the main-axis, whose
          align-self is baseline, and whose cross-axis margins are both non-auto. Find the
          largest of the distances between each item's baseline and its hypothetical outer
          cross-start edge, and the largest of the distances between each item's baseline
          and its hypothetical outer cross-end edge, and sum these two values.
          
       2. Among all the items not collected by the previous step, find the largest
          outer hypothetical cross size.
          
       3. The used cross-size of the flex line is the largest of the numbers found in the
          previous two steps and zero. *)
    List.iter
      (fun line ->
        let max_baseline =
          List.fold_left
            (fun acc child -> Float.max acc child.baseline)
            0.0 line.items
        in
        line.cross_size <-
          List.fold_left
            (fun acc child ->
              let cross_val =
                if
                  child.align_self = Style.Alignment.Baseline
                  && (not
                        (cross_start_is_auto constants.dir child.margin_is_auto))
                  && not (cross_end_is_auto constants.dir child.margin_is_auto)
                then
                  max_baseline -. child.baseline
                  +. cross_size constants.dir child.hypothetical_outer_size
                else cross_size constants.dir child.hypothetical_outer_size
              in
              Float.max acc cross_val)
            0.0 line.items)
      !flex_lines;

  (* If the flex container is single-line, then clamp the line's cross-size to be within the container's computed min and max cross sizes.
       Note that if CSS 2.1's definition of min/max-width/height applied more generally, this behavior would fall out automatically. *)
  if not constants.is_wrap then
    let cross_axis_padding_border =
      cross_axis_sum constants.dir constants.content_box_inset
    in
    let cross_min_size = cross_size constants.dir constants.min_size in
    let cross_max_size = cross_size constants.dir constants.max_size in
    match !flex_lines with
    | line :: _ ->
        line.cross_size <-
          Math.FloatOption.maybe_clamp line.cross_size
            (Math.OptionFloat.maybe_sub cross_min_size cross_axis_padding_border)
            (Math.OptionFloat.maybe_sub cross_max_size cross_axis_padding_border)
    | [] -> ()

(** Handle 'align-content: stretch'.

    #
    [9.4. Cross Size Determination](https://www.w3.org/TR/css-flexbox-1/#cross-sizing)

    - [**Handle 'align-content: stretch'**](https://www.w3.org/TR/css-flexbox-1/#algo-line-stretch).
      If the flex container has a definite cross size, align-content is stretch,
      and the sum of the flex lines' cross sizes is less than the flex
      container's inner cross size, increase the cross size of each flex line by
      equal amounts such that the sum of their cross sizes exactly equals the
      flex container's inner cross size. *)
let handle_align_content_stretch (flex_lines : 'a flex_line list ref)
    (node_size : float option size) (constants : algo_constants) : unit =
  if constants.align_content = Style.Alignment.Stretch then
    let cross_axis_padding_border =
      cross_axis_sum constants.dir constants.content_box_inset
    in
    let cross_min_size = cross_size constants.dir constants.min_size in
    let cross_max_size = cross_size constants.dir constants.max_size in
    let container_min_inner_cross =
      (match cross_size constants.dir node_size with
      | Some v -> Some v
      | None -> cross_min_size)
      |> fun c ->
      Math.OptionOption.maybe_clamp c cross_min_size cross_max_size |> fun c ->
      Math.OptionFloat.maybe_sub c cross_axis_padding_border |> fun c ->
      Math.OptionFloat.maybe_max c 0.0 |> Option.value ~default:0.0
    in

    let total_cross_axis_gap =
      sum_axis_gaps
        (cross_size constants.dir constants.gap)
        (List.length !flex_lines)
    in
    let lines_total_cross =
      List.fold_left (fun acc line -> acc +. line.cross_size) 0.0 !flex_lines
      +. total_cross_axis_gap
    in

    if lines_total_cross < container_min_inner_cross then
      let remaining = container_min_inner_cross -. lines_total_cross in
      let addition = remaining /. float_of_int (List.length !flex_lines) in
      List.iter
        (fun line -> line.cross_size <- line.cross_size +. addition)
        !flex_lines

(** Determine the used cross size of each flex item.

    #
    [9.4. Cross Size Determination](https://www.w3.org/TR/css-flexbox-1/#cross-sizing)

    - [**Determine the used cross size of each flex item**](https://www.w3.org/TR/css-flexbox-1/#algo-stretch).
      If a flex item has align-self: stretch, its computed cross size property
      is auto, and neither of its cross-axis margins are auto, the used outer
      cross size is the used cross size of its flex line, clamped according to
      the item's used min and max cross sizes. Otherwise, the used cross size is
      the item's hypothetical cross size.

    If the flex item has align-self: stretch, redo layout for its contents,
    treating this used size as its definite cross size so that percentage-sized
    children can be resolved.

    **Note that this step does not affect the main size of the flex item, even
    if it has an intrinsic aspect ratio**. *)
let determine_used_cross_size (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (flex_lines : 'a flex_line list) (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      let line_cross_size = line.cross_size in

      List.iter
        (fun child ->
          let child_style = Tree.get_flexbox_child_style tree child.node in
          child.target_size <-
            set_cross_size constants.dir child.target_size
              (if
                 child.align_self = Style.Alignment.Stretch
                 && (not
                       (cross_start_is_auto constants.dir child.margin_is_auto))
                 && (not (cross_end_is_auto constants.dir child.margin_is_auto))
                 &&
                 match cross_size constants.dir child_style.size with
                 | Style.Dimension.Auto -> true
                 | _ -> false
               then
                 (* For some reason this particular usage of max_width is an exception to the rule that max_width's transfer
                    using the aspect_ratio (if set). Both Chrome and Firefox agree on this. And reading the spec, it seems like
                    a reasonable interpretation. Although it seems to me that the spec *should* apply aspect_ratio here. *)
                 let padding =
                   rect_map child_style.padding (fun lp ->
                       match lp with
                       | Style.Length_percentage.Length v -> v
                       | Style.Length_percentage.Percent p ->
                           Option.value ~default:0.0
                             (Option.map
                                (fun w -> w *. p)
                                (main_size constants.dir
                                   constants.node_inner_size)))
                 in
                 let border =
                   rect_map child_style.border (fun lp ->
                       match lp with
                       | Style.Length_percentage.Length v -> v
                       | Style.Length_percentage.Percent p ->
                           Option.value ~default:0.0
                             (Option.map
                                (fun w -> w *. p)
                                (main_size constants.dir
                                   constants.node_inner_size)))
                 in
                 let pb_sum = rect_sum_axes (rect_add padding border) in
                 let box_sizing_adjustment =
                   if child_style.box_sizing = Style.Content_box then pb_sum
                   else { width = 0.0; height = 0.0 }
                 in

                 let max_size_ignoring_aspect_ratio =
                   size_map child_style.max_size (fun dim ->
                       match dim with
                       | Style.Dimension.Auto -> None
                       | Style.Dimension.Length v -> Some v
                       | Style.Dimension.Percent p ->
                           Option.map
                             (fun sz -> sz *. p)
                             (main_size constants.dir constants.node_inner_size))
                   |> fun s ->
                   Math.size_maybe_add_option_float s box_sizing_adjustment
                 in

                 Math.FloatOption.maybe_clamp
                   (line_cross_size -. cross_axis_sum constants.dir child.margin)
                   (cross_size constants.dir child.min_size)
                   (cross_size constants.dir max_size_ignoring_aspect_ratio)
               else cross_size constants.dir child.hypothetical_inner_size);

          child.outer_target_size <-
            set_cross_size constants.dir child.outer_target_size
              (cross_size constants.dir child.target_size
              +. cross_axis_sum constants.dir child.margin))
        line.items)
    flex_lines

(** Distribute remaining free space and perform main axis alignment

    #
    [9.5. Main-Axis Alignment](https://www.w3.org/TR/css-flexbox-1/#main-alignment)

    Now that we've finished with the flex items & flex lines, we have to go back
    to the flex container.

    1. If a flex item has auto margins then resolve them now. Otherwise, set all
    `auto` margins to zero.

    2. Align the items along the main-axis per `justify-content`. *)
let distribute_remaining_free_space (flex_lines : 'a flex_line list)
    (constants : algo_constants) : unit =
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

      let num_auto_margins = ref 0 in

      List.iter
        (fun child ->
          if main_start_is_auto constants.dir child.margin_is_auto then
            incr num_auto_margins;
          if main_end_is_auto constants.dir child.margin_is_auto then
            incr num_auto_margins)
        line.items;

      if free_space > 0.0 && !num_auto_margins > 0 then
        let margin = free_space /. float_of_int !num_auto_margins in

        List.iter
          (fun child ->
            if main_start_is_auto constants.dir child.margin_is_auto then
              if constants.is_row then
                child.margin <- { child.margin with left = margin }
              else child.margin <- { child.margin with top = margin };
            if main_end_is_auto constants.dir child.margin_is_auto then
              if constants.is_row then
                child.margin <- { child.margin with right = margin }
              else child.margin <- { child.margin with bottom = margin })
          line.items
      else
        let num_items = List.length line.items in
        let layout_reverse = Style.Flex.is_reverse constants.dir in
        let gap = main_size constants.dir constants.gap in
        let is_safe = false in
        (* TODO: Implement safe alignment *)
        let raw_justify_content_mode =
          Option.value ~default:Style.Alignment.Flex_start
            constants.justify_content
        in
        let justify_content_mode =
          Alignment.apply_alignment_fallback ~free_space ~num_items
            ~alignment_mode:raw_justify_content_mode ~is_safe
        in

        let justify_item (i, child) =
          child.offset_main <-
            Alignment.compute_alignment_offset ~free_space ~num_items ~gap
              ~alignment_mode:justify_content_mode
              ~layout_is_flex_reversed:layout_reverse ~is_first:(i = 0)
        in

        if layout_reverse then
          List.iteri
            (fun i child -> justify_item (i, child))
            (List.rev line.items)
        else List.iteri (fun i child -> justify_item (i, child)) line.items)
    flex_lines

(** Align all flex lines per `align-content`.

    #
    [9.6. Cross-Axis Alignment](https://www.w3.org/TR/css-flexbox-1/#cross-alignment)

    - [**Align all flex lines**](https://www.w3.org/TR/css-flexbox-1/#algo-line-align)
      per `align-content`. *)
let align_flex_lines_per_align_content (flex_lines : 'a flex_line list ref)
    (constants : algo_constants) (total_cross_size : float) : unit =
  let num_lines = List.length !flex_lines in
  let gap = cross_size constants.dir constants.gap in
  let total_cross_axis_gap = sum_axis_gaps gap num_lines in
  let free_space =
    cross_size constants.dir constants.inner_container_size
    -. total_cross_size -. total_cross_axis_gap
  in
  let is_safe = false in
  (* TODO: Implement safe alignment *)

  let align_content_mode =
    Alignment.apply_alignment_fallback ~free_space ~num_items:num_lines
      ~alignment_mode:constants.align_content ~is_safe
  in

  let align_line (i, line) =
    line.offset_cross <-
      Alignment.compute_alignment_offset ~free_space ~num_items:num_lines ~gap
        ~alignment_mode:align_content_mode
        ~layout_is_flex_reversed:constants.is_wrap_reverse ~is_first:(i = 0)
  in

  if constants.is_wrap_reverse then
    List.iteri (fun i line -> align_line (i, line)) (List.rev !flex_lines)
  else List.iteri (fun i line -> align_line (i, line)) !flex_lines

(** Align all flex items along the cross-axis.

    #
    [9.6. Cross-Axis Alignment](https://www.w3.org/TR/css-flexbox-1/#cross-alignment)

    - [**Align all flex items along the cross-axis**](https://www.w3.org/TR/css-flexbox-1/#algo-cross-align)
      per `align-self`, if neither of the item's cross-axis margins are `auto`.
*)
let align_flex_items_along_cross_axis (child : 'a flex_item)
    (free_space : float) (max_baseline : float) (constants : algo_constants) :
    float =
  match child.align_self with
  | Style.Alignment.Start -> 0.0
  | Style.Alignment.Flex_start ->
      if constants.is_wrap_reverse then free_space else 0.0
  | Style.Alignment.End -> free_space
  | Style.Alignment.Flex_end ->
      if constants.is_wrap_reverse then 0.0 else free_space
  | Style.Alignment.Center -> free_space /. 2.0
  | Style.Alignment.Baseline ->
      if constants.is_row then max_baseline -. child.baseline
      else if
        (* Until we support vertical writing modes, baseline alignment only makes sense if
           the constants.direction is row, so we treat it as flex-start alignment in columns. *)
        constants.is_wrap_reverse
      then free_space
      else 0.0
  | Style.Alignment.Stretch ->
      if constants.is_wrap_reverse then free_space else 0.0

(** Resolve cross-axis `auto` margins.

    #
    [9.6. Cross-Axis Alignment](https://www.w3.org/TR/css-flexbox-1/#cross-alignment)

    - [**Resolve cross-axis `auto` margins**](https://www.w3.org/TR/css-flexbox-1/#algo-cross-margins).
      If a flex item has auto cross-axis margins:

    - If its outer cross size (treating those auto margins as zero) is less than
      the cross size of its flex line, distribute the difference in those sizes
      equally to the auto margins.

    - Otherwise, if the block-start or inline-start margin (whichever is in the
      cross axis) is auto, set it to zero. Set the opposite margin so that the
      outer cross size of the item equals the cross size of its flex line. *)
let resolve_cross_axis_auto_margins (flex_lines : 'a flex_line list)
    (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      let line_cross_size = line.cross_size in
      let max_baseline =
        List.fold_left
          (fun acc child -> Float.max acc child.baseline)
          0.0 line.items
      in

      List.iter
        (fun child ->
          let free_space =
            line_cross_size -. cross_size constants.dir child.outer_target_size
          in

          if
            cross_start_is_auto constants.dir child.margin_is_auto
            && cross_end_is_auto constants.dir child.margin_is_auto
          then
            if constants.is_row then (
              child.margin <- { child.margin with top = free_space /. 2.0 };
              child.margin <- { child.margin with bottom = free_space /. 2.0 })
            else (
              child.margin <- { child.margin with left = free_space /. 2.0 };
              child.margin <- { child.margin with right = free_space /. 2.0 })
          else if cross_start_is_auto constants.dir child.margin_is_auto then
            if constants.is_row then
              child.margin <- { child.margin with top = free_space }
            else child.margin <- { child.margin with left = free_space }
          else if cross_end_is_auto constants.dir child.margin_is_auto then
            if constants.is_row then
              child.margin <- { child.margin with bottom = free_space }
            else child.margin <- { child.margin with right = free_space }
          else
            (* 14. Align all flex items along the cross-axis. *)
            child.offset_cross <-
              align_flex_items_along_cross_axis child free_space max_baseline
                constants)
        line.items)
    flex_lines

(** Determine the flex container's used cross size.

    #
    [9.6. Cross-Axis Alignment](https://www.w3.org/TR/css-flexbox-1/#cross-alignment)

    - [**Determine the flex container's used cross size**](https://www.w3.org/TR/css-flexbox-1/#algo-cross-container):

    - If the cross size property is a definite size, use that, clamped by the
      used min and max cross sizes of the flex container.

    - Otherwise, use the sum of the flex lines' cross sizes, clamped by the used
      min and max cross sizes of the flex container. *)
let determine_container_cross_size (flex_lines : 'a flex_line list)
    (node_size : float option size) (constants : algo_constants ref) : float =
  let total_cross_axis_gap =
    sum_axis_gaps
      (cross_size !constants.dir !constants.gap)
      (List.length flex_lines)
  in
  let total_line_cross_size =
    List.fold_left (fun acc line -> acc +. line.cross_size) 0.0 flex_lines
  in

  let padding_border_sum =
    cross_axis_sum !constants.dir !constants.content_box_inset
  in
  let cross_scrollbar_gutter =
    cross_point !constants.dir !constants.scrollbar_gutter
  in
  let min_cross_size = cross_size !constants.dir !constants.min_size in
  let max_cross_size = cross_size !constants.dir !constants.max_size in
  let outer_container_size =
    Option.value
      ~default:
        (total_line_cross_size +. total_cross_axis_gap +. padding_border_sum)
      (cross_size !constants.dir node_size)
    |> fun sz ->
    Math.FloatOption.maybe_clamp sz min_cross_size max_cross_size
    |> Float.max (padding_border_sum -. cross_scrollbar_gutter)
  in
  let inner_container_size =
    Float.max (outer_container_size -. padding_border_sum) 0.0
  in

  constants :=
    {
      !constants with
      container_size =
        set_cross_size !constants.dir !constants.container_size
          outer_container_size;
      inner_container_size =
        set_cross_size !constants.dir !constants.inner_container_size
          inner_container_size;
    };

  total_line_cross_size

(** Calculate layout for a single flex item *)
let calculate_flex_item (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (item : Node.Node_id.t flex_item) (total_offset_main : float ref)
    (total_offset_cross : float) (line_offset_cross : float)
    (total_content_size : float size ref) (container_size : float size)
    (node_inner_size : float option size)
    (direction : Style.Flex.flex_direction) : unit =
  let module TExt = Tree_intf.LayoutPartialTreeExt (Tree) in
  (* Perform child layout *)
  let layout_output =
    TExt.perform_child_layout tree item.node
      ~known_dimensions:(size_map item.target_size (fun s -> Some s))
      ~parent_size:node_inner_size
      ~available_space:
        (size_map container_size (fun s -> Style.Available_space.Definite s))
      ~sizing_mode:Layout.Sizing_mode.Content_size
      ~vertical_margins_are_collapsible:line_false
  in

  let size = layout_output.size in

  (* Calculate offsets *)
  let offset_main =
    !total_offset_main +. item.offset_main
    +. main_start direction item.margin
    +.
    match
      (main_start_opt direction item.inset, main_end_opt direction item.inset)
    with
    | Some v, _ -> v
    | None, Some v -> -.v
    | None, None -> 0.0
  in

  let offset_cross =
    total_offset_cross +. item.offset_cross +. line_offset_cross
    +. cross_start direction item.margin
    +.
    match
      (cross_start_opt direction item.inset, cross_end_opt direction item.inset)
    with
    | Some v, _ -> v
    | None, Some v -> -.v
    | None, None -> 0.0
  in

  (* Update baseline *)
  (if is_row direction then
     let baseline_offset_cross =
       total_offset_cross +. item.offset_cross
       +. cross_start direction item.margin
     in
     let inner_baseline =
       Option.value ~default:size.height layout_output.first_baselines.y
     in
     item.baseline <- baseline_offset_cross +. inner_baseline
   else
     let baseline_offset_main =
       !total_offset_main +. item.offset_main
       +. main_start direction item.margin
     in
     let inner_baseline =
       Option.value ~default:size.height layout_output.first_baselines.y
     in
     item.baseline <- baseline_offset_main +. inner_baseline);

  (* Calculate location *)
  let location =
    if is_row direction then { x = offset_main; y = offset_cross }
    else { x = offset_cross; y = offset_main }
  in

  (* Calculate scrollbar size *)
  let scrollbar_size =
    {
      width =
        (if item.overflow.y = Style.Scroll then item.scrollbar_width else 0.0);
      height =
        (if item.overflow.x = Style.Scroll then item.scrollbar_width else 0.0);
    }
  in

  (* Set layout *)
  Tree.set_unrounded_layout tree item.node
    {
      Layout.Layout.empty with
      order = item.order;
      size;
      scrollbar_size;
      location;
      padding = item.padding;
      border = item.border;
      margin = item.margin;
    };

  (* Update total offset main *)
  total_offset_main :=
    !total_offset_main +. item.offset_main
    +. main_axis_sum direction item.margin
    +. main_size direction size;

  (* Update content size *)
  total_content_size :=
    size_zip_map !total_content_size
      (Content_size.compute_content_size_contribution ~location ~size
         ~content_size:layout_output.size ~overflow:item.overflow) (fun a b ->
        Float.max a b)

(** Calculate layout for a flex line *)
let calculate_layout_line (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (line : Node.Node_id.t flex_line) (total_offset_cross : float ref)
    (content_size : float size ref) (container_size : float size)
    (node_inner_size : float option size) (padding_border : float rect)
    (direction : Style.Flex.flex_direction) : unit =
  let total_offset_main = ref (main_start direction padding_border) in
  let line_offset_cross = line.offset_cross in

  if Style.Flex.is_reverse direction then
    List.iter
      (fun item ->
        calculate_flex_item
          (module Tree)
          tree item total_offset_main !total_offset_cross line_offset_cross
          content_size container_size node_inner_size direction)
      (List.rev line.items)
  else
    List.iter
      (fun item ->
        calculate_flex_item
          (module Tree)
          tree item total_offset_main !total_offset_cross line_offset_cross
          content_size container_size node_inner_size direction)
      line.items

(** Perform final layout and positioning *)
let perform_final_layout_and_positioning (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
    (_node : Node.Node_id.t) (_container_size : float size)
    (constants : algo_constants) (flex_lines : Node.Node_id.t flex_line list) :
    float size =
  (* Track content size *)
  let content_size = ref size_zero in
  let total_offset_cross =
    ref (cross_start constants.dir constants.content_box_inset)
  in

  (* Process lines in correct order based on wrap-reverse *)
  let lines_to_process =
    if constants.is_wrap_reverse then List.rev flex_lines else flex_lines
  in

  List.iter
    (fun line ->
      calculate_layout_line
        (module Tree)
        tree line total_offset_cross content_size constants.container_size
        constants.node_inner_size constants.content_box_inset constants.dir;

      (* Update total cross offset *)
      total_offset_cross :=
        !total_offset_cross +. line.cross_size
        +. cross_size constants.dir constants.gap)
    lines_to_process;

  (* Adjust content size as in Rust *)
  content_size :=
    {
      width =
        !content_size.width +. constants.content_box_inset.right
        -. constants.border.right -. constants.scrollbar_gutter.x;
      height =
        !content_size.height +. constants.content_box_inset.bottom
        -. constants.border.bottom -. constants.scrollbar_gutter.y;
    };

  !content_size

(** Perform absolute layout on absolutely positioned children *)
let perform_absolute_layout_on_absolute_children (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
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
        {
          left =
            (match child_style.inset.left with
            | Style.Length_percentage_auto.Auto -> None
            | Style.Length_percentage_auto.Length v -> Some v
            | Style.Length_percentage_auto.Percent p ->
                Option.map (fun s -> s *. p) constants.node_inner_size.width);
          right =
            (match child_style.inset.right with
            | Style.Length_percentage_auto.Auto -> None
            | Style.Length_percentage_auto.Length v -> Some v
            | Style.Length_percentage_auto.Percent p ->
                Option.map (fun s -> s *. p) constants.node_inner_size.width);
          top =
            (match child_style.inset.top with
            | Style.Length_percentage_auto.Auto -> None
            | Style.Length_percentage_auto.Length v -> Some v
            | Style.Length_percentage_auto.Percent p ->
                Option.map (fun s -> s *. p) constants.node_inner_size.height);
          bottom =
            (match child_style.inset.bottom with
            | Style.Length_percentage_auto.Auto -> None
            | Style.Length_percentage_auto.Length v -> Some v
            | Style.Length_percentage_auto.Percent p ->
                Option.map (fun s -> s *. p) constants.node_inner_size.height);
        }
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
      let mut_known_dimensions =
        ref
          ( {
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
                Option.map (fun s -> s +. adj) sz) )
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
        |> apply_aspect_ratio_to_size
        |> fun size ->
        size_zip_map size box_sizing_adjustment (fun sz adj ->
            Option.map (fun s -> s +. adj) sz)
        |> fun size ->
        {
          width =
            (match size.width with
            | None -> Some padding_border_sum.width
            | Some w -> Some (Float.max w padding_border_sum.width));
          height =
            (match size.height with
            | None -> Some padding_border_sum.height
            | Some h -> Some (Float.max h padding_border_sum.height));
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
        |> apply_aspect_ratio_to_size
        |> fun size ->
        size_zip_map size box_sizing_adjustment (fun sz adj ->
            Option.map (fun s -> s +. adj) sz)
      in

      (* Apply clamping after width/height calculation from insets *)
      (match (!mut_known_dimensions.width, inset.left, inset.right) with
      | None, Some left, Some right ->
          let new_width_raw =
            Option.value ~default:0.0 parent_size.width
            -. Option.value ~default:0.0 margin.left
            -. Option.value ~default:0.0 margin.right
            -. left -. right
          in
          mut_known_dimensions :=
            {
              !mut_known_dimensions with
              width = Some (Float.max new_width_raw 0.0);
            }
            |> apply_aspect_ratio_to_size
            |> fun size -> Size.maybe_clamp size min_size max_size
      | _ -> ());

      (match (!mut_known_dimensions.height, inset.top, inset.bottom) with
      | None, Some top, Some bottom ->
          let new_height_raw =
            Option.value ~default:0.0 parent_size.height
            -. Option.value ~default:0.0 margin.top
            -. Option.value ~default:0.0 margin.bottom
            -. top -. bottom
          in
          mut_known_dimensions :=
            {
              !mut_known_dimensions with
              height = Some (Float.max new_height_raw 0.0);
            }
            |> apply_aspect_ratio_to_size
            |> fun size -> Size.maybe_clamp size min_size max_size
      | _ -> ());

      let size = !mut_known_dimensions in

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
      (* After clamping, reapply aspect ratio if needed *)
      let final_clamped =
        match aspect_ratio with
        | None -> clamped
        | Some ratio -> (
            match
              (clamped.width, clamped.height, unclamped.width, unclamped.height)
            with
            (* If height was clamped by max/min, recalculate width *)
            | _, Some h, Some _, Some orig_h when h <> orig_h ->
                { clamped with width = Some (h *. ratio) }
            (* If width was clamped by max/min, recalculate height *)
            | Some w, _, Some orig_w, Some _ when w <> orig_w ->
                { clamped with height = Some (w /. ratio) }
            | _ -> clamped)
      in
      let final_size =
        {
          width = Option.value ~default:measured_size.width final_clamped.width;
          height =
            Option.value ~default:measured_size.height final_clamped.height;
        }
      in

      (* Resolve auto margins and compute position *)
      let non_auto_margin =
        {
          left = Option.value ~default:0.0 margin.left;
          right = Option.value ~default:0.0 margin.right;
          top = Option.value ~default:0.0 margin.top;
          bottom = Option.value ~default:0.0 margin.bottom;
        }
      in
      let free_space =
        {
          width =
            constants.container_size.width -. final_size.width
            -. non_auto_margin.left -. non_auto_margin.right;
          height =
            constants.container_size.height -. final_size.height
            -. non_auto_margin.top -. non_auto_margin.bottom;
        }
        |> fun s ->
        { width = Float.max 0.0 s.width; height = Float.max 0.0 s.height }
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
                constants.border.left +. left +. resolved_margin.left
            | None, Some right ->
                constants.container_size.width -. constants.border.right
                -. constants.scrollbar_gutter.x -. final_size.width -. right
                -. resolved_margin.right
            | None, None ->
                (* Apply alignment when no inset - matches Rust flexbox.rs line 2210+ *)
                constants.content_box_inset.left +. resolved_margin.left);
          y =
            (match (inset.top, inset.bottom) with
            | Some top, _ -> constants.border.top +. top +. resolved_margin.top
            | None, Some bottom ->
                constants.container_size.height -. constants.border.bottom
                -. constants.scrollbar_gutter.y -. final_size.height -. bottom
                -. resolved_margin.bottom
            | None, None ->
                (* Apply alignment when no inset - matches Rust flexbox.rs line 2254+ *)
                constants.content_box_inset.top +. resolved_margin.top);
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
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
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
  (* Scrollbar gutters are reserved when the overflow property is set to Overflow::Scroll.
     However, the axes are switched (transposed) because a node that scrolls vertically needs
     *horizontal* space to be reserved for a scrollbar *)
  let scrollbar_gutter =
    match (style.overflow.x, style.overflow.y) with
    | Style.Scroll, Style.Scroll ->
        { x = style.scrollbar_width; y = style.scrollbar_width }
    | Style.Scroll, _ -> { x = 0.0; y = style.scrollbar_width }
    | _, Style.Scroll -> { x = style.scrollbar_width; y = 0.0 }
    | _ -> point_zero
  in

  let content_box_inset =
    let base_inset = rect_add padding border in
    (* Add scrollbar gutter to content box inset *)
    {
      left = base_inset.left;
      right = base_inset.right +. scrollbar_gutter.x;
      top = base_inset.top;
      bottom = base_inset.bottom +. scrollbar_gutter.y;
    }
  in

  let padding_border_sum = rect_sum_axes (rect_add padding border) in
  let box_sizing_adjustment =
    if style.box_sizing = Style.Content_box then padding_border_sum
    else size_zero
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

  let constants =
    ref
      {
        dir;
        is_row;
        is_column;
        is_wrap;
        is_wrap_reverse;
        min_size =
          (let base_min_size =
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
             }
           in
           let with_aspect =
             match style.aspect_ratio with
             | None -> base_min_size
             | Some ratio -> (
                 match (base_min_size.width, base_min_size.height) with
                 | Some w, None ->
                     { base_min_size with height = Some (w /. ratio) }
                 | None, Some h ->
                     { base_min_size with width = Some (h *. ratio) }
                 | _ -> base_min_size)
           in
           Math.size_maybe_add_option_float with_aspect box_sizing_adjustment);
        max_size =
          (let base_max_size =
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
             }
           in
           let with_aspect =
             match style.aspect_ratio with
             | None -> base_max_size
             | Some ratio -> (
                 match (base_max_size.width, base_max_size.height) with
                 | Some w, None ->
                     { base_max_size with height = Some (w /. ratio) }
                 | None, Some h ->
                     { base_max_size with width = Some (h *. ratio) }
                 | _ -> base_max_size)
           in
           Math.size_maybe_add_option_float with_aspect box_sizing_adjustment);
        margin;
        border;
        content_box_inset;
        scrollbar_gutter;
        gap =
          {
            width =
              (match style.gap.width with
              | Style.Length_percentage.Length v -> v
              | Style.Length_percentage.Percent p ->
                  Option.value ~default:0.0
                    (Option.map (fun w -> w *. p) node_inner_size.width));
            height =
              (match style.gap.height with
              | Style.Length_percentage.Length v -> v
              | Style.Length_percentage.Percent p ->
                  Option.value ~default:0.0
                    (Option.map (fun h -> h *. p) node_inner_size.height));
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
  | None ->
      determine_container_main_size
        (module Tree)
        tree inputs.sizing_mode available_space !flex_lines constants);

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
  calculate_children_base_lines
    (module Tree)
    tree known_dimensions available_space flex_lines !constants;

  (* 8. Calculate the cross size of each flex line *)
  calculate_cross_size flex_lines known_dimensions !constants;

  (* 9. Handle 'align-content: stretch' *)
  handle_align_content_stretch flex_lines known_dimensions !constants;

  (* 10. Collapse visibility:collapse items - skipped for now *)

  (* 11. Determine the used cross size of each flex item *)
  determine_used_cross_size (module Tree) tree !flex_lines !constants;

  (* 12. Main-axis alignment *)
  distribute_remaining_free_space !flex_lines !constants;

  (* 13. Resolve cross-axis auto margins *)
  resolve_cross_axis_auto_margins !flex_lines !constants;

  (* 14. Determine the flex container's used cross size *)
  let total_cross_size =
    determine_container_cross_size !flex_lines known_dimensions constants
  in

  (* 15. Align all flex lines per align-content *)
  align_flex_lines_per_align_content flex_lines !constants total_cross_size;

  (* Perform final layout *)
  let inflow_content_size =
    perform_final_layout_and_positioning
      (module Tree)
      tree node !constants.container_size !constants !flex_lines
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

  (* 8.5. Flex Container Baselines: calculate the flex container's first baseline *)
  (* See https://www.w3.org/TR/css-flexbox-1/#flex-baselines *)
  let first_vertical_baseline =
    match !flex_lines with
    | [] -> None
    | first_line :: _ -> (
        (* Find first item that is either column or has baseline alignment *)
        let baseline_item =
          List.find_opt
            (fun item ->
              !constants.is_column || item.align_self = Style.Alignment.Baseline)
            first_line.items
        in
        match baseline_item with
        | Some item ->
            let offset_vertical =
              if !constants.is_row then item.offset_cross else item.offset_main
            in
            Some (offset_vertical +. item.baseline)
        | None -> (
            (* Fall back to first item if no baseline item found *)
            match first_line.items with
            | item :: _ ->
                let offset_vertical =
                  if !constants.is_row then item.offset_cross
                  else item.offset_main
                in
                Some (offset_vertical +. item.baseline)
            | [] -> None))
  in

  (* Return the layout output *)
  Layout.Layout_output.of_sizes_and_baselines ~size:!constants.container_size
    ~content_size
    ~first_baselines:{ x = None; y = first_vertical_baseline }

(** Compute layout constants from the given style *)
(* compute_constants function was inlined into compute_preliminary *)

(** Main entry point for flexbox layout computation *)
let compute_flexbox_layout (type tree)
    (module Tree : Tree_intf.LayoutFlexboxContainer
      with type t = tree
       and type flexbox_container_style = Style.style
       and type flexbox_item_style = Style.style) (tree : tree)
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
    (* Implement the Rust logic: known_dimensions.or(min_max_definite_size.or(clamped_style_size).maybe_max(padding_border_sum)) *)
    let or_size (a : float option size) (b : float option size) :
        float option size =
      {
        width = (match a.width with Some _ -> a.width | None -> b.width);
        height = (match a.height with Some _ -> a.height | None -> b.height);
      }
    in
    let min_max_or_clamped = or_size min_max_definite_size clamped_style_size in
    let base = or_size known_dimensions min_max_or_clamped in
    let result = Math.size_maybe_max_option_float base padding_border_sum in
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
  else
    (* Delegate to compute_preliminary for full layout *)
    compute_preliminary
      (module Tree)
      tree node
      { inputs with known_dimensions = styled_based_known_dimensions }
