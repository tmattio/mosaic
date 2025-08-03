(** grid_item.ml
    ---------------------------------------------------------------------------
    Contains GridItem used to represent a single grid item during layout
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry
open Grid_coordinates
open Grid_grid_axis_helpers

type 'tree intrinsic_size_measurer = {
  tree : (module Tree_intf.LayoutPartialTreeExt with type t = 'tree);
  tree_val : 'tree;
  other_axis_tracks : Grid_track.t array;
  get_track_size_estimate :
    Grid_track.t -> float option -> 'tree -> float option;
  axis : abstract_axis;
  inner_node_size : float option size;
}
(** Type for intrinsic size measurer used in track sizing *)

type t = {
  node : Node.Node_id.t;  (** The id of the node that this item represents *)
  source_order : int;
      (** The order of the item in the children array. We sort the list of grid
          items during track sizing. This field allows us to sort back the
          original order for final positioning *)
  row : OriginZeroLine.t line;
      (** The item's definite row-start and row-end, as resolved by the
          placement algorithm (in origin-zero coordinates) *)
  column : OriginZeroLine.t line;
      (** The items definite column-start and column-end, as resolved by the
          placement algorithm (in origin-zero coordinates) *)
  is_compressible_replaced : bool;
      (** Is it a compressible replaced element?
          https://drafts.csswg.org/css-sizing-3/#min-content-zero *)
  overflow : Style.overflow point;  (** The item's overflow style *)
  box_sizing : Style.box_sizing;  (** The item's box_sizing style *)
  size : Style.Dimension.t size;  (** The item's size style *)
  min_size : Style.Dimension.t size;  (** The item's min_size style *)
  max_size : Style.Dimension.t size;  (** The item's max_size style *)
  aspect_ratio : float option;  (** The item's aspect_ratio style *)
  padding : Style.Length_percentage.t rect;  (** The item's padding style *)
  border : Style.Length_percentage.t rect;  (** The item's border style *)
  margin : Style.Length_percentage_auto.t rect;  (** The item's margin style *)
  align_self : Style.Alignment.align_self;
      (** The item's align_self property, or the parent's align_items property
          is not set *)
  justify_self : Style.Alignment.justify_self;
      (** The item's justify_self property, or the parent's justify_items
          property is not set *)
  mutable baseline : float option;  (** The items first baseline (horizontal) *)
  mutable baseline_shim : float;
      (** Shim for baseline alignment that acts like an extra top margin TODO:
          Support last baseline and vertical text baselines *)
  mutable row_indexes : int line;
      (** The item's definite row-start and row-end (same as `row` field, except
          in a different coordinate system) (as indexes into the Vec<GridTrack>
          stored in a grid's AbstractAxisTracks) *)
  mutable column_indexes : int line;
      (** The items definite column-start and column-end (same as `column`
          field, except in a different coordinate system) (as indexes into the
          Vec<GridTrack> stored in a grid's AbstractAxisTracks) *)
  mutable crosses_flexible_row : bool;
      (** Whether the item crosses a flexible row *)
  mutable crosses_flexible_column : bool;
      (** Whether the item crosses a flexible column *)
  mutable crosses_intrinsic_row : bool;
      (** Whether the item crosses a intrinsic row *)
  mutable crosses_intrinsic_column : bool;
      (** Whether the item crosses a intrinsic column *)
  (* Caches for intrinsic size computation. These caches are only valid for a single run of the track-sizing algorithm. *)
  mutable available_space_cache : float option size option;
      (** Cache for the known_dimensions input to intrinsic sizing computation
      *)
  mutable min_content_contribution_cache : float option size;
      (** Cache for the min-content size *)
  mutable minimum_contribution_cache : float option size;
      (** Cache for the minimum contribution *)
  mutable max_content_contribution_cache : float option size;
      (** Cache for the max-content size *)
  mutable y_position : float;
      (** Final y position. Used to compute baseline alignment for the
          container. *)
  mutable height : float;
      (** Final height. Used to compute baseline alignment for the container. *)
}
(** Represents a single grid item *)

(** Create a new item given a concrete placement in both axes *)
let new_with_placement_style_and_order ~node ~col_span ~row_span
    ~(style : Style.style) ~parent_align_items ~parent_justify_items
    ~source_order =
  {
    node;
    source_order;
    row = row_span;
    column = col_span;
    is_compressible_replaced =
      style.item_is_replaced && style.size.width <> Auto
      && style.size.height <> Auto;
    overflow = style.overflow;
    box_sizing = style.box_sizing;
    size = style.size;
    min_size = style.min_size;
    max_size = style.max_size;
    aspect_ratio = style.aspect_ratio;
    padding = style.padding;
    border = style.border;
    margin = style.margin;
    align_self =
      (match style.align_self with Some v -> v | None -> parent_align_items);
    justify_self =
      (match style.justify_self with
      | Some v -> v
      | None -> parent_justify_items);
    baseline = None;
    baseline_shim = 0.0;
    row_indexes = { start = 0; end_ = 0 };
    (* Properly initialised later *)
    column_indexes = { start = 0; end_ = 0 };
    (* Properly initialised later *)
    crosses_flexible_row = false;
    (* Properly initialised later *)
    crosses_flexible_column = false;
    (* Properly initialised later *)
    crosses_intrinsic_row = false;
    (* Properly initialised later *)
    crosses_intrinsic_column = false;
    (* Properly initialised later *)
    available_space_cache = None;
    min_content_contribution_cache = Size.none;
    max_content_contribution_cache = Size.none;
    minimum_contribution_cache = Size.none;
    y_position = 0.0;
    height = 0.0;
  }

(** This item's placement in the specified axis in OriginZero coordinates *)
let placement t axis =
  let open Abstract_axis in
  match axis with Block -> t.row | Inline -> t.column

(** This item's placement in the specified axis as GridTrackVec indices *)
let placement_indexes t axis =
  let open Abstract_axis in
  match axis with Block -> t.row_indexes | Inline -> t.column_indexes

(** Returns a range which can be used as an index into the GridTrackVec in the
    specified axis which will produce a sub-slice of covering all the tracks and
    lines that this item spans excluding the lines that bound it. *)
let track_range_excluding_lines t axis =
  let indexes = placement_indexes t axis in
  (indexes.start + 1, indexes.end_)

(** Returns the number of tracks that this item spans in the specified axis *)
let span t axis =
  let open Abstract_axis in
  match axis with
  | Block -> Line_ext.span t.row
  | Inline -> Line_ext.span t.column

(** Returns the pre-computed value indicating whether the grid item crosses a
    flexible track in the specified axis *)
let crosses_flexible_track t axis =
  let open Abstract_axis in
  match axis with
  | Inline -> t.crosses_flexible_column
  | Block -> t.crosses_flexible_row

(** Returns the pre-computed value indicating whether the grid item crosses an
    intrinsic track in the specified axis *)
let crosses_intrinsic_track t axis =
  let open Abstract_axis in
  match axis with
  | Inline -> t.crosses_intrinsic_column
  | Block -> t.crosses_intrinsic_row

(** For an item spanning multiple tracks, the upper limit used to calculate its
    limited min-/max-content contribution is the sum of the fixed max track
    sizing functions of any tracks it spans, and is applied if it only spans
    such tracks. *)
let spanned_track_limit t axis axis_tracks axis_parent_size calc =
  let start_idx, end_idx = track_range_excluding_lines t axis in
  let spanned_tracks = Array.sub axis_tracks start_idx (end_idx - start_idx) in
  let tracks_all_fixed =
    Array.for_all
      (fun track ->
        match
          Grid_helpers.MaxSizing.definite_limit
            track.Grid_track.max_track_sizing_function axis_parent_size calc
        with
        | Some _ -> true
        | None -> false)
      spanned_tracks
  in
  if tracks_all_fixed then
    let limit =
      Array.fold_left
        (fun acc track ->
          let limit =
            Grid_helpers.MaxSizing.definite_limit
              track.Grid_track.max_track_sizing_function axis_parent_size calc
          in
          acc +. match limit with Some l -> l | None -> assert false)
        0.0 spanned_tracks
    in
    Some limit
  else None

(** Similar to the spanned_track_limit, but excludes FitContent arguments from
    the limit. Used to clamp the automatic minimum contributions of an item *)
let spanned_fixed_track_limit t axis axis_tracks axis_parent_size calc =
  let start_idx, end_idx = track_range_excluding_lines t axis in
  let spanned_tracks = Array.sub axis_tracks start_idx (end_idx - start_idx) in
  let tracks_all_fixed =
    Array.for_all
      (fun track ->
        match
          Grid_helpers.MaxSizing.definite_value
            track.Grid_track.max_track_sizing_function axis_parent_size calc
        with
        | Some _ -> true
        | None -> false)
      spanned_tracks
  in
  if tracks_all_fixed then
    let limit =
      Array.fold_left
        (fun acc track ->
          let value =
            Grid_helpers.MaxSizing.definite_value
              track.Grid_track.max_track_sizing_function axis_parent_size calc
          in
          acc +. match value with Some v -> v | None -> assert false)
        0.0 spanned_tracks
    in
    Some limit
  else None

(** Compute the item's resolved margins for size contributions. Horizontal
    percentage margins always resolve to zero if the container size is
    indefinite as otherwise this would introduce a cyclic dependency. *)
let margins_axis_sums_with_baseline_shims (type tree) t inner_node_width
    (measurer : tree intrinsic_size_measurer) =
  let module Tree =
    (val measurer.tree : Tree_intf.LayoutPartialTreeExt with type t = tree)
  in
  let calc ~ptr ~basis =
    Tree.resolve_calc_value measurer.tree_val ~ptr ~basis
  in
  let left =
    Resolve.resolve_or_zero_length_percentage_auto t.margin.left (Some 0.0) calc
  in
  let right =
    Resolve.resolve_or_zero_length_percentage_auto t.margin.right (Some 0.0)
      calc
  in
  let top =
    Resolve.resolve_or_zero_length_percentage_auto t.margin.top inner_node_width
      calc
    +. t.baseline_shim
  in
  let bottom =
    Resolve.resolve_or_zero_length_percentage_auto t.margin.bottom
      inner_node_width calc
  in
  Size.{ width = left +. right; height = top +. bottom }

(** Compute the known_dimensions to be passed to the child sizing functions The
    key thing that is being done here is applying stretch alignment, which is
    necessary to allow percentage sizes further down the tree to resolve
    properly in some cases *)
let known_dimensions (type tree) t (measurer : tree intrinsic_size_measurer)
    inner_node_size grid_area_size =
  let module Tree =
    (val measurer.tree : Tree_intf.LayoutPartialTreeExt with type t = tree)
  in
  let calc ~ptr ~basis =
    Tree.resolve_calc_value measurer.tree_val ~ptr ~basis
  in
  let margins =
    margins_axis_sums_with_baseline_shims t inner_node_size.Size.width measurer
  in

  let padding =
    Resolve.resolve_or_zero_rect_with_size
      Resolve.resolve_or_zero_length_percentage t.padding grid_area_size calc
  in
  let border =
    Resolve.resolve_or_zero_rect_with_size
      Resolve.resolve_or_zero_length_percentage t.border grid_area_size calc
  in
  let padding_border_size = Rect.sum_axes (Rect.add padding border) in
  let box_sizing_adjustment =
    if t.box_sizing = Style.Border_box then Size.zero else padding_border_size
  in

  let inherent_size =
    Size.maybe_apply_aspect_ratio
      (Size.maybe_add
         (Size.maybe_resolve t.size grid_area_size calc)
         box_sizing_adjustment)
      t.aspect_ratio
  in
  let min_size =
    Size.maybe_apply_aspect_ratio
      (Size.maybe_add
         (Size.maybe_resolve t.min_size grid_area_size calc)
         box_sizing_adjustment)
      t.aspect_ratio
  in
  let max_size =
    Size.maybe_apply_aspect_ratio
      (Size.maybe_add
         (Size.maybe_resolve t.max_size grid_area_size calc)
         box_sizing_adjustment)
      t.aspect_ratio
  in

  let grid_area_minus_item_margins_size =
    Size.maybe_sub grid_area_size margins
  in

  (* If node is absolutely positioned and width is not set explicitly, then deduce it
     from left, right and container_content_box if both are set. *)
  let width =
    match inherent_size.width with
    | Some w -> Some w
    | None ->
        (* Apply width based on stretch alignment if:
           - Alignment style is "stretch"
           - The node is not absolutely positioned
           - The node does not have auto margins in this axis. *)
        if
          (match t.margin.left with
          | Style.Length_percentage_auto.Auto -> false
          | _ -> true)
          && (match t.margin.right with
             | Style.Length_percentage_auto.Auto -> false
             | _ -> true)
          && t.justify_self = Style.Alignment.Stretch
        then grid_area_minus_item_margins_size.width
        else None
  in

  (* Reapply aspect ratio after stretch and absolute position width adjustments *)
  let size_after_width =
    Size.maybe_apply_aspect_ratio
      { width; height = inherent_size.height }
      t.aspect_ratio
  in

  let height =
    match size_after_width.height with
    | Some h -> Some h
    | None ->
        (* Apply height based on stretch alignment if:
           - Alignment style is "stretch"
           - The node is not absolutely positioned
           - The node does not have auto margins in this axis. *)
        if
          (match t.margin.top with
          | Style.Length_percentage_auto.Auto -> false
          | _ -> true)
          && (match t.margin.bottom with
             | Style.Length_percentage_auto.Auto -> false
             | _ -> true)
          && t.align_self = Style.Alignment.Stretch
        then grid_area_minus_item_margins_size.height
        else None
  in

  (* Reapply aspect ratio after stretch and absolute position height adjustments *)
  let size_after_height =
    Size.maybe_apply_aspect_ratio
      { width = size_after_width.width; height }
      t.aspect_ratio
  in

  (* Clamp size by min and max width/height *)
  Size.maybe_clamp size_after_height min_size max_size

(** Compute the available_space to be passed to the child sizing functions These
    are estimates based on either the max track sizing function or the
    provisional base size in the opposite axis to the one currently being sized.
    https://www.w3.org/TR/css-grid-1/#algo-overview *)
let available_space t axis other_axis_tracks other_axis_available_space
    get_track_size_estimate =
  let start_idx, end_idx =
    track_range_excluding_lines t (Abstract_axis.other axis)
  in
  let item_other_axis_size =
    let sum = ref 0.0 in
    let all_defined = ref true in
    for i = start_idx to end_idx - 1 do
      match
        get_track_size_estimate other_axis_tracks.(i) other_axis_available_space
      with
      | Some size ->
          sum :=
            !sum +. size
            +. other_axis_tracks.(i).Grid_track.content_alignment_adjustment
      | None -> all_defined := false
    done;
    if !all_defined then Some !sum else None
  in

  let size = Size.none in
  let size =
    Abstract_axis.set_size size (Abstract_axis.other axis) item_other_axis_size
  in
  size

(** Retrieve the available_space from the cache or compute them using the passed
    parameters *)
let available_space_cached t axis other_axis_tracks other_axis_available_space
    get_track_size_estimate =
  match t.available_space_cache with
  | Some cached -> cached
  | None ->
      let available_spaces =
        available_space t axis other_axis_tracks other_axis_available_space
          get_track_size_estimate
      in
      t.available_space_cache <- Some available_spaces;
      available_spaces

(** Compute the item's min content contribution from the provided parameters *)
let min_content_contribution (type tree) t axis
    (measurer : tree intrinsic_size_measurer) available_space inner_node_size =
  let module Tree =
    (val measurer.tree : Tree_intf.LayoutPartialTreeExt with type t = tree)
  in
  let known_dimensions =
    known_dimensions t measurer inner_node_size available_space
  in
  let available_space_map =
    Size.map available_space ~f:(function
      | Some size -> Style.Available_space.Definite size
      | None -> Style.Available_space.Min_content)
  in
  Tree.measure_child_size measurer.tree_val t.node ~known_dimensions
    ~parent_size:inner_node_size ~available_space:available_space_map
    ~sizing_mode:Layout.Sizing_mode.Inherent_size
    ~axis:(Abstract_axis.as_abs_naive axis)
    ~vertical_margins_are_collapsible:Line.false_

(** Retrieve the item's min content contribution from the cache or compute it
    using the provided parameters *)
let min_content_contribution_cached t axis measurer available_space
    inner_node_size =
  match Size.get t.min_content_contribution_cache axis with
  | Some cached -> cached
  | None ->
      let size =
        min_content_contribution t axis measurer available_space inner_node_size
      in
      t.min_content_contribution_cache <-
        Size.set t.min_content_contribution_cache axis (Some size);
      size

(** Compute the item's max content contribution from the provided parameters *)
let max_content_contribution (type tree) t axis
    (measurer : tree intrinsic_size_measurer) available_space inner_node_size =
  let module Tree =
    (val measurer.tree : Tree_intf.LayoutPartialTreeExt with type t = tree)
  in
  let known_dimensions =
    known_dimensions t measurer inner_node_size available_space
  in
  let available_space_map =
    Size.map available_space ~f:(function
      | Some size -> Style.Available_space.Definite size
      | None -> Style.Available_space.Max_content)
  in
  Tree.measure_child_size measurer.tree_val t.node ~known_dimensions
    ~parent_size:inner_node_size ~available_space:available_space_map
    ~sizing_mode:Layout.Sizing_mode.Inherent_size
    ~axis:(Abstract_axis.as_abs_naive axis)
    ~vertical_margins_are_collapsible:Line.false_

(** Retrieve the item's max content contribution from the cache or compute it
    using the provided parameters *)
let max_content_contribution_cached t axis measurer available_space
    inner_node_size =
  match Size.get t.max_content_contribution_cache axis with
  | Some cached -> cached
  | None ->
      let size =
        max_content_contribution t axis measurer available_space inner_node_size
      in
      t.max_content_contribution_cache <-
        Size.set t.max_content_contribution_cache axis (Some size);
      size

(** The minimum contribution of an item is the smallest outer size it can have.
    Specifically:
    - If the item's computed preferred size behaves as auto or depends on the
      size of its containing block in the relevant axis: Its minimum
      contribution is the outer size that would result from assuming the item's
      used minimum size as its preferred size;
    - Else the item's minimum contribution is its min-content contribution.

    Because the minimum contribution often depends on the size of the item's
    content, it is considered a type of intrinsic size contribution. See:
    https://www.w3.org/TR/css-grid-1/#min-size-auto *)
let minimum_contribution (type tree) t (measurer : tree intrinsic_size_measurer)
    axis axis_tracks known_dimensions inner_node_size =
  let module Tree =
    (val measurer.tree : Tree_intf.LayoutPartialTreeExt with type t = tree)
  in
  let calc ~ptr ~basis =
    Tree.resolve_calc_value measurer.tree_val ~ptr ~basis
  in
  let padding =
    Resolve.resolve_or_zero_rect_with_size
      Resolve.resolve_or_zero_length_percentage t.padding inner_node_size calc
  in
  let border =
    Resolve.resolve_or_zero_rect_with_size
      Resolve.resolve_or_zero_length_percentage t.border inner_node_size calc
  in
  let padding_border_size = Rect.sum_axes (Rect.add padding border) in
  let box_sizing_adjustment =
    if t.box_sizing = Style.Border_box then Size.zero else padding_border_size
  in

  let size_option =
    match
      Size.get
        (Size.maybe_apply_aspect_ratio
           (Size.maybe_add
              (Size.maybe_resolve t.size inner_node_size calc)
              box_sizing_adjustment)
           t.aspect_ratio)
        axis
    with
    | Some s -> Some s
    | None -> (
        match
          Size.get
            (Size.maybe_apply_aspect_ratio
               (Size.maybe_add
                  (Size.maybe_resolve t.min_size inner_node_size calc)
                  box_sizing_adjustment)
               t.aspect_ratio)
            axis
        with
        | Some s -> Some s
        | None -> (
            match
              Overflow.maybe_into_automatic_min_size (Point.get t.overflow axis)
            with
            | Some s -> Some s
            | None ->
                (* Automatic minimum size. See https://www.w3.org/TR/css-grid-1/#min-size-auto *)

                (* To provide a more reasonable default minimum size for grid items, the used value of its automatic minimum size
                   in a given axis is the content-based minimum size if all of the following are true: *)
                let start_idx, end_idx = track_range_excluding_lines t axis in

                (* it is not a scroll container *)
                (* TODO: support overflow property *)

                (* it spans at least one track in that axis whose min track sizing function is auto *)
                let spans_auto_min_track =
                  let rec check i =
                    if i >= end_idx then false
                    else if
                      Grid_helpers.MinSizing.is_auto
                        axis_tracks.(i).Grid_track.min_track_sizing_function
                    then true
                    else check (i + 1)
                  in
                  check start_idx
                in

                (* if it spans more than one track in that axis, none of those tracks are flexible *)
                let only_span_one_track = end_idx - start_idx = 1 in
                let spans_a_flexible_track =
                  let rec check i =
                    if i >= end_idx then false
                    else if
                      Grid_helpers.MaxSizing.is_fr
                        axis_tracks.(i).Grid_track.max_track_sizing_function
                    then true
                    else check (i + 1)
                  in
                  check start_idx
                in

                let use_content_based_minimum =
                  spans_auto_min_track
                  && (only_span_one_track || not spans_a_flexible_track)
                in

                (* Otherwise, the automatic minimum size is zero, as usual. *)
                if use_content_based_minimum then
                  let minimum_contribution =
                    min_content_contribution_cached t axis measurer
                      known_dimensions inner_node_size
                  in

                  (* If the item is a compressible replaced element, and has a definite preferred size or maximum size in the
                     relevant axis, the size suggestion is capped by those sizes; for this purpose, any indefinite percentages
                     in these sizes are resolved against zero (and considered definite). *)
                  let minimum_contribution =
                    if t.is_compressible_replaced then
                      let size =
                        Dimension.maybe_resolve (Size.get t.size axis)
                          (Some 0.0) calc
                      in
                      let max_size =
                        Dimension.maybe_resolve (Size.get t.max_size axis)
                          (Some 0.0) calc
                      in
                      match
                        Option.maybe_min
                          (Option.maybe_min (Some minimum_contribution) size)
                          max_size
                      with
                      | Some v -> v
                      | None -> minimum_contribution
                    else minimum_contribution
                  in

                  Some minimum_contribution
                else Some 0.0))
  in

  (* In all cases, the size suggestion is additionally clamped by the maximum size in the affected axis, if it's definite.
     Note: The argument to fit-content() does not clamp the content-based minimum size in the same way as a fixed max track
     sizing function. *)
  let limit =
    spanned_fixed_track_limit t axis axis_tracks
      (Size.get inner_node_size axis)
      calc
  in
  match Option.maybe_min size_option limit with
  | Some v -> v
  | None -> ( match size_option with Some v -> v | None -> 0.0)

(** Retrieve the item's minimum contribution from the cache or compute it using
    the provided parameters *)
let minimum_contribution_cached t measurer axis axis_tracks known_dimensions
    inner_node_size =
  match Size.get t.minimum_contribution_cache axis with
  | Some cached -> cached
  | None ->
      let size =
        minimum_contribution t measurer axis axis_tracks known_dimensions
          inner_node_size
      in
      t.minimum_contribution_cache <-
        Size.set t.minimum_contribution_cache axis (Some size);
      size
