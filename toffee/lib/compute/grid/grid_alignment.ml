(** alignment.ml
    ---------------------------------------------------------------------------
    Alignment of tracks and final positioning of items
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry
open Style
open Grid_track
open Layout

(** Align the grid tracks within the grid according to the align-content (rows)
    or justify-content (columns) property. This only does anything if the size
    of the grid is not equal to the size of the grid container in the axis being
    aligned. *)
let align_tracks ~grid_container_content_box_size ~padding ~border ~tracks
    ~track_alignment_style =
  let used_size =
    Array.fold_left (fun acc track -> acc +. track.base_size) 0.0 tracks
  in
  let free_space = grid_container_content_box_size -. used_size in
  let origin = padding.start +. border.start in

  (* Count the number of non-collapsed tracks (not counting gutters) *)
  let num_tracks =
    let count = ref 0 in
    for i = 1 to Array.length tracks - 1 do
      if i mod 2 = 1 && not tracks.(i).is_collapsed then incr count
    done;
    !count
  in

  (* Grid layout treats gaps as full tracks rather than applying them at alignment so we
     simply pass zero here. Grid layout is never reversed. *)
  let gap = 0.0 in
  let layout_is_reversed = false in
  let is_safe = false in
  (* TODO: Implement safe alignment *)
  (* Apply alignment fallback *)
  let track_alignment =
    let alignment_mode, is_safe =
      if num_tracks <= 1 || free_space <= 0. then
        match track_alignment_style with
        | Style.Alignment.Stretch -> (Style.Alignment.Flex_start, true)
        | Style.Alignment.Space_between -> (Style.Alignment.Flex_start, true)
        | Style.Alignment.Space_around -> (Style.Alignment.Center, true)
        | Style.Alignment.Space_evenly -> (Style.Alignment.Center, true)
        | _ -> (track_alignment_style, is_safe)
      else (track_alignment_style, is_safe)
    in
    if free_space <= 0. && is_safe then Style.Alignment.Start
    else alignment_mode
  in

  (* Compute offsets *)
  let total_offset = ref origin in
  Array.iteri
    (fun i track ->
      (* Odd tracks are gutters (but slices are zero-indexed, so odd tracks have even indices) *)
      let is_gutter = i mod 2 = 0 in

      (* The first non-gutter track is index 1 *)
      let is_first = i = 1 in

      let offset =
        if is_gutter then 0.0
        else if
          (* Compute alignment offset *)
          is_first
        then
          match track_alignment with
          | Style.Alignment.Start -> 0.
          | Style.Alignment.Flex_start ->
              if layout_is_reversed then free_space else 0.
          | Style.Alignment.End -> free_space
          | Style.Alignment.Flex_end ->
              if layout_is_reversed then 0. else free_space
          | Style.Alignment.Center -> free_space /. 2.
          | Style.Alignment.Stretch -> 0.
          | Style.Alignment.Space_between -> 0.
          | Style.Alignment.Space_around ->
              if free_space >= 0. then
                free_space /. float_of_int num_tracks /. 2.
              else free_space /. 2.
          | Style.Alignment.Space_evenly ->
              if free_space >= 0. then
                free_space /. float_of_int (num_tracks + 1)
              else free_space /. 2.
        else
          let free_space = Float.max free_space 0. in
          gap
          +.
          match track_alignment with
          | Style.Alignment.Start | Style.Alignment.Flex_start
          | Style.Alignment.End | Style.Alignment.Flex_end
          | Style.Alignment.Center | Style.Alignment.Stretch ->
              0.
          | Style.Alignment.Space_between ->
              free_space /. float_of_int (num_tracks - 1)
          | Style.Alignment.Space_around ->
              free_space /. float_of_int num_tracks
          | Style.Alignment.Space_evenly ->
              free_space /. float_of_int (num_tracks + 1)
      in

      track.offset <- !total_offset +. offset;
      total_offset := !total_offset +. offset +. track.base_size)
    tracks

(** Align and size a grid item along a single axis *)
let align_item_within_area ~grid_area
    ~(alignment_style : Style.Alignment.align_items) ~resolved_size ~position
    ~inset ~margin ~baseline_shim =
  (* Calculate grid area dimension in the axis *)
  let non_auto_margin =
    {
      start =
        (match margin.start with Some v -> v | None -> 0.0) +. baseline_shim;
      end_ = (match margin.end_ with Some v -> v | None -> 0.0);
    }
  in
  let grid_area_size = max (grid_area.end_ -. grid_area.start) 0.0 in
  let free_space =
    max
      (grid_area_size -. resolved_size
      -. (non_auto_margin.start +. non_auto_margin.end_))
      0.0
  in

  (* Expand auto margins to fill available space *)
  let auto_margin_count =
    (if Option.is_none margin.start then 1 else 0)
    + if Option.is_none margin.end_ then 1 else 0
  in
  let auto_margin_size =
    if auto_margin_count > 0 then free_space /. float_of_int auto_margin_count
    else 0.0
  in
  let resolved_margin =
    {
      start =
        (match margin.start with Some v -> v | None -> auto_margin_size)
        +. baseline_shim;
      end_ = (match margin.end_ with Some v -> v | None -> auto_margin_size);
    }
  in

  (* Compute offset in the axis *)
  let alignment_based_offset =
    match alignment_style with
    | Style.Alignment.Start | Style.Alignment.Flex_start ->
        resolved_margin.start
    | Style.Alignment.End | Style.Alignment.Flex_end ->
        grid_area_size -. resolved_size -. resolved_margin.end_
    | Style.Alignment.Center ->
        (grid_area_size -. resolved_size +. resolved_margin.start
       -. resolved_margin.end_)
        /. 2.0
    (* TODO: Add support for baseline alignment. For now we treat it as "start". *)
    | Style.Alignment.Baseline -> resolved_margin.start
    | Style.Alignment.Stretch -> resolved_margin.start
  in

  let offset_within_area =
    if position = Absolute then
      match (inset.start, inset.end_) with
      | Some start, _ -> start +. non_auto_margin.start
      | None, Some end_ ->
          grid_area_size -. end_ -. resolved_size -. non_auto_margin.end_
      | None, None -> alignment_based_offset
    else alignment_based_offset
  in

  let start =
    let base = grid_area.start +. offset_within_area in
    if position = Relative then
      base
      +.
      match inset.start with
      | Some v -> v
      | None -> ( match inset.end_ with Some pos -> -.pos | None -> 0.0)
    else base
  in

  (start, resolved_margin)

type 'a in_both_abs_axis = { horizontal : 'a; vertical : 'a }
(** Type for in-both-abs-axis alignment *)

(** Align and size a grid item into it's final position *)
let align_and_position_item ~node ~order ~grid_area ~container_alignment_styles
    ~baseline_shim ~style ~calc ~perform_child_layout ~set_unrounded_layout =
  let open Grid_grid_axis_helpers in
  let grid_area_size =
    {
      width = grid_area.right -. grid_area.left;
      height = grid_area.bottom -. grid_area.top;
    }
  in

  let overflow = style.overflow in
  let scrollbar_width = style.scrollbar_width in
  let aspect_ratio = style.aspect_ratio in
  let justify_self = style.justify_self in
  let align_self = style.align_self in

  let position = style.position in
  let inset_horizontal =
    let l =
      Resolve.maybe_resolve_length_percentage_auto style.inset.left
        (Some grid_area_size.width) calc
    in
    let r =
      Resolve.maybe_resolve_length_percentage_auto style.inset.right
        (Some grid_area_size.width) calc
    in
    { start = l; end_ = r }
  in
  let inset_vertical =
    let t =
      Resolve.maybe_resolve_length_percentage_auto style.inset.top
        (Some grid_area_size.height) calc
    in
    let b =
      Resolve.maybe_resolve_length_percentage_auto style.inset.bottom
        (Some grid_area_size.height) calc
    in
    { start = t; end_ = b }
  in

  let padding =
    Resolve.resolve_or_zero_rect_with_option
      Resolve.resolve_or_zero_length_percentage style.padding
      (Some grid_area_size.width) calc
  in
  let border =
    Resolve.resolve_or_zero_rect_with_option
      Resolve.resolve_or_zero_length_percentage style.border
      (Some grid_area_size.width) calc
  in
  let padding_border_size = Rect.sum_axes (Rect.add padding border) in

  let box_sizing_adjustment =
    if style.box_sizing = Border_box then Size.zero else padding_border_size
  in

  let inherent_size =
    Size.maybe_apply_aspect_ratio
      (Size.maybe_add
         (Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension style.size
            {
              width = Some grid_area_size.width;
              height = Some grid_area_size.height;
            }
            calc)
         box_sizing_adjustment)
      aspect_ratio
  in
  let min_size =
    Size.maybe_apply_aspect_ratio
      (let base =
         Size.maybe_add
           (Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension
              style.min_size
              {
                width = Some grid_area_size.width;
                height = Some grid_area_size.height;
              }
              calc)
           box_sizing_adjustment
       in
       match (base.width, base.height) with
       | None, None ->
           {
             width = Some padding_border_size.width;
             height = Some padding_border_size.height;
           }
       | Some w, None ->
           {
             width = Some (max w padding_border_size.width);
             height = Some padding_border_size.height;
           }
       | None, Some h ->
           {
             width = Some padding_border_size.width;
             height = Some (max h padding_border_size.height);
           }
       | Some w, Some h ->
           {
             width = Some (max w padding_border_size.width);
             height = Some (max h padding_border_size.height);
           })
      aspect_ratio
  in
  let max_size =
    Size.maybe_apply_aspect_ratio
      (Size.maybe_add
         (Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension
            style.max_size
            {
              width = Some grid_area_size.width;
              height = Some grid_area_size.height;
            }
            calc)
         box_sizing_adjustment)
      aspect_ratio
  in

  (* Resolve default alignment styles if they are set on neither the parent or the node itself
     Note: if the child has a preferred aspect ratio but neither width or height are set, then the width is stretched
     and the then height is calculated from the width according the aspect ratio
     See: https://www.w3.org/TR/css-grid-1/#grid-item-sizing *)
  let alignment_styles =
    {
      horizontal =
        (match (justify_self, container_alignment_styles.horizontal) with
        | Some js, _ -> js
        | None, Some js -> js
        | None, None ->
            if
              (match inherent_size.width with Some _ -> true | None -> false)
              || position = Absolute
            then Style.Alignment.Start
            else Alignment.Stretch);
      vertical =
        (match (align_self, container_alignment_styles.vertical) with
        | Some as_, _ -> as_
        | None, Some as_ -> as_
        | None, None ->
            if
              (match inherent_size.height with Some _ -> true | None -> false)
              || (match aspect_ratio with Some _ -> true | None -> false)
              || position = Absolute
            then Style.Alignment.Start
            else Alignment.Stretch);
    }
  in

  (* Note: This is not a bug. It is part of the CSS spec that both horizontal and vertical margins
     resolve against the WIDTH of the grid area. *)
  let margin =
    let resolve m =
      Resolve.maybe_resolve_length_percentage_auto m (Some grid_area_size.width)
        calc
    in
    {
      left = resolve style.margin.left;
      right = resolve style.margin.right;
      top = resolve style.margin.top;
      bottom = resolve style.margin.bottom;
    }
  in

  let grid_area_minus_item_margins_size =
    {
      width =
        (match (margin.left, margin.right) with
        | Some l, Some r -> grid_area_size.width -. l -. r
        | Some l, None -> grid_area_size.width -. l
        | None, Some r -> grid_area_size.width -. r
        | None, None -> grid_area_size.width);
      height =
        (match (margin.top, margin.bottom) with
        | Some t, Some b -> grid_area_size.height -. t -. b -. baseline_shim
        | Some t, None -> grid_area_size.height -. t -. baseline_shim
        | None, Some b -> grid_area_size.height -. b -. baseline_shim
        | None, None -> grid_area_size.height -. baseline_shim);
    }
  in

  (* If node is absolutely positioned and width is not set explicitly, then deduce it
     from left, right and container_content_box if both are set. *)
  let width =
    match inherent_size.width with
    | Some w -> Some w
    | None ->
        (* Apply width derived from both the left and right properties of an absolutely
           positioned element being set *)
        if position = Absolute then
          match (inset_horizontal.start, inset_horizontal.end_) with
          | Some left, Some right ->
              Some
                (max
                   (grid_area_minus_item_margins_size.width -. left -. right)
                   0.0)
          | _ -> None
          (* Apply width based on stretch alignment if:
           - Alignment style is "stretch"
           - The node is not absolutely positioned
           - The node does not have auto margins in this axis. *)
        else if
          (fun x -> match x with Some _ -> true | None -> false) margin.left
          && (fun x -> match x with Some _ -> true | None -> false)
               margin.right
          && alignment_styles.horizontal = Alignment.Stretch
          && position <> Absolute
        then Some grid_area_minus_item_margins_size.width
        else None
  in

  (* Reapply aspect ratio after stretch and absolute position width adjustments *)
  let size_after_width =
    Size.maybe_apply_aspect_ratio
      { width; height = inherent_size.height }
      aspect_ratio
  in

  let height =
    match size_after_width.height with
    | Some h -> Some h
    | None ->
        if position = Absolute then
          match (inset_vertical.start, inset_vertical.end_) with
          | Some top, Some bottom ->
              Some
                (max
                   (grid_area_minus_item_margins_size.height -. top -. bottom)
                   0.0)
          | _ -> None
          (* Apply height based on stretch alignment if:
           - Alignment style is "stretch"
           - The node is not absolutely positioned
           - The node does not have auto margins in this axis. *)
        else if
          (fun x -> match x with Some _ -> true | None -> false) margin.top
          && (fun x -> match x with Some _ -> true | None -> false)
               margin.bottom
          && alignment_styles.vertical = Alignment.Stretch
          && position <> Absolute
        then Some grid_area_minus_item_margins_size.height
        else None
  in

  (* Reapply aspect ratio after stretch and absolute position height adjustments *)
  let size_after_height =
    Size.maybe_apply_aspect_ratio
      { width = size_after_width.width; height }
      aspect_ratio
  in

  (* Clamp size by min and max width/height *)
  let clamped_size = Size.maybe_clamp size_after_height min_size max_size in

  (* Layout node *)
  let layout_output : Layout_output.t =
    perform_child_layout node clamped_size
      { width = Some grid_area_size.width; height = Some grid_area_size.height }
      {
        width = Available_space.Definite grid_area_minus_item_margins_size.width;
        height =
          Available_space.Definite grid_area_minus_item_margins_size.height;
      }
      Sizing_mode.Inherent_size
      { start = false; end_ = false }
  in

  (* Resolve final size *)
  let final_size =
    (* In Rust: Size { width, height }.unwrap_or(layout_output.size).maybe_clamp(min_size, max_size) *)
    let base =
      match (clamped_size.width, clamped_size.height) with
      | Some w, Some h -> { width = w; height = h }
      | Some w, None -> { width = w; height = layout_output.size.height }
      | None, Some h -> { width = layout_output.size.width; height = h }
      | None, None -> layout_output.size
    in
    Size.maybe_clamp (Size.map base ~f:(fun x -> Some x)) min_size max_size
    |> Size.map ~f:(function Some v -> v | None -> 0.0)
  in

  let x, x_margin =
    align_item_within_area
      ~grid_area:{ start = grid_area.left; end_ = grid_area.right }
      ~alignment_style:
        (match justify_self with
        | Some js -> js
        | None -> alignment_styles.horizontal)
      ~resolved_size:final_size.width ~position ~inset:inset_horizontal
      ~margin:{ start = margin.left; end_ = margin.right }
      ~baseline_shim:0.0
  in

  let y, y_margin =
    align_item_within_area
      ~grid_area:{ start = grid_area.top; end_ = grid_area.bottom }
      ~alignment_style:
        (match align_self with
        | Some as_ -> as_
        | None -> alignment_styles.vertical)
      ~resolved_size:final_size.height ~position ~inset:inset_vertical
      ~margin:{ start = margin.top; end_ = margin.bottom }
      ~baseline_shim
  in

  let scrollbar_size =
    {
      width = (if overflow.y = Scroll then scrollbar_width else 0.0);
      height = (if overflow.x = Scroll then scrollbar_width else 0.0);
    }
  in

  let resolved_margin =
    {
      left = x_margin.start;
      right = x_margin.end_;
      top = y_margin.start;
      bottom = y_margin.end_;
    }
  in

  set_unrounded_layout node
    (let open Layout in
     {
       order = Int32.to_int order;
       location = { x; y };
       size = final_size;
       content_size = layout_output.size;
       scrollbar_size;
       padding;
       border;
       margin = resolved_margin;
     });

  (* @feature content_size *)
  let contribution =
    Content_size.compute_content_size_contribution ~location:{ x; y }
      ~size:final_size ~content_size:layout_output.size ~overflow
  in

  (contribution, y, final_size.height)
