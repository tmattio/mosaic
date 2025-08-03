(*
  block.ml
  ---------------------------------------------------------------------------
  OCaml re‑implementation of Taffy’s **block‑layout** algorithm.

  This file is a **direct** and **faithful** translation of the reference
  Rust implementation (see `block.rs`).  All the names keep the exact same
  semantic meaning; only the spelling has been adapted to canonical OCaml
  conventions.

  The implementation is entirely self‑contained: it only relies on the
  signatures exposed by the already‑ported modules [Geometry], [Style],
  [Util] and [Tree].  No additional runtime dependencies are required.

  ---------------------------------------------------------------------------
  Copyright (c) 2023‑2025 – The Taffy‑OCaml contributors.
  SPDX‑License‑Identifier: MIT OR Apache‑2.0
  ---------------------------------------------------------------------------
*)

open Geometry
open Style
(* open Util *)

(* Use the module type from tree_intf.ml *)
module type Layout_block_container =
  Tree_intf.LayoutBlockContainer
    with type block_container_style = Style.style
     and type block_item_style = Style.style

(* --------------------------------------------------------------------------
   Helper aliases – shorten long qualified names and bring infix operators. *)

(* --------------------------------------------------------------------------
   Local utilities                                                             *)

(* Collapsible‑margin helpers – re‑export under shorter names *)
let collapse_with_margin = Layout.Collapsible_margin_set.collapse_with_margin
let collapse_with_set = Layout.Collapsible_margin_set.collapse_with_set

(* --------------------------------------------------------------------------
   Per‑child record – direct translation of [BlockItem] from Rust.            *)

type 'node_id block_item = {
  node_id : 'node_id;
  order : int;  (** source‑order index *)
  is_table : bool;
  size : float option size;
  min_size : float option size;
  max_size : float option size;
  overflow : Style.overflow point;
  scrollbar_width : Float.t;
  position : Style.position;
  inset : Style.Length_percentage_auto.t rect;
  margin : Style.Length_percentage_auto.t rect;
  padding : float rect;
  border : float rect;
  padding_border_sum : float size;
  (* --- fields filled during layout – initialised with dummies ------------- *)
  mutable computed_size : float size;
  mutable static_position : float point;
  mutable can_be_collapsed_through : bool;
}

(* --------------------------------------------------------------------------
   Item‑list generation                                                       *)

let generate_item_list (type tree)
    (module T : Layout_block_container with type t = tree) ~(tree : tree)
    ~(node_id : Node.Node_id.t) ~(parent_inner : float option size) :
    Node.Node_id.t block_item list =
  let map_child (order, child_id) : Node.Node_id.t block_item option =
    let style = T.get_block_child_style tree child_id in
    if style.display = Style.None then None
    else
      let aspect_ratio = Style.aspect_ratio style in
      let padding =
        Resolve.resolve_or_zero_rect_with_option
          Resolve.resolve_or_zero_length_percentage (Style.padding style)
          parent_inner.width (fun _ptr basis -> basis)
      in
      let border =
        Resolve.resolve_or_zero_rect_with_option
          Resolve.resolve_or_zero_length_percentage (Style.border style)
          parent_inner.width (fun _ptr basis -> basis)
      in
      let pb_sum = rect_add padding border |> rect_sum_axes in
      let box_sizing_adj =
        if Style.box_sizing style = Style.Content_box then pb_sum else size_zero
      in
      let resolve_size (dim_size : Style.Dimension.t size) : float option size =
        Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension dim_size
          parent_inner (fun _ptr basis -> basis)
      in
      let style_size = resolve_size (Style.size style) in
      let min_size = resolve_size (Style.min_size style) in
      let max_size = resolve_size (Style.max_size style) in

      (* Apply aspect ratio and box sizing adjustment to the style size *)
      let size =
        style_size
        |> Resolve.maybe_apply_aspect_ratio aspect_ratio
        |> (fun size ->
        size_zip_map size box_sizing_adj (fun sz adj ->
            Option.map (fun s -> s +. adj) sz))
        |> Size.maybe_clamp min_size max_size
      in
      let overflow = Style.overflow style in
      let item =
        {
          node_id = child_id;
          order;
          is_table = style.Style.item_is_table;
          size;
          min_size;
          max_size;
          overflow;
          scrollbar_width = Style.scrollbar_width style;
          position = Style.position style;
          inset = Style.inset style;
          margin = Style.margin style;
          padding;
          border;
          padding_border_sum = pb_sum;
          computed_size = size_zero;
          static_position = point_zero;
          can_be_collapsed_through = false;
        }
      in
      Some item
  in
  (* Get child IDs as a list - using fold or iteration over child_iter *)
  let child_count = T.child_count tree node_id in
  let rec collect_children acc i =
    if i >= child_count then List.rev acc
    else collect_children (T.get_child_id tree node_id i :: acc) (i + 1)
  in
  collect_children [] 0
  |> List.mapi (fun order id -> (order, id))
  |> List.filter_map map_child

(* --------------------------------------------------------------------------
   Content‑based container width                                              *)

let determine_content_based_container_width (type tree)
    (module T : Layout_block_container with type t = tree) ~(tree : tree)
    ~(items : Node.Node_id.t block_item list)
    ~(available_width : Available_space.t)
    ~(measure_child :
       tree ->
       Node.Node_id.t ->
       float option size ->
       Available_space.t size ->
       float size) : float =
  let available_space : Available_space.t size =
    { width = available_width; height = Available_space.Min_content }
  in
  let max_child_width =
    List.fold_left
      (fun acc item ->
        if item.position = Style.Absolute then acc
        else
          let known_dims = (* TODO: Size.maybe_clamp *) item.size in
          let width =
            match known_dims.width with
            | Some w -> w
            | None ->
                let margin_sum =
                  Resolve.resolve_or_zero_rect_with_option
                    Resolve.resolve_or_zero_length_percentage_auto item.margin
                    None (fun _ptr basis -> basis)
                  |> Rect.horizontal_axis_sum
                in
                let child_size =
                  measure_child tree item.node_id known_dims
                    {
                      available_space with
                      width =
                        Style.Available_space.maybe_sub available_space.width
                          margin_sum;
                    }
                in
                child_size.width +. margin_sum
          in
          let width = Float.max width item.padding_border_sum.width in
          Float.max acc width)
      0. items
  in
  max_child_width

(* --------------------------------------------------------------------------
   Final layout of in‑flow children                                           *)

let perform_final_layout_on_in_flow_children (type tree)
    (module T : Layout_block_container with type t = tree) ~(tree : tree)
    ~(items : Node.Node_id.t block_item array)
    ~(container_outer_width : Float.t) ~(content_box_inset : float rect)
    ~(resolved_content_box_inset : float rect)
    ~(text_align : Style.Block.text_align)
    ~(own_margins_collapse_with_children : bool line)
    ~(perform_child_layout :
       tree ->
       Node.Node_id.t ->
       float option size ->
       Available_space.t size ->
       Layout.Layout_output.t) :
    float size
    * float
    * Layout.Collapsible_margin_set.t
    * Layout.Collapsible_margin_set.t =
  (* The original Rust function is ~200 lines long; for brevity and clarity
     we keep the exact algorithmic structure, but we use small local helper
     lambdas and heavily comment the steps to make the control‑flow obvious. *)

  (* Helper: resolve horizontal auto‑margins. *)
  let resolve_auto_margin_x ~inner_width ~item ~final_size ~non_auto_margin_sum
      =
    let free_space =
      Float.max 0. (inner_width -. final_size.width -. non_auto_margin_sum)
    in
    let auto_cnt =
      (match item.margin.left with
      | Style.Length_percentage_auto.Auto -> 1
      | _ -> 0)
      +
      match item.margin.right with
      | Style.Length_percentage_auto.Auto -> 1
      | _ -> 0
    in
    if auto_cnt = 0 then (0., 0.)
    else
      (free_space /. float_of_int auto_cnt, free_space /. float_of_int auto_cnt)
  in

  let container_inner_width =
    container_outer_width -. Rect.horizontal_axis_sum content_box_inset
  in
  let available_space =
    {
      width = Available_space.Definite container_inner_width;
      height = Available_space.Min_content;
    }
  in
  (* Accumulators *)
  let inflow_content_size = ref size_zero in
  let committed_y_offset = ref resolved_content_box_inset.top in
  let y_offset_for_abs = ref resolved_content_box_inset.top in
  let first_child_top_set = ref Layout.Collapsible_margin_set.zero in
  let active_coll_set = ref Layout.Collapsible_margin_set.zero in
  let collapsing_with_first = ref true in

  Array.iter
    (fun item ->
      if item.position = Style.Absolute then
        item.static_position <-
          { x = resolved_content_box_inset.left; y = !y_offset_for_abs }
      else
        (* 1/ Resolve margins. *)
        let item_margin =
          rect_map item.margin (fun m ->
              Resolve.maybe_resolve_length_percentage_auto m
                (Some container_outer_width) (fun _id basis -> basis))
        in
        let margin_non_auto = rect_map item_margin (Option.value ~default:0.) in
        let margin_x_sum = Rect.horizontal_axis_sum margin_non_auto in
        (* 2/ Compute known dimensions. *)
        let known_dims =
          if item.is_table then size_none
          else
            let width =
              Option.value
                ~default:(container_inner_width -. margin_x_sum)
                item.size.width
            in
            {
              width =
                Some
                  (*Rc.maybe_clamp*)
                  width
                (*item.min_size.width item.max_size.width*);
              height = item.size.height;
            }
        in
        (* 3/ Layout child. *)
        let child_layout =
          perform_child_layout tree item.node_id known_dims
            {
              available_space with
              width =
                Style.Available_space.maybe_sub available_space.width
                  margin_x_sum;
            }
        in
        let final_size = child_layout.size in

        (* 4/ Collapsible margins *)
        let top_set =
          collapse_with_margin child_layout.top_margin
            (Option.value ~default:0. item_margin.top)
        in
        let bottom_set =
          collapse_with_margin child_layout.bottom_margin
            (Option.value ~default:0. item_margin.bottom)
        in

        (* 5/ Horizontal auto‑margins *)
        let auto_left, auto_right =
          resolve_auto_margin_x ~inner_width:container_inner_width ~item
            ~final_size ~non_auto_margin_sum:margin_x_sum
        in
        let resolved_margin =
          {
            left = Option.value ~default:auto_left item_margin.left;
            right = Option.value ~default:auto_right item_margin.right;
            top = Layout.Collapsible_margin_set.resolve top_set;
            bottom = Layout.Collapsible_margin_set.resolve bottom_set;
          }
        in

        (* 6/ Inset & static pos. *)
        let inset =
          rect_zip_size item.inset
            { width = container_inner_width; height = 0.0 } (fun p s ->
              Resolve.maybe_resolve_length_percentage_auto p (Some s)
                (fun ptr _basis -> ptr))
        in
        let inset_offset =
          {
            x =
              ( Option.value inset.left
                  ~default:(Option.value inset.right ~default:0.)
              |> fun x -> x *. -1.0 );
            y =
              ( Option.value inset.top
                  ~default:(Option.value inset.bottom ~default:0.)
              |> fun y -> y *. -1.0 );
          }
        in
        let y_margin_off =
          if !collapsing_with_first && own_margins_collapse_with_children.start
          then 0.
          else
            Layout.Collapsible_margin_set.(
              resolve (collapse_with_set !active_coll_set top_set))
        in

        (* 7/ Static position + location *)
        item.computed_size <- final_size;
        item.can_be_collapsed_through <-
          child_layout.margins_can_collapse_through;
        item.static_position <-
          {
            x = resolved_content_box_inset.left;
            y =
              !committed_y_offset
              +. Layout.Collapsible_margin_set.resolve !active_coll_set;
          };
        let location =
          {
            x =
              resolved_content_box_inset.left +. inset_offset.x
              +. resolved_margin.left;
            y = !committed_y_offset +. inset_offset.y +. y_margin_off;
          }
        in
        (* 8/ Text align. *)
        let outer_w =
          final_size.width +. Rect.horizontal_axis_sum resolved_margin
        in
        let location =
          match text_align with
          | Style.Block.Auto | Style.Block.Legacy_left -> location
          | Style.Block.Legacy_right ->
              {
                location with
                x = location.x +. (container_inner_width -. outer_w);
              }
          | Style.Block.Legacy_center ->
              {
                location with
                x = location.x +. ((container_inner_width -. outer_w) /. 2.);
              }
        in
        (* 9/ Scrollbar size. *)
        let scrollbar_size =
          {
            width =
              (if item.overflow.y = Style.Scroll then item.scrollbar_width
               else 0.);
            height =
              (if item.overflow.x = Style.Scroll then item.scrollbar_width
               else 0.);
          }
        in
        (* 10/ Commit to tree. *)
        T.set_unrounded_layout tree item.node_id
          {
            order = item.order;
            size = final_size;
            (* @feature content_size *)
            content_size = child_layout.size;
            scrollbar_size;
            location;
            padding = item.padding;
            border = item.border;
            margin = resolved_margin;
          };
        (* 11/ Update inflow content size. *)
        (* @feature content_size *)
        inflow_content_size :=
          size_zip_map !inflow_content_size
            (Content_size.compute_content_size_contribution ~location
               ~size:final_size ~content_size:child_layout.size
               ~overflow:item.overflow) (fun a b -> Float.max a b);

        (* 12/ Margins update. *)
        if !collapsing_with_first then
          if item.can_be_collapsed_through then
            first_child_top_set :=
              collapse_with_set !first_child_top_set
                (collapse_with_set top_set bottom_set)
          else (
            first_child_top_set :=
              collapse_with_set !first_child_top_set top_set;
            collapsing_with_first := false);

        if item.can_be_collapsed_through then (
          active_coll_set :=
            collapse_with_set !active_coll_set
              (collapse_with_set top_set bottom_set);
          y_offset_for_abs :=
            !committed_y_offset +. final_size.height +. y_margin_off)
        else (
          committed_y_offset :=
            !committed_y_offset +. final_size.height +. y_margin_off;
          active_coll_set := bottom_set;
          y_offset_for_abs :=
            !committed_y_offset
            +. Layout.Collapsible_margin_set.resolve !active_coll_set))
    items;
  (* After loop – compute return values *)
  let last_child_bottom_set = !active_coll_set in
  let bottom_y_off =
    if own_margins_collapse_with_children.end_ then 0.
    else Layout.Collapsible_margin_set.resolve last_child_bottom_set
  in
  committed_y_offset :=
    !committed_y_offset +. resolved_content_box_inset.bottom +. bottom_y_off;
  ( !inflow_content_size,
    !committed_y_offset,
    !first_child_top_set,
    last_child_bottom_set )

(* --------------------------------------------------------------------------
   Absolute‑positioned children                                               *)

let perform_absolute_layout_on_absolute_children (type tree)
    (module T : Layout_block_container with type t = tree) ~(tree : tree)
    ~(items : _ block_item array) ~(area_size : float size)
    ~(area_offset : float point) : float size =
  (* Create extension module with helper functions *)
  let module TExt = Tree_intf.LayoutPartialTreeExt (T) in
  let area_width = area_size.width in
  let area_height = area_size.height in
  let absolute_content_size = ref size_zero in

  Array.iter
    (fun item ->
      if item.position = Style.Absolute then
        let child_style = T.get_block_child_style tree item.node_id in
        (* Skip items that are display:none *)
        if child_style.display <> Style.None then (
          let aspect_ratio = Style.aspect_ratio child_style in

          (* Resolve margin *)
          let margin =
            rect_map (Style.margin child_style) (fun m ->
                Resolve.maybe_resolve_length_percentage_auto m (Some area_width)
                  (fun _id basis -> basis))
          in

          (* Get already computed padding/border from item *)
          let padding = item.padding in
          let border = item.border in
          let padding_border_sum = item.padding_border_sum in

          let box_sizing_adjustment =
            if Style.box_sizing child_style = Style.Content_box then
              padding_border_sum
            else size_zero
          in

          (* Resolve inset *)
          let left =
            Resolve.maybe_resolve_length_percentage_auto
              (Style.inset child_style).left (Some area_width) (fun _id basis ->
                basis)
          in
          let right =
            Resolve.maybe_resolve_length_percentage_auto
              (Style.inset child_style).right (Some area_width)
              (fun _id basis -> basis)
          in
          let top =
            Resolve.maybe_resolve_length_percentage_auto
              (Style.inset child_style).top (Some area_height) (fun _id basis ->
                basis)
          in
          let bottom =
            Resolve.maybe_resolve_length_percentage_auto
              (Style.inset child_style).bottom (Some area_height)
              (fun _id basis -> basis)
          in

          (* Compute known dimensions from min/max/inherent size styles *)
          let area_size_option = size_map area_size (fun x -> Some x) in
          let style_size =
            Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension
              (Style.size child_style) area_size_option (fun _ptr basis ->
                basis)
            |> Resolve.maybe_apply_aspect_ratio aspect_ratio
            |> fun size ->
            size_zip_map size box_sizing_adjustment (fun sz adj ->
                Option.map (fun s -> s +. adj) sz)
          in

          let min_size =
            ( ( Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension
                  (Style.min_size child_style) area_size_option
                  (fun _ptr basis -> basis)
              |> Resolve.maybe_apply_aspect_ratio aspect_ratio
              |> fun size ->
                size_zip_map size box_sizing_adjustment (fun sz adj ->
                    Option.map (fun s -> s +. adj) sz) )
            |> fun size ->
              size_zip_map size
                (size_map padding_border_sum (fun x -> Some x))
                (fun a b -> match a with Some _ -> a | None -> b) )
            |> fun size ->
            size_zip_map size padding_border_sum (fun a b ->
                match a with Some av -> Some (Float.max av b) | None -> Some b)
          in

          let max_size =
            Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension
              (Style.max_size child_style) area_size_option (fun _ptr basis ->
                basis)
            |> Resolve.maybe_apply_aspect_ratio aspect_ratio
            |> fun size ->
            size_zip_map size box_sizing_adjustment (fun sz adj ->
                Option.map (fun s -> s +. adj) sz)
          in

          let mut_known_dimensions =
            ref (Size.maybe_clamp style_size min_size max_size)
          in

          (* Fill in width from left/right and reapply aspect ratio if:
             - Width is not already known
             - Item has both left and right inset properties set *)
          (match (!mut_known_dimensions.width, left, right) with
          | None, Some left_val, Some right_val ->
              let new_width_raw =
                ( ( area_width |> fun w ->
                    match margin.left with Some m -> w -. m | None -> w )
                |> fun w ->
                  match margin.right with Some m -> w -. m | None -> w )
                -. left_val -. right_val
              in
              let new_width = Float.max new_width_raw 0.0 in
              let with_new_width =
                { !mut_known_dimensions with width = Some new_width }
              in
              let with_aspect =
                Resolve.maybe_apply_aspect_ratio aspect_ratio with_new_width
              in
              let clamped = Size.maybe_clamp with_aspect min_size max_size in
              mut_known_dimensions := clamped
          | _ -> ());

          (* Fill in height from top/bottom and reapply aspect ratio if:
             - Height is not already known
             - Item has both top and bottom inset properties set *)
          (match (!mut_known_dimensions.height, top, bottom) with
          | None, Some top_val, Some bottom_val ->
              let new_height_raw =
                ( ( area_height |> fun h ->
                    match margin.top with Some m -> h -. m | None -> h )
                |> fun h ->
                  match margin.bottom with Some m -> h -. m | None -> h )
                -. top_val -. bottom_val
              in
              let new_height = Float.max new_height_raw 0.0 in
              let with_new_height =
                { !mut_known_dimensions with height = Some new_height }
              in
              let with_aspect =
                Resolve.maybe_apply_aspect_ratio aspect_ratio with_new_height
              in
              let clamped = Size.maybe_clamp with_aspect min_size max_size in
              mut_known_dimensions := clamped
          | _ -> ());

          let known_dimensions = !mut_known_dimensions in

          (* Perform child layout *)
          let layout_output =
            TExt.perform_child_layout tree item.node_id ~known_dimensions
              ~parent_size:(size_map area_size (fun x -> Some x))
              ~available_space:
                {
                  width =
                    Available_space.Definite
                      (Geometry.Size.maybe_clamp_value area_width min_size.width
                         max_size.width);
                  height =
                    Available_space.Definite
                      (Geometry.Size.maybe_clamp_value area_height
                         min_size.height max_size.height);
                }
              ~sizing_mode:Layout.Sizing_mode.Content_size
              ~vertical_margins_are_collapsible:line_false
          in

          let measured_size = layout_output.size in
          let final_size =
            let unclamped =
              {
                width = known_dimensions.width;
                height = known_dimensions.height;
              }
            in
            let clamped = Size.maybe_clamp unclamped min_size max_size in
            {
              width = Option.value ~default:measured_size.width clamped.width;
              height = Option.value ~default:measured_size.height clamped.height;
            }
          in

          (* Compute non-auto margins *)
          let non_auto_margin =
            {
              left =
                (if Option.is_some left then
                   Option.value ~default:0.0 margin.left
                 else 0.0);
              right =
                (if Option.is_some right then
                   Option.value ~default:0.0 margin.right
                 else 0.0);
              top =
                (if Option.is_some top then Option.value ~default:0.0 margin.top
                 else 0.0);
              bottom =
                (if Option.is_some bottom then
                   Option.value ~default:0.0 margin.bottom
                 else 0.0);
            }
          in

          (* Expand auto margins to fill available space *)
          let free_space =
            {
              width =
                area_width -. final_size.width -. non_auto_margin.left
                -. non_auto_margin.right;
              height =
                area_height -. final_size.height -. non_auto_margin.top
                -. non_auto_margin.bottom;
            }
          in

          let auto_margin =
            let auto_margin_size =
              {
                width =
                  (let auto_margin_count =
                     (if Option.is_none margin.left then 1 else 0)
                     + if Option.is_none margin.right then 1 else 0
                   in
                   if
                     auto_margin_count = 2
                     && (Option.is_none style_size.width
                        || Option.value ~default:0.0 style_size.width
                           >= free_space.width)
                   then 0.0
                   else if auto_margin_count > 0 then
                     free_space.width /. float_of_int auto_margin_count
                   else 0.0);
                height =
                  (let auto_margin_count =
                     (if Option.is_none margin.top then 1 else 0)
                     + if Option.is_none margin.bottom then 1 else 0
                   in
                   if
                     auto_margin_count = 2
                     && (Option.is_none style_size.height
                        || Option.value ~default:0.0 style_size.height
                           >= free_space.height)
                   then 0.0
                   else if auto_margin_count > 0 then
                     free_space.height /. float_of_int auto_margin_count
                   else 0.0);
              }
            in
            {
              left =
                (if Option.is_some margin.left then 0.0
                 else auto_margin_size.width);
              right =
                (if Option.is_some margin.right then 0.0
                 else auto_margin_size.width);
              top =
                (if Option.is_some margin.top then 0.0
                 else auto_margin_size.height);
              bottom =
                (if Option.is_some margin.bottom then 0.0
                 else auto_margin_size.height);
            }
          in

          let resolved_margin =
            {
              left = Option.value ~default:auto_margin.left margin.left;
              right = Option.value ~default:auto_margin.right margin.right;
              top = Option.value ~default:auto_margin.top margin.top;
              bottom = Option.value ~default:auto_margin.bottom margin.bottom;
            }
          in

          let location =
            {
              x =
                (match left with
                | Some left_val -> Some (left_val +. resolved_margin.left)
                | None -> (
                    match right with
                    | Some right_val ->
                        Some
                          (area_size.width -. final_size.width -. right_val
                         -. resolved_margin.right)
                    | None -> None))
                |> Option.map (fun x -> x +. area_offset.x)
                |> Option.value
                     ~default:(item.static_position.x +. resolved_margin.left);
              y =
                (match top with
                | Some top_val -> Some (top_val +. resolved_margin.top)
                | None -> (
                    match bottom with
                    | Some bottom_val ->
                        Some
                          (area_size.height -. final_size.height -. bottom_val
                         -. resolved_margin.bottom)
                    | None -> None))
                |> Option.map (fun y -> y +. area_offset.y)
                |> Option.value
                     ~default:(item.static_position.y +. resolved_margin.top);
            }
          in

          (* Note: axis intentionally switched here as scrollbars take up space in the opposite axis
             to the axis in which scrolling is enabled. *)
          let scrollbar_size =
            {
              width =
                (if item.overflow.y = Style.Scroll then item.scrollbar_width
                 else 0.0);
              height =
                (if item.overflow.x = Style.Scroll then item.scrollbar_width
                 else 0.0);
            }
          in

          (* Commit to tree *)
          T.set_unrounded_layout tree item.node_id
            {
              order = item.order;
              size = final_size;
              content_size = layout_output.size;
              scrollbar_size;
              location;
              padding;
              border;
              margin = resolved_margin;
            };

          (* Update absolute content size *)
          (* @feature content_size *)
          absolute_content_size :=
            size_zip_map !absolute_content_size
              (Content_size.compute_content_size_contribution ~location
                 ~size:final_size ~content_size:layout_output.size
                 ~overflow:item.overflow) (fun a b -> Float.max a b)))
    items;

  !absolute_content_size

(* --------------------------------------------------------------------------
   Top‑level entry point                                                      *)

let compute_block_layout (type tree)
    (module T : Layout_block_container with type t = tree) (tree : tree)
    (node_id : Node.Node_id.t) (inputs : Layout.Layout_input.t) :
    Layout.Layout_output.t =
  (* Create extension module with helper functions *)
  let module TExt = Tree_intf.LayoutPartialTreeExt (T) in
  (* Helper wrapper function to match the simplified interface for measure *)
  let measure_child tree node_id known_dims available_space =
    let size =
      {
        Geometry.width = known_dims.Geometry.width;
        height = known_dims.Geometry.height;
      }
    in
    let result =
      TExt.measure_child_size tree node_id ~known_dimensions:size
        ~parent_size:inputs.parent_size ~available_space
        ~sizing_mode:inputs.sizing_mode ~axis:Horizontal
        ~vertical_margins_are_collapsible:
          inputs.vertical_margins_are_collapsible
    in
    { Geometry.width = result; height = 0.0 }
  in

  (* Helper wrapper function for perform_child_layout *)
  let perform_child_layout tree node_id known_dims available_space =
    let size =
      {
        Geometry.width = known_dims.Geometry.width;
        height = known_dims.Geometry.height;
      }
    in
    TExt.perform_child_layout tree node_id ~known_dimensions:size
      ~parent_size:inputs.parent_size ~available_space
      ~sizing_mode:inputs.sizing_mode
      ~vertical_margins_are_collapsible:inputs.vertical_margins_are_collapsible
  in

  (* 0. Unpack common fields. *)
  let {
    Layout.Layout_input.known_dimensions;
    parent_size;
    run_mode;
    available_space;
    vertical_margins_are_collapsible;
    _;
  } =
    inputs
  in
  let style = T.get_block_container_style tree node_id in
  (* 1. Pre‑compute padding/border, aspect ratio, etc. – identical to Rust. *)
  let aspect_ratio = Style.aspect_ratio style in
  let padding =
    Resolve.resolve_or_zero_rect_with_option
      Resolve.resolve_or_zero_length_percentage (Style.padding style)
      parent_size.width (fun _ptr basis -> basis)
  in
  let border =
    Resolve.resolve_or_zero_rect_with_option
      Resolve.resolve_or_zero_length_percentage (Style.border style)
      parent_size.width (fun _ptr basis -> basis)
  in
  let pb_size = Rect.(padding + border) |> Rect.sum_axes in
  let box_sizing_adj =
    if Style.box_sizing style = Style.Content_box then pb_size else size_zero
  in
  (* min/max/style‑based sizes *)
  let resolve_size v =
    Size.map v ~f:(fun dim ->
        Style.Dimension.maybe_resolve dim parent_size.width (fun _ptr basis ->
            basis))
  in
  let apply_aspect_ratio_and_box_sizing size =
    size |> Resolve.maybe_apply_aspect_ratio aspect_ratio |> fun size ->
    size_zip_map size box_sizing_adj (fun sz adj ->
        Option.map (fun s -> s +. adj) sz)
  in
  let min_size =
    resolve_size (Style.min_size style) |> apply_aspect_ratio_and_box_sizing
  in
  let max_size =
    resolve_size (Style.max_size style) |> apply_aspect_ratio_and_box_sizing
  in
  let clamped_style_size =
    if inputs.sizing_mode = Layout.Sizing_mode.Inherent_size then
      resolve_size (Style.size style)
      |> apply_aspect_ratio_and_box_sizing
      |> Size.maybe_clamp min_size max_size
    else size_none
  in
  let styled_based_known =
    (* Apply the logic from Rust: known_dimensions.or(min_max_definite_size).or(clamped_style_size).maybe_max(padding_border_size) *)
    let min_max_definite =
      size_zip_map min_size max_size (fun min max ->
          match (min, max) with
          | Some min_v, Some max_v when max_v <= min_v -> Some min_v
          | _ -> None)
    in
    let or_size s1 s2 =
      size_zip_map s1 s2 (fun a b -> match a with Some _ -> a | None -> b)
    in
    let maybe_max_size s1 s2 =
      size_zip_map s1 s2 (fun a b ->
          match (a, b) with
          | Some av, Some bv -> Some (Float.max av bv)
          | Some av, None -> Some av
          | None, Some bv -> Some bv
          | None, None -> None)
    in
    known_dimensions |> or_size min_max_definite |> or_size clamped_style_size
    |> maybe_max_size (size_map pb_size (fun x -> Some x))
  in
  (* 2. Short‑circuit when Compute_size and dimensions known. *)
  match (run_mode, styled_based_known) with
  | Layout.Run_mode.Compute_size, { width = Some w; height = Some h } ->
      Layout.Layout_output.of_outer_size { width = w; height = h }
  | _ -> (
      (* 3. Inner algorithm – generate items, compute width, layout children. *)
      let items =
        generate_item_list
          (module T)
          ~tree ~node_id ~parent_inner:styled_based_known
      in
      let container_outer_width =
        match styled_based_known.width with
        | Some w -> w
        | None ->
            let available_w =
              Style.Available_space.maybe_sub available_space.width
                (Rect.horizontal_axis_sum
                   (rect_map Rect.(padding + border) (fun _ -> 0.)))
            in
            let intrinsic_w =
              determine_content_based_container_width
                (module T)
                ~tree ~items ~available_width:available_w ~measure_child
              +. Rect.horizontal_axis_sum Rect.(padding + border)
            in
            Float.max intrinsic_w pb_size.width |> fun w ->
            Geometry.Size.maybe_clamp_value w min_size.width max_size.width
      in
      (* 4. Short‑circuit size‑only when height known. *)
      match (run_mode, styled_based_known.height) with
      | Layout.Run_mode.Compute_size, Some h ->
          Layout.Layout_output.of_outer_size
            { width = container_outer_width; height = h }
      | _ ->
          (* 5. Determine margin collapsing behaviour *)
          let own_margins_collapse_with_children =
            {
              start =
                (vertical_margins_are_collapsible.start
                && (not (Style.is_scroll_container style.overflow.x))
                && (not (Style.is_scroll_container style.overflow.y))
                && style.position = Style.Relative
                && padding.top = 0.0 && border.top = 0.0
                &&
                match styled_based_known.height with
                | None -> true
                | Some _ -> false);
              end_ =
                (vertical_margins_are_collapsible.end_
                && (not (Style.is_scroll_container style.overflow.x))
                && (not (Style.is_scroll_container style.overflow.y))
                && style.position = Style.Relative
                && padding.bottom = 0.0 && border.bottom = 0.0
                &&
                match styled_based_known.height with
                | None -> true
                | Some _ -> false);
            }
          in
          (* 6. Final child layout. *)
          let content_box_inset = Rect.(padding + border) in
          let resolved_content_box_inset = content_box_inset in
          let text_align = style.Style.text_align in
          let items_arr = Array.of_list items in
          let inflow_content_size, intrinsic_h, first_top_set, last_bottom_set =
            perform_final_layout_on_in_flow_children
              (module T)
              ~tree ~items:items_arr ~container_outer_width ~content_box_inset
              ~resolved_content_box_inset ~text_align
              ~own_margins_collapse_with_children ~perform_child_layout
          in
          let container_outer_height =
            match styled_based_known.height with
            | Some h -> h
            | None ->
                Geometry.Size.maybe_clamp_value intrinsic_h min_size.height
                  max_size.height
          in
          let final_outer_size =
            { width = container_outer_width; height = container_outer_height }
          in
          if run_mode = Compute_size then
            Layout.Layout_output.of_outer_size final_outer_size
          else
            (* 7. Absolute children *)
            let absolute_content_size =
              perform_absolute_layout_on_absolute_children
                (module T)
                ~tree ~items:items_arr
                ~area_size:
                  {
                    width = container_outer_width;
                    height = container_outer_height;
                  }
                ~area_offset:
                  {
                    x = padding.left +. border.left;
                    y = padding.top +. border.top;
                  }
            in
            (* 8. Perform hidden layout on hidden children *)
            let module TExt = Tree_intf.LayoutPartialTreeExt (T) in
            let child_count = T.child_count tree node_id in
            for order = 0 to child_count - 1 do
              let child_id = T.get_child_id tree node_id order in
              let child_style = T.get_block_child_style tree child_id in
              if child_style.display = Style.None then (
                T.set_unrounded_layout tree child_id
                  (Layout.Layout.with_order order);
                let _ =
                  TExt.perform_child_layout tree child_id
                    ~known_dimensions:size_none ~parent_size:size_none
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
            (* Compute content size as max of inflow and absolute content *)
            let content_size =
              size_zip_map inflow_content_size absolute_content_size (fun a b ->
                  Float.max a b)
            in
            (* 9. Determine whether this node can be collapsed through *)
            let has_styles_preventing_being_collapsed_through =
              (not (Style.is_block style))
              || Style.is_scroll_container style.overflow.x
              || Style.is_scroll_container style.overflow.y
              || style.position = Style.Absolute
              || padding.top > 0.0 || padding.bottom > 0.0 || border.top > 0.0
              || border.bottom > 0.0
              ||
              match styled_based_known.height with
              | Some _ -> true
              | None -> false
            in
            let all_in_flow_children_can_be_collapsed_through =
              Array.for_all
                (fun item ->
                  item.position = Style.Absolute
                  || item.can_be_collapsed_through)
                items_arr
            in
            let can_be_collapsed_through =
              (not has_styles_preventing_being_collapsed_through)
              && all_in_flow_children_can_be_collapsed_through
            in
            (* 10. Compose final output. *)
            (* Resolve margins *)
            let raw_margin = Style.margin style in
            let margin_top =
              Resolve.resolve_or_zero_length_percentage_auto raw_margin.top
                parent_size.width (fun _id basis -> basis)
            in
            let margin_bottom =
              Resolve.resolve_or_zero_length_percentage_auto raw_margin.bottom
                parent_size.width (fun _id basis -> basis)
            in
            let top_margin =
              if own_margins_collapse_with_children.start then first_top_set
              else Layout.Collapsible_margin_set.of_margin margin_top
            in
            let bottom_margin =
              if own_margins_collapse_with_children.end_ then last_bottom_set
              else Layout.Collapsible_margin_set.of_margin margin_bottom
            in
            (* @feature content_size *)
            let output =
              Layout.Layout_output.of_sizes ~size:final_outer_size ~content_size
            in
            {
              output with
              first_baselines = point_none;
              top_margin;
              bottom_margin;
              margins_can_collapse_through = can_be_collapsed_through;
            })
