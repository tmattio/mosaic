(*
  leaf.ml
  ---------------------------------------------------------------------------
  **Leaf layout**: computes the size of a node with *no* children, based on
  its styles and an external measure function (text/image).

  This is a full OCaml port of `leaf.rs` from the Taffy reference
  implementation.  It supports both `Compute_size` and `Perform_layout` run
  modes and correctly handles box‑sizing, aspect‑ratio, min/max‑size
  constraints, scrollbars, and collapsible margins.

  ---------------------------------------------------------------------------
  SPDX-License-Identifier: MIT OR Apache-2.0
  ---------------------------------------------------------------------------
*)

open Geometry
open Layout
(* open Util *)

(** Main entry point – see [Tree.compute_leaf_layout] in the `.mli`. *)
let compute_leaf_layout ~(inputs : Layout_input.t) ~(style : Style.style)
    ~(resolve_calc_value : unit -> float -> float)
    ~(measure_function :
       float option Geometry.size ->
       Style.Available_space.t Geometry.size ->
       float Geometry.size) : Layout_output.t =
  let open Layout_input in
  let {
    known_dimensions;
    parent_size;
    available_space;
    sizing_mode;
    run_mode;
    _;
  } =
    inputs
  in
  (* Convenience bindings *)
  let margin =
    Resolve.resolve_or_zero_rect_with_option
      Resolve.resolve_or_zero_length_percentage_auto (Style.margin style)
      parent_size.width resolve_calc_value
  in
  let padding =
    Resolve.resolve_or_zero_rect_with_option
      Resolve.resolve_or_zero_length_percentage (Style.padding style)
      parent_size.width resolve_calc_value
  in
  let border =
    Resolve.resolve_or_zero_rect_with_option
      Resolve.resolve_or_zero_length_percentage (Style.border style)
      parent_size.width resolve_calc_value
  in
  let pb_sum = Geometry.Rect.(padding + border) |> Geometry.Rect.sum_axes in
  let box_sizing_adjustment =
    if Style.box_sizing style = Style.Content_box then pb_sum else size_zero
  in
  (* Preferred / min / max sizes – vary depending on sizing mode. *)
  let aspect_ratio = Style.aspect_ratio style in
  let node_size, node_min_size, node_max_size, aspect_ratio =
    match sizing_mode with
    | Sizing_mode.Content_size -> (known_dimensions, size_none, size_none, None)
    | Inherent_size ->
        let resolve_size v =
          (* Resolve dimension size to concrete values *)
          let resolved =
            Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension v
              parent_size resolve_calc_value
          in
          resolved |> size_maybe_apply_aspect_ratio ~aspect_ratio |> fun s ->
          Size.maybe_add s box_sizing_adjustment
        in
        let size = resolve_size (Style.size style) in
        let min_size = resolve_size (Style.min_size style) in
        let max_size = resolve_size (Style.max_size style) in
        (Size.or_ known_dimensions size, min_size, max_size, aspect_ratio)
  in
  (* Scrollbar gutters *)
  let scrollbar_gutter =
    let overflow = Style.overflow style |> point_transpose in
    point_map overflow (function
      | Style.Scroll -> Style.scrollbar_width style
      | _ -> 0.)
  in
  let content_box_inset =
    let inset = Geometry.Rect.add padding border in
    {
      inset with
      right = inset.right +. scrollbar_gutter.x;
      bottom = inset.bottom +. scrollbar_gutter.y;
    }
  in
  (* Collapse‑through checks (same semantics as Rust). *)
  let has_styles_preventing_being_collapsed_through =
    (not (Style.is_block style))
    || (match Style.(overflow style).x with Style.Scroll -> true | _ -> false)
    || (match Style.(overflow style).y with Style.Scroll -> true | _ -> false)
    || Style.position style = Style.Absolute
    || padding.top > 0. || padding.bottom > 0. || border.top > 0.
    || border.bottom > 0.
    || (match node_size.height with Some h when h > 0. -> true | _ -> false)
    || match node_min_size.height with Some h when h > 0. -> true | _ -> false
  in
  (* Early‑return optimisation when both dimensions are known. *)
  match run_mode with
  | Run_mode.Compute_size when has_styles_preventing_being_collapsed_through
    -> (
      match node_size with
      | { width = Some w; height = Some h } ->
          let size = { width = w; height = h } in
          Layout_output.
            {
              size;
              first_baselines = point_none;
              top_margin = Collapsible_margin_set.zero;
              bottom_margin = Collapsible_margin_set.zero;
              margins_can_collapse_through = false;
            }
      | _ ->
          (* Continue with normal computation flow *)
          let available_space =
            let open Style.Available_space in
            {
              width =
                known_dimensions.width |> Option.map from
                |> Option.value ~default:available_space.width
                |> (fun av ->
                maybe_sub av (Geometry.Rect.horizontal_axis_sum margin))
                |> (fun av -> maybe_set av known_dimensions.width)
                |> (fun av -> maybe_set av node_size.width)
                |> map_definite_value ~f:(fun size ->
                       let clamped =
                         Geometry.Size.maybe_clamp_value size
                           node_min_size.width node_max_size.width
                       in
                       clamped
                       -. Geometry.Rect.horizontal_axis_sum content_box_inset);
              height =
                known_dimensions.height |> Option.map from
                |> Option.value ~default:available_space.height
                |> (fun av ->
                maybe_sub av (Geometry.Rect.vertical_axis_sum margin))
                |> (fun av -> maybe_set av known_dimensions.height)
                |> (fun av -> maybe_set av node_size.height)
                |> map_definite_value ~f:(fun size ->
                       let clamped =
                         Geometry.Size.maybe_clamp_value size
                           node_min_size.height node_max_size.height
                       in
                       clamped
                       -. Geometry.Rect.vertical_axis_sum content_box_inset);
            }
          in
          (* Measure / clamp. *)
          let measured_size =
            measure_function
              (match run_mode with
              | Compute_size -> known_dimensions
              | _ -> Size.none)
              available_space
          in
          let clamped_size =
            let default_size =
              size_add measured_size (Geometry.Rect.sum_axes content_box_inset)
            in
            let optional_size = Size.or_ known_dimensions node_size in
            let unwrapped_size =
              size_option_unwrap_or optional_size default_size
            in
            (* Convert float size to float option size for maybe_clamp *)
            let as_option_size =
              {
                width = Some unwrapped_size.width;
                height = Some unwrapped_size.height;
              }
            in
            Size.maybe_clamp as_option_size node_min_size node_max_size
            |> fun s -> size_option_unwrap_or s size_zero
          in
          let size =
            let height =
              Float.max clamped_size.height
                (match aspect_ratio with
                | Some ratio -> clamped_size.width /. ratio
                | None -> 0.)
            in
            let sized = { width = clamped_size.width; height } in
            (* Apply maybe_max by converting to option size *)
            let as_option =
              { width = Some sized.width; height = Some sized.height }
            in
            let pb_sum_option = Size.map pb_sum ~f:(fun x -> Some x) in
            Size.maybe_max as_option pb_sum_option |> fun s ->
            size_option_unwrap_or s size_zero
          in
          (* Final output. *)
          Layout_output.
            {
              size;
              first_baselines = point_none;
              top_margin = Collapsible_margin_set.zero;
              bottom_margin = Collapsible_margin_set.zero;
              margins_can_collapse_through =
                (not has_styles_preventing_being_collapsed_through)
                && size.height = 0. && measured_size.height = 0.;
            })
  | _ ->
      (* Available space for the measure function (complex spec logic…) *)
      let available_space =
        let open Style.Available_space in
        {
          width =
            known_dimensions.width |> Option.map from
            |> Option.value ~default:available_space.width
            |> (fun av_space ->
            Style.Available_space.maybe_sub av_space
              (Geometry.Rect.horizontal_axis_sum margin))
            |> (fun av_space ->
            Style.Available_space.maybe_set av_space known_dimensions.width)
            |> (fun av_space ->
            Style.Available_space.maybe_set av_space node_size.width)
            |> map_definite_value ~f:(fun size ->
                   let clamped =
                     Geometry.Size.maybe_clamp_value size node_min_size.width
                       node_max_size.width
                   in
                   clamped
                   -. Geometry.Rect.horizontal_axis_sum content_box_inset);
          height =
            known_dimensions.height |> Option.map from
            |> Option.value ~default:available_space.height
            |> (fun av_space ->
            Style.Available_space.maybe_sub av_space
              (Geometry.Rect.vertical_axis_sum margin))
            |> (fun av_space ->
            Style.Available_space.maybe_set av_space known_dimensions.height)
            |> (fun av_space ->
            Style.Available_space.maybe_set av_space node_size.height)
            |> map_definite_value ~f:(fun size ->
                   let clamped =
                     Size.maybe_clamp_value size node_min_size.height
                       node_max_size.height
                   in
                   clamped -. Geometry.Rect.vertical_axis_sum content_box_inset);
        }
      in
      (* Measure / clamp. *)
      let measured_size =
        measure_function
          (match run_mode with
          | Compute_size -> known_dimensions
          | _ -> Size.none)
          available_space
      in
      let clamped_size =
        let default_size =
          size_add measured_size (Geometry.Rect.sum_axes content_box_inset)
        in
        let optional_size = Size.or_ known_dimensions node_size in
        let unwrapped_size = size_option_unwrap_or optional_size default_size in
        (* Convert float size to float option size for maybe_clamp *)
        let as_option_size =
          {
            width = Some unwrapped_size.width;
            height = Some unwrapped_size.height;
          }
        in
        Size.maybe_clamp as_option_size node_min_size node_max_size |> fun s ->
        size_option_unwrap_or s size_zero
      in
      let size =
        let height =
          Float.max clamped_size.height
            (match aspect_ratio with
            | Some ratio -> clamped_size.width /. ratio
            | None -> 0.)
        in
        let sized = { width = clamped_size.width; height } in
        (* Apply maybe_max by converting to option size *)
        let as_option =
          { width = Some sized.width; height = Some sized.height }
        in
        let pb_sum_option = Size.map pb_sum ~f:(fun x -> Some x) in
        Size.maybe_max as_option pb_sum_option |> fun s ->
        size_option_unwrap_or s size_zero
      in
      (* Final output. *)
      Layout_output.
        {
          size;
          first_baselines = point_none;
          top_margin = Collapsible_margin_set.zero;
          bottom_margin = Collapsible_margin_set.zero;
          margins_can_collapse_through =
            (not has_styles_preventing_being_collapsed_through)
            && size.height = 0. && measured_size.height = 0.;
        }
