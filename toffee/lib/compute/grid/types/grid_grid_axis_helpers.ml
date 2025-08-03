(** axis_helpers.ml
    ---------------------------------------------------------------------------
    Axis-related helper modules
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Geometry

module Absolute_axis = struct
  type t = absolute_axis = Horizontal | Vertical

  let other_axis = other_axis
end

module Abstract_axis = struct
  type t = abstract_axis = Inline | Block

  let other = abstract_other
  let as_abs_naive = abstract_as_abs_naive
  let set_size s axis v = size_set s axis v
  let get_size s axis = size_get s axis
end

module Size = struct
  type 'a t = 'a size = { width : 'a; height : 'a }

  let zero = size_zero
  let none = size_none
  let get s axis = size_get s axis
  let set s axis v = size_set s axis v
  let map s ~f = size_map s f

  let maybe_resolve s base _calc =
    let open Style.Dimension in
    {
      width =
        (match s.width with
        | Length px -> Some px
        | Percent pct -> Option.map (fun b -> b *. pct) base.width
        | Auto -> None);
      height =
        (match s.height with
        | Length px -> Some px
        | Percent pct -> Option.map (fun b -> b *. pct) base.height
        | Auto -> None);
    }

  let maybe_apply_aspect_ratio s aspect_ratio =
    match aspect_ratio with
    | None -> s
    | Some ratio -> (
        match (s.width, s.height) with
        | Some w, None -> { width = Some w; height = Some (w /. ratio) }
        | None, Some h -> { width = Some (h *. ratio); height = Some h }
        | _ -> s)

  let maybe_add s addition =
    match (s.width, s.height, addition.width, addition.height) with
    | Some w, Some h, add_w, add_h ->
        { width = Some (w +. add_w); height = Some (h +. add_h) }
    | Some w, None, add_w, _ -> { width = Some (w +. add_w); height = None }
    | None, Some h, _, add_h -> { width = None; height = Some (h +. add_h) }
    | None, None, _, _ -> s

  let maybe_sub s subtraction =
    match (s.width, s.height) with
    | Some w, Some h ->
        {
          width = Some (w -. subtraction.width);
          height = Some (h -. subtraction.height);
        }
    | Some w, None -> { width = Some (w -. subtraction.width); height = None }
    | None, Some h -> { width = None; height = Some (h -. subtraction.height) }
    | None, None -> s

  let maybe_clamp s min_size max_size =
    let clamp_dim dim min_dim max_dim =
      match (dim, min_dim, max_dim) with
      | Some d, Some min_d, Some max_d -> Some (max min_d (min d max_d))
      | Some d, Some min_d, None -> Some (max min_d d)
      | Some d, None, Some max_d -> Some (min d max_d)
      | _ -> dim
    in
    {
      width = clamp_dim s.width min_size.width max_size.width;
      height = clamp_dim s.height min_size.height max_size.height;
    }
end

module Rect = struct
  type 'a t = 'a rect = { left : 'a; right : 'a; top : 'a; bottom : 'a }

  let zero = rect_zero
  let add = rect_add
  let sum_axes = rect_sum_axes

  let resolve_or_zero r base _calc =
    let open Style.Dimension in
    let resolve_dim dim base_val =
      match dim with
      | Length px -> px
      | Percent pct ->
          Option.value (Option.map (fun b -> b *. pct) base_val) ~default:0.0
      | Auto -> 0.0
    in
    {
      left = resolve_dim r.left base.width;
      right = resolve_dim r.right base.width;
      top = resolve_dim r.top base.height;
      bottom = resolve_dim r.bottom base.height;
    }
end

module Point = struct
  type 'a t = 'a point = { x : 'a; y : 'a }

  let get p axis = point_get p axis
end

module Line = struct
  type 'a t = 'a line = { start : 'a; end_ : 'a }

  let false_ = line_false
end

module Overflow = struct
  open Style

  let maybe_into_automatic_min_size = function
    | Visible | Clip -> Some 0.0
    | Hidden | Scroll -> None
end

module Dimension = struct
  open Style.Dimension

  let is_auto = function Auto -> true | _ -> false

  let resolve_or_zero dim base _calc =
    match dim with
    | Length px -> px
    | Percent pct ->
        Option.value (Option.map (fun p -> p *. pct) base) ~default:0.0
    | Auto -> 0.0

  let maybe_resolve dim base _calc =
    match dim with
    | Length px -> Some px
    | Percent pct -> Option.map (fun p -> p *. pct) base
    | Auto -> None
end

module Option = struct
  let maybe_min a b =
    match (a, b) with
    | Some x, Some y -> Some (min x y)
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None
end
