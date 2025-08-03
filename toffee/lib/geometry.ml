(* geometry.ml *)

(* ─────────────────  Types  ───────────────── *)

type absolute_axis = Horizontal | Vertical
type abstract_axis = Inline | Block
type 'a rect = { left : 'a; right : 'a; top : 'a; bottom : 'a }
type 'a size = { width : 'a; height : 'a }
type 'a point = { x : 'a; y : 'a }
type 'a line = { start : 'a; end_ : 'a }
type ('min, 'max) min_max = { min : 'min; max : 'max }

(* ─────────────────  Constants  ───────────────── *)

let rect_zero : float rect = { left = 0.; right = 0.; top = 0.; bottom = 0. }
let size_zero : float size = { width = 0.; height = 0. }
let size_none : float option size = { width = None; height = None }
let line_true : bool line = { start = true; end_ = true }
let line_false : bool line = { start = false; end_ = false }
let point_zero : float point = { x = 0.; y = 0. }
let point_none : float option point = { x = None; y = None }

(* ─────────────────  Axis helpers  ───────────────── *)

let other_axis = function Horizontal -> Vertical | Vertical -> Horizontal
let abstract_other = function Inline -> Block | Block -> Inline
let abstract_as_abs_naive = function Inline -> Horizontal | Block -> Vertical

(* ─────────────────  Axis-aware accessors  ───────────────── *)

let size_get_abs s = function Horizontal -> s.width | Vertical -> s.height

let rect_grid_axis_sum r = function
  | Horizontal -> r.left +. r.right
  | Vertical -> r.top +. r.bottom

let size_get s = function Inline -> s.width | Block -> s.height

let size_set s axis v =
  match axis with
  | Inline -> { s with width = v }
  | Block -> { s with height = v }

let point_get p = function Inline -> p.x | Block -> p.y

let point_set p axis v =
  match axis with Inline -> { p with x = v } | Block -> { p with y = v }

let point_transpose p = { x = p.y; y = p.x }

(* ─────────────────  Arithmetic helpers (float)  ───────────────── *)

let size_add a b = { width = a.width +. b.width; height = a.height +. b.height }
let size_sub a b = { width = a.width -. b.width; height = a.height -. b.height }

let rect_add a b =
  {
    left = a.left +. b.left;
    right = a.right +. b.right;
    top = a.top +. b.top;
    bottom = a.bottom +. b.bottom;
  }

let rect_horizontal_axis_sum r = r.left +. r.right
let rect_vertical_axis_sum r = r.top +. r.bottom

let rect_sum_axes r =
  { width = rect_horizontal_axis_sum r; height = rect_vertical_axis_sum r }

let line_sum l = l.start +. l.end_

(* ─────────────────  Mapping helpers (generic)  ───────────────── *)

let size_map s f = { width = f s.width; height = f s.height }
let size_map_width s f = { width = f s.width; height = s.height }
let size_map_height s f = { width = s.width; height = f s.height }

let size_zip_map s1 s2 f =
  { width = f s1.width s2.width; height = f s1.height s2.height }

let size_map2 s1 s2 ~f = size_zip_map s1 s2 f

let rect_map r f =
  { left = f r.left; right = f r.right; top = f r.top; bottom = f r.bottom }

let rect_zip_size r sz f =
  {
    left = f r.left sz.width;
    right = f r.right sz.width;
    top = f r.top sz.height;
    bottom = f r.bottom sz.height;
  }

let point_map p f = { x = f p.x; y = f p.y }
let line_map l f = { start = f l.start; end_ = f l.end_ }

(* ─────────────────  Float helpers  ───────────────── *)

let size_f32_max a b =
  {
    width = (if a.width >= b.width then a.width else b.width);
    height = (if a.height >= b.height then a.height else b.height);
  }

let size_f32_min a b =
  {
    width = (if a.width <= b.width then a.width else b.width);
    height = (if a.height <= b.height then a.height else b.height);
  }

let size_has_non_zero_area s = s.width > 0. && s.height > 0.

(* ─────────────────  Option helpers  ───────────────── *)

let size_option_new w h : float option size =
  { width = Some w; height = Some h }

let size_option_both_axis_defined s =
  match (s.width, s.height) with Some _, Some _ -> true | _ -> false

let size_option_unwrap_or s alt =
  {
    width = Option.value s.width ~default:alt.width;
    height = Option.value s.height ~default:alt.height;
  }

let size_option_or s alt =
  {
    width = (match s.width with None -> alt.width | some -> some);
    height = (match s.height with None -> alt.height | some -> some);
  }

let size_maybe_apply_aspect_ratio s ~aspect_ratio =
  match aspect_ratio with
  | None -> s
  | Some ratio -> (
      match (s.width, s.height) with
      | Some w, None -> { width = Some w; height = Some (w /. ratio) }
      | None, Some h -> { width = Some (h *. ratio); height = Some h }
      | _ -> s)

(* ─────────────────  Size module helpers  ───────────────── *)

module Size = struct
  let zero = size_zero
  let none = size_none

  (* Maybe clamp a single value between min and max bounds *)
  let maybe_clamp_value value min_val max_val =
    match (min_val, max_val) with
    | Some min_v, Some max_v -> Float.max min_v (Float.min value max_v)
    | Some min_v, None -> Float.max min_v value
    | None, Some max_v -> Float.min value max_v
    | None, None -> value

  (* Maybe clamp a size between min and max bounds *)
  let maybe_clamp s min_size max_size =
    let clamp_dim value min_val max_val =
      match (value, min_val, max_val) with
      | Some v, Some min_v, Some max_v ->
          Some (Float.max min_v (Float.min v max_v))
      | Some v, Some min_v, None -> Some (Float.max min_v v)
      | Some v, None, Some max_v -> Some (Float.min v max_v)
      | _ -> value
    in
    {
      width = clamp_dim s.width min_size.width max_size.width;
      height = clamp_dim s.height min_size.height max_size.height;
    }

  (* Add two sizes together *)
  let add s1 s2 = size_add s1 s2

  (* Map function over size *)
  let map s ~f = size_map s f

  (* Maybe apply max constraint *)
  let maybe_max s1 s2 =
    {
      width =
        (match (s1.width, s2.width) with
        | Some w1, Some w2 -> Some (Float.max w1 w2)
        | Some w, None | None, Some w -> Some w
        | None, None -> None);
      height =
        (match (s1.height, s2.height) with
        | Some h1, Some h2 -> Some (Float.max h1 h2)
        | Some h, None | None, Some h -> Some h
        | None, None -> None);
    }

  (* Or operation - take first if Some, otherwise second *)
  let or_ s1 s2 = size_option_or s1 s2

  (* Maybe resolve size - resolves dimension types to concrete values *)
  let maybe_resolve s _parent_size _resolve_calc =
    (* Since s is already a Dimension.dimension size from style.size(),
       we need to use the resolve functions from the Style module *)
    s
  (* This will be resolved at the call site using Style.resolve functions *)

  (* Maybe add size *)
  let maybe_add s addition =
    match (s.width, s.height) with
    | Some w, Some h ->
        {
          width = Some (w +. addition.width);
          height = Some (h +. addition.height);
        }
    | Some w, None -> { width = Some (w +. addition.width); height = None }
    | None, Some h -> { width = None; height = Some (h +. addition.height) }
    | None, None -> s

  (* Get width or height based on axis *)
  let get s axis = match axis with Inline -> s.width | Block -> s.height

  (* Set width or height based on axis *)
  let set s axis value =
    match axis with
    | Inline -> { s with width = value }
    | Block -> { s with height = value }

  (* Apply aspect ratio to size *)
  let maybe_apply_aspect_ratio s aspect_ratio =
    size_maybe_apply_aspect_ratio s ~aspect_ratio

  (* Map2 function - alias for size_map2 *)
  let map2 s1 s2 ~f = size_map2 s1 s2 ~f
end

(* Add size_get_abs to the global scope so it's accessible by tree_intf *)
let size_get_abs = size_get_abs

(* ─────────────────  Rect module with resolve functions  ───────────────── *)

module Rect = struct
  (* Basic functions *)
  let zero = rect_zero
  let add = rect_add
  let ( + ) = rect_add (* Infix operator *)
  let sum_axes = rect_sum_axes
  let horizontal_axis_sum = rect_horizontal_axis_sum
  let vertical_axis_sum = rect_vertical_axis_sum

  (* sum_axis function for use in leaf.ml *)
  let sum_axis r axis = rect_grid_axis_sum r axis
end
