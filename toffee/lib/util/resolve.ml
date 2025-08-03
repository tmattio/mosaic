(* We need style types but avoid opening to prevent name clashes *)

(* In Rust, the calc function takes a pointer and a context dimension.
   In OCaml, we can represent this as a regular function that takes
   an abstract calc_id and a dimension *)
type calc_fn = int -> float -> float

let maybe_resolve_length_percentage self context _calc =
  match self with
  | Style.Length_percentage.Length value -> Some value
  | Style.Length_percentage.Percent value ->
      Option.map (fun dim -> dim *. value) context

let maybe_resolve_length_percentage_auto self context _calc =
  match self with
  | Style.Length_percentage_auto.Auto -> None
  | Style.Length_percentage_auto.Length value -> Some value
  | Style.Length_percentage_auto.Percent value ->
      Option.map (fun dim -> dim *. value) context

let maybe_resolve_dimension self context _calc =
  match self with
  | Style.Dimension.Auto -> None
  | Style.Dimension.Length value -> Some value
  | Style.Dimension.Percent value ->
      Option.map (fun dim -> dim *. value) context

let maybe_resolve_dimension_with_float self context calc =
  maybe_resolve_dimension self (Some context) calc

let maybe_resolve_size resolve_fn self context calc =
  let open Geometry in
  {
    width = resolve_fn self.width context.width calc;
    height = resolve_fn self.height context.height calc;
  }

let resolve_or_zero_length_percentage self context calc =
  match maybe_resolve_length_percentage self context calc with
  | Some value -> value
  | None -> 0.0

let resolve_or_zero_length_percentage_auto self context calc =
  match maybe_resolve_length_percentage_auto self context calc with
  | Some value -> value
  | None -> 0.0

let resolve_or_zero_dimension self context calc =
  match maybe_resolve_dimension self context calc with
  | Some value -> value
  | None -> 0.0

let resolve_or_zero_size resolve_fn self context calc =
  let open Geometry in
  {
    width = resolve_fn self.width context.width calc;
    height = resolve_fn self.height context.height calc;
  }

let resolve_or_zero_rect_with_size resolve_fn self context calc =
  let open Geometry in
  {
    left = resolve_fn self.left context.width calc;
    right = resolve_fn self.right context.width calc;
    top = resolve_fn self.top context.height calc;
    bottom = resolve_fn self.bottom context.height calc;
  }

let resolve_or_zero_rect_with_option resolve_fn self context calc =
  let open Geometry in
  {
    left = resolve_fn self.left context calc;
    right = resolve_fn self.right context calc;
    top = resolve_fn self.top context calc;
    bottom = resolve_fn self.bottom context calc;
  }

(** Apply aspect ratio to a size *)
let maybe_apply_aspect_ratio aspect_ratio size =
  let open Geometry in
  match aspect_ratio with
  | None -> size
  | Some ratio -> (
      match (size.width, size.height) with
      | Some w, None -> { width = Some w; height = Some (w /. ratio) }
      | None, Some h -> { width = Some (h *. ratio); height = Some h }
      | _ -> size)
