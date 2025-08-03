(** grid_helpers.ml
    ---------------------------------------------------------------------------
    Helper functions for Grid sizing functions
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

open Style
open Style.Grid

module MinSizing = struct
  let zero : min_track_sizing_function = Length 0.0

  let from_length_percentage (lp : Length_percentage.t) :
      min_track_sizing_function =
    match lp with
    | Length_percentage.Length px -> Length px
    | Length_percentage.Percent pct -> Percent pct

  let is_auto (min_fn : min_track_sizing_function) =
    match min_fn with Auto -> true | _ -> false

  let is_intrinsic (min_fn : min_track_sizing_function) =
    match min_fn with Min_content | Max_content -> true | _ -> false

  let is_min_content (min_fn : min_track_sizing_function) =
    match min_fn with Min_content -> true | _ -> false

  let is_max_content (min_fn : min_track_sizing_function) =
    match min_fn with Max_content -> true | _ -> false

  let is_min_or_max_content (min_fn : min_track_sizing_function) =
    match min_fn with Min_content | Max_content -> true | _ -> false

  let uses_percentage (min_fn : min_track_sizing_function) =
    match min_fn with Percent _ -> true | _ -> false

  (** Returns fixed point values directly. Attempts to resolve percentage values
      against the passed available_space and returns if this results in a
      concrete value (which it will if the available_space is `Some`). Otherwise
      returns None. *)
  let definite_value (min_fn : min_track_sizing_function) parent_size _calc =
    match min_fn with
    | Length px -> Some px
    | Percent pct -> Option.map (fun size -> pct *. size) parent_size
    | _ -> None
end

module MaxSizing = struct
  let zero : max_track_sizing_function = Length 0.0

  let from_length_percentage (lp : Length_percentage.t) :
      max_track_sizing_function =
    match lp with
    | Length_percentage.Length px -> Length px
    | Length_percentage.Percent pct -> Percent pct

  let is_fr (max_fn : max_track_sizing_function) =
    match max_fn with Fr _ -> true | _ -> false

  let is_intrinsic (max_fn : max_track_sizing_function) =
    match max_fn with Min_content | Max_content -> true | _ -> false

  let is_auto (max_fn : max_track_sizing_function) =
    match max_fn with Auto -> true | _ -> false

  let is_min_content (max_fn : max_track_sizing_function) =
    match max_fn with Min_content -> true | _ -> false

  let is_max_content (max_fn : max_track_sizing_function) =
    match max_fn with Max_content -> true | _ -> false

  let is_max_content_alike (max_fn : max_track_sizing_function) =
    match max_fn with Max_content | Auto -> true | _ -> false

  let is_max_or_fit_content (max_fn : max_track_sizing_function) =
    match max_fn with Max_content | Fit_content _ -> true | _ -> false

  let is_fit_content (max_fn : max_track_sizing_function) =
    match max_fn with Fit_content _ -> true | _ -> false

  let uses_percentage (max_fn : max_track_sizing_function) =
    match max_fn with
    | Percent _ -> true
    | Fit_content (Style.Length_percentage.Percent _) -> true
    | _ -> false

  (** Get definite value from max sizing function (excludes Fit_content) *)
  let definite_value max_fn parent_size _calc =
    match max_fn with
    | Length px -> Some px
    | Percent pct -> Option.map (fun size -> size *. pct) parent_size
    | _ -> None

  (** Get definite limit from max sizing function (includes Fit_content) *)
  let definite_limit (max_fn : max_track_sizing_function) parent_size _calc =
    match max_fn with
    | Length px -> Some px
    | Percent pct -> Option.map (fun size -> size *. pct) parent_size
    | Fit_content lp -> (
        match lp with
        | Style.Length_percentage.Length px -> Some px
        | Style.Length_percentage.Percent pct ->
            Option.map (fun size -> size *. pct) parent_size)
    | _ -> None

  (** Returns true if the max track sizing function has a definite value *)
  let has_definite_value (max_fn : max_track_sizing_function) parent_size =
    match max_fn with
    | Length _ -> true
    | Percent _ -> Option.is_some parent_size
    | _ -> false
end

module Overflow = struct
  open Style

  (** Returns true if the overflow mode would cause item to act as a scroll
      container (and thus potentially have its size determined by the size of
      its contents) or else false for overflow modes that allow their contents
      to spill (`Overflow::Visible`). *)
  let is_scroll_container (overflow : overflow) =
    match overflow with Visible | Clip -> false | Hidden | Scroll -> true

  (** Returns `Some(0.0)` if the overflow mode would cause the automatic minimum
      size of a Flexbox or CSS Grid item to be `0`. Else returns None. *)
  let maybe_into_automatic_min_size (overflow : overflow) : float option =
    match overflow with Visible | Clip -> None | Hidden | Scroll -> Some 0.0
end
