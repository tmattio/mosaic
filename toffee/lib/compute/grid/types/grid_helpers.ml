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

  let uses_percentage (min_fn : min_track_sizing_function) =
    match min_fn with Percent _ -> true | _ -> false
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
end
