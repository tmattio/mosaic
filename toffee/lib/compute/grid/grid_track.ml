(* Contains GridTrack used to represent a single grid track (row/column) during layout *)

open Style
open Style.Grid

(* Whether a GridTrack represents an actual track or a gutter *)
type grid_track_kind =
  | Track (* Track is an actual track *)
  | Gutter (* Track is a gutter (aka grid line) (aka gap) *)

(* Internal sizing information for a single grid track (row/column)
    Gutters between tracks are sized similarly to actual tracks, so they
    are also represented by this struct *)
type t = {
  kind : grid_track_kind;
      (* Whether the track is a full track, a gutter, or a placeholder that has not yet been initialised *)
  is_collapsed : bool;
      (* Whether the track is a collapsed track/gutter. Collapsed tracks are effectively treated as if
      they don't exist for the purposes of grid sizing. Gutters between collapsed tracks are also collapsed *)
  track_sizing_function : track_sizing_function;
      (* The track sizing function (contains both min and max) *)
  mutable offset : float;
      (* The distance of the start of the track from the start of the grid container *)
  mutable base_size : float;
      (* The size (width/height as applicable) of the track *)
  mutable growth_limit : float;
      (* A temporary scratch value when sizing tracks. Note: can be infinity *)
  mutable content_alignment_adjustment : float;
      (* A temporary scratch value when sizing tracks. Is used as an additional amount to add to the
      estimate for the available space in the opposite axis when content sizing items *)
  mutable item_incurred_increase : float;
      (* A temporary scratch value when "distributing space" to avoid clobbering planned increase variable *)
  mutable base_size_planned_increase : float;
      (* A temporary scratch value when "distributing space" to avoid clobbering the main variable *)
  mutable growth_limit_planned_increase : float;
      (* A temporary scratch value when "distributing space" to avoid clobbering the main variable *)
  mutable infinitely_growable : bool;
      (* A temporary scratch value when "distributing space"
      See: https://www.w3.org/TR/css3-grid-layout/#infinitely-growable *)
}

(* GridTrack constructor with all configuration parameters for the other constructors exposed *)
let new_with_kind kind track_sizing_function =
  {
    kind;
    is_collapsed = false;
    track_sizing_function;
    offset = 0.0;
    base_size = 0.0;
    growth_limit = 0.0;
    content_alignment_adjustment = 0.0;
    item_incurred_increase = 0.0;
    base_size_planned_increase = 0.0;
    growth_limit_planned_increase = 0.0;
    infinitely_growable = false;
  }

(* Create new GridTrack representing an actual track (not a gutter) *)
let create track_sizing_function = new_with_kind Track track_sizing_function

(* Create a new GridTrack representing a gutter *)
let gutter (size : length_percentage) =
  (* For gutters, both min and max track sizing functions are the same length_percentage *)
  let sizing_fn =
    Style.Grid.Track_sizing_function.from_length_percentage size
  in
  new_with_kind Gutter sizing_fn

(* Mark a GridTrack as collapsed. Also sets both of the track's sizing functions
    to fixed zero-sized sizing functions *)
let collapse t =
  {
    t with
    is_collapsed = true;
    track_sizing_function = Track_sizing_function.zero;
  }

(* Returns true if the track is flexible (has a Flex MaxTrackSizingFunction), else false *)
let is_flexible t = Track_sizing_function.Max.is_fr t.track_sizing_function

(* Returns true if the track uses percentage sizing *)
let uses_percentage t =
  Track_sizing_function.Min.uses_percentage t.track_sizing_function
  || Track_sizing_function.Max.uses_percentage t.track_sizing_function

(* Returns true if the track has an intrinsic min and or max sizing function *)
let has_intrinsic_sizing_function t =
  Track_sizing_function.Min.is_intrinsic t.track_sizing_function
  || Track_sizing_function.Max.is_intrinsic t.track_sizing_function

(* Returns the fit-content limit for the track *)
let fit_content_limit t axis_available_grid_space =
  match
    Track_sizing_function.Max.definite_limit t.track_sizing_function
      axis_available_grid_space
  with
  | Some limit
    when Track_sizing_function.Max.is_fit_content t.track_sizing_function ->
      limit
  | _ -> Float.infinity

(* Returns the fit-content limited growth limit *)
let fit_content_limited_growth_limit t axis_available_grid_space =
  Float.min t.growth_limit (fit_content_limit t axis_available_grid_space)

(* Returns the track's flex factor if it is a flex track, else 0 *)
let flex_factor t =
  if Track_sizing_function.Max.is_fr t.track_sizing_function then
    Track_sizing_function.Max.fr_value t.track_sizing_function
  else 0.0
