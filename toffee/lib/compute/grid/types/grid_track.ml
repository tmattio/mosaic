(** grid_track.ml
    ---------------------------------------------------------------------------
    Contains GridTrack used to represent a single grid track (row/column) during
    layout
    ---------------------------------------------------------------------------
    SPDX-License-Identifier: MIT OR Apache-2.0
    ---------------------------------------------------------------------------
*)

(* open Style *)
open Grid_helpers

(** Whether a GridTrack represents an actual track or a gutter. *)
type grid_track_kind =
  | Track  (** Track is an actual track *)
  | Gutter  (** Track is a gutter (aka grid line) (aka gap) *)

type t = {
  kind : grid_track_kind;
      (** Whether the track is a full track, a gutter, or a placeholder that has
          not yet been initialised *)
  mutable is_collapsed : bool;
      (** Whether the track is a collapsed track/gutter. Collapsed tracks are
          effectively treated as if they don't exist for the purposes of grid
          sizing. Gutters between collapsed tracks are also collapsed. *)
  mutable min_track_sizing_function : Style.Grid.min_track_sizing_function;
      (** The minimum track sizing function of the track *)
  mutable max_track_sizing_function : Style.Grid.max_track_sizing_function;
      (** The maximum track sizing function of the track *)
  mutable offset : float;
      (** The distance of the start of the track from the start of the grid
          container *)
  mutable base_size : float;
      (** The size (width/height as applicable) of the track *)
  mutable growth_limit : float;
      (** A temporary scratch value when sizing tracks. Note: can be infinity *)
  mutable content_alignment_adjustment : float;
      (** A temporary scratch value when sizing tracks. Is used as an additional
          amount to add to the estimate for the available space in the opposite
          axis when content sizing items *)
  mutable item_incurred_increase : float;
      (** A temporary scratch value when "distributing space" to avoid
          clobbering planned increase variable *)
  mutable base_size_planned_increase : float;
      (** A temporary scratch value when "distributing space" to avoid
          clobbering the main variable *)
  mutable growth_limit_planned_increase : float;
      (** A temporary scratch value when "distributing space" to avoid
          clobbering the main variable *)
  mutable infinitely_growable : bool;
      (** A temporary scratch value when "distributing space" See:
          https://www.w3.org/TR/css3-grid-layout/#infinitely-growable *)
}
(** Internal sizing information for a single grid track (row/column) Gutters
    between tracks are sized similarly to actual tracks, so they are also
    represented by this struct *)

(** GridTrack constructor with all configuration parameters for the other
    constructors exposed *)
let new_with_kind kind min_track_sizing_function max_track_sizing_function =
  {
    kind;
    is_collapsed = false;
    min_track_sizing_function;
    max_track_sizing_function;
    offset = 0.0;
    base_size = 0.0;
    growth_limit = 0.0;
    content_alignment_adjustment = 0.0;
    item_incurred_increase = 0.0;
    base_size_planned_increase = 0.0;
    growth_limit_planned_increase = 0.0;
    infinitely_growable = false;
  }

(** Create new GridTrack representing an actual track (not a gutter) *)
let create min_track_sizing_function max_track_sizing_function =
  new_with_kind Track min_track_sizing_function max_track_sizing_function

(** Create a new GridTrack representing a gutter *)
let gutter size =
  let min_sizing_fn = MinSizing.from_length_percentage size in
  let max_sizing_fn = MaxSizing.from_length_percentage size in
  new_with_kind Gutter min_sizing_fn max_sizing_fn

(** Mark a GridTrack as collapsed. Also sets both of the track's sizing
    functions to fixed zero-sized sizing functions. *)
let collapse t =
  t.is_collapsed <- true;
  t.min_track_sizing_function <- MinSizing.zero;
  t.max_track_sizing_function <- MaxSizing.zero

(** Returns true if the track is flexible (has a Flex MaxTrackSizingFunction),
    else false. *)
let is_flexible t = MaxSizing.is_fr t.max_track_sizing_function

(** Returns true if the track uses percentage sizing *)
let uses_percentage t =
  MinSizing.uses_percentage t.min_track_sizing_function
  || MaxSizing.uses_percentage t.max_track_sizing_function

(** Returns true if the track has an intrinsic min and or max sizing function *)
let has_intrinsic_sizing_function t =
  MinSizing.is_intrinsic t.min_track_sizing_function
  || MaxSizing.is_intrinsic t.max_track_sizing_function

(** Returns the fit-content limit for the track *)
let fit_content_limit t axis_available_grid_space =
  match t.max_track_sizing_function with
  | Style.Grid.Fit_content lp -> (
      match lp with
      | Style.Length_percentage.Length px -> px
      | Style.Length_percentage.Percent pct -> (
          match axis_available_grid_space with
          | Some space -> space *. pct
          | None -> Float.infinity))
  | _ -> Float.infinity

(** Returns the track's growth limit clamped by the fit-content limit *)
let fit_content_limited_growth_limit t axis_available_grid_space =
  min t.growth_limit (fit_content_limit t axis_available_grid_space)

(** Returns the track's flex factor if it is a flex track, else 0. *)
let flex_factor t =
  match t.max_track_sizing_function with Style.Grid.Fr fr -> fr | _ -> 0.0
