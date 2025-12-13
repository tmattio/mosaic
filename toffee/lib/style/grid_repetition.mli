(** Grid track repetition specification.

    Represents a [repeat()] clause in CSS Grid [grid-template-rows] and
    [grid-template-columns] definitions. A repetition specifies a count (either
    a fixed integer or auto-fill/auto-fit), a list of track sizing functions to
    repeat, and associated line names for those tracks.

    See
    {{:https://www.w3.org/TR/css-grid-1/#auto-repeat}Auto-repeat specification}
    for details on auto-repeated track definitions. *)

type t = {
  count : Repetition_count.t;
      (** The number of times the tracks are repeated. May be a fixed count or
          an auto-repeat mode (auto-fill or auto-fit). *)
  tracks : Track_sizing_function.t list;
      (** The list of track sizing functions to repeat. *)
  line_names : string list list;
      (** Line names associated with each track boundary. The outer list
          corresponds to the track boundaries (length is
          [List.length tracks + 1]), and each inner list contains the names for
          that boundary. *)
}
(** A grid track repetition specification. *)

val make :
  count:Repetition_count.t ->
  tracks:Track_sizing_function.t list ->
  line_names:string list list ->
  t
(** [make ~count ~tracks ~line_names] creates a grid track repetition
    specification.

    {b Precondition}: [line_names] should have [List.length tracks + 1] elements
    to cover all track boundaries. This is not validated; mismatched lengths may
    cause incorrect behavior during grid layout. *)

val count : t -> Repetition_count.t
(** [count t] returns the repetition count. *)

val tracks : t -> Track_sizing_function.t list
(** [tracks t] returns the list of track sizing functions. *)

val line_names : t -> string list list
(** [line_names t] returns the line names for track boundaries. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality between two repetitions. *)

val compare : t -> t -> int
(** [compare a b] provides a total ordering over repetitions. *)

val is_auto_repetition : t -> bool
(** [is_auto_repetition t] returns [true] if the repetition uses auto-fill or
    auto-fit, [false] for fixed counts. *)

val track_count : t -> int
(** [track_count t] returns the number of tracks in the repetition (equivalent
    to [List.length (tracks t)]). *)
